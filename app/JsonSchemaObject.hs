{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module JsonSchemaObject where

import Control.Monad (foldM)
import Data.Aeson (FromJSON)
import qualified Data.Map as Map
import Data.Text (Text, unpack)
import GHC.Generics (Generic)
import qualified JsonSchemaObjectRef as Ref
import JsonSchemaTypeEnum

type ObjectMap = Map.Map Text JsonSchemaObject

data JsonSchemaObject = JsonSchemaObject
  { title :: Maybe Text,
    description :: Maybe Text,
    type_ :: Maybe SchemaTypeEnum,
    properties :: Maybe ObjectMap,
    required :: Maybe [Text],
    enum :: Maybe [Text],
    definitions :: ObjectMap,
    anyOf :: Maybe [JsonSchemaObject]
  }
  deriving (Show, Generic)

instance FromJSON JsonSchemaObject

fromRef :: ObjectMap -> Ref.JsonSchemaObjectRef -> Either Text JsonSchemaObject
fromRef previousDefinitions root =
  let resolveObjectRef :: ObjectMap -> Ref.JsonSchemaObjectRef -> Either Text JsonSchemaObject
      resolveObjectRef defs v = case Ref.definitions v of
        Just _ -> Left "element has sub-definitions, that's not supported"
        Nothing ->
          case Ref.ref v of
            Just ref ->
              case Map.lookup ("#/definitions/" <> ref) defs of
                Nothing -> Left ("element has ref to " <> ref <> " which we haven't found")
                Just resolvedRef -> Right resolvedRef
            Nothing ->
              fromRef (previousDefinitions `Map.union` defs) v

      resolveDefinition :: ObjectMap -> (Text, Ref.JsonSchemaObjectRef) -> Either Text ObjectMap
      resolveDefinition oldDefinitions (k, v) =
        case resolveObjectRef oldDefinitions v of
          Left e -> Left e
          Right v' -> Right (Map.insert k v' oldDefinitions)

      resolvedDefinitions :: Maybe [(Text, Ref.JsonSchemaObjectRef)] -> Either Text ObjectMap
      resolvedDefinitions d = case d of
        Nothing -> Right mempty
        Just defs -> foldM resolveDefinition mempty defs

      resolvedProperties :: ObjectMap -> Maybe Ref.ObjectRefMap -> Either Text (Maybe ObjectMap)
      resolvedProperties defs props' = case props' of
        Nothing -> Right Nothing
        Just props ->
          let resolveWithMap :: ObjectMap -> (Text, Ref.JsonSchemaObjectRef) -> Either Text ObjectMap
              resolveWithMap prevMap (newKey, newElement) = (\resolvedEl -> Map.insert newKey resolvedEl prevMap) <$> resolveObjectRef defs newElement
           in Just <$> foldM resolveWithMap mempty (Map.toList props)

      resolvedAnyOfs :: ObjectMap -> Maybe [Ref.JsonSchemaObjectRef] -> Either Text (Maybe [JsonSchemaObject])
      resolvedAnyOfs defs anyOfs' = case anyOfs' of
        Nothing -> Right Nothing
        Just anyOfs ->
          let resolveWithList :: [JsonSchemaObject] -> Ref.JsonSchemaObjectRef -> Either Text [JsonSchemaObject]
              resolveWithList prevList newElement = (: prevList) <$> resolveObjectRef defs newElement
           in Just <$> foldM resolveWithList mempty anyOfs
   in do
        defs <- resolvedDefinitions (Ref.definitions root)
        props <- resolvedProperties defs (Ref.properties root)
        anyOfs <- resolvedAnyOfs defs (Ref.anyOf root)
        pure
          ( JsonSchemaObject
              (Ref.title root)
              (Ref.description root)
              (Ref.type_ root)
              props
              (Ref.required root)
              (Ref.enum root)
              defs
              anyOfs
          )
