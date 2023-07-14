{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module JsonSchemaObject where

import Control.Applicative (Applicative (..))
import Control.Monad (foldM)
import Data.Aeson (FromJSON)
import Data.Bool (not)
import Data.Either (Either (Left, Right))
import Data.Foldable (Foldable)
import Data.Functor ((<$>))
import qualified Data.Map as Map
import Data.Maybe (Maybe (Just, Nothing), fromMaybe)
import Data.Monoid (Monoid (..))
import Data.Semigroup ((<>))
import Data.Text (Text, drop, isPrefixOf, length)
import Data.Traversable (Traversable (traverse))
import GHC.Generics (Generic)
import qualified JsonSchemaObjectRef as Ref
import JsonSchemaTypeEnum
import Text.Show (Show)
import Prelude ()

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

expectedRefPrefix :: Text
expectedRefPrefix = "#/definitions/"

resolveMap :: (Foldable t) => ObjectMap -> t (Text, Ref.JsonSchemaObjectRef) -> Either Text ObjectMap
resolveMap rootDefs =
  foldM
    (\prevMap (newKey, newElement) -> (\resolvedEl -> Map.insert newKey resolvedEl prevMap) <$> resolveSingleDefinition rootDefs newElement)
    mempty

resolveSingleDefinition :: ObjectMap -> Ref.JsonSchemaObjectRef -> Either Text JsonSchemaObject
resolveSingleDefinition rootDefs root =
  case Ref.ref root of
    -- The currently looked-at node is a ref itself (shouldn't apply to the root node)
    Just ref' ->
      if not (expectedRefPrefix `isPrefixOf` ref')
        then Left ("reference doesn't start with \"" <> expectedRefPrefix <> "\": " <> ref')
        else case Map.lookup (drop (length expectedRefPrefix) ref') rootDefs of
          Nothing -> Left ("looking up " <> ref' <> ": not found")
          Just result -> Right result
    -- The currently looked-at node is not a ref
    Nothing -> do
      defs <- case Ref.definitions root of
        Nothing -> Right mempty
        Just defs' -> resolveMap rootDefs (Map.toList defs')

      -- Next, look at the properties of the object. They also need to be resolved because they might contain refs.
      props <- case Ref.properties root of
        Nothing -> Right Nothing
        Just props' ->
          Just <$> resolveMap rootDefs (Map.toList props')

      -- Same game for the anyOf alternatives
      anyOfs <- case Ref.anyOf root of
        Nothing -> Right Nothing
        Just anyOfs' ->
          Just
            <$> foldM
              (\prevList newElement -> (: prevList) <$> resolveSingleDefinition rootDefs newElement)
              mempty
              anyOfs'
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

-- Map k v -> (v -> m b) -> Map k (m b) -> m (Map k b)
resolveRootDefinitions :: Ref.ObjectRefMap -> Either Text ObjectMap
resolveRootDefinitions rootDefs =
  let resolveMap' :: (Foldable t) => t (Text, Ref.JsonSchemaObjectRef) -> Either Text ObjectMap
      resolveMap' =
        foldM
          (\prevMap (newKey, newElement) -> (\resolvedEl -> Map.insert newKey resolvedEl prevMap) <$> resolveRootDefinition newElement)
          mempty

      resolveRootDefinition :: Ref.JsonSchemaObjectRef -> Either Text JsonSchemaObject
      resolveRootDefinition root = case Ref.ref root of
        Just ref' ->
          if not (expectedRefPrefix `isPrefixOf` ref')
            then Left ("reference doesn't start with \"" <> expectedRefPrefix <> "\": " <> ref')
            else case Map.lookup (drop (length expectedRefPrefix) ref') rootDefs of
              Nothing -> Left ("looking up " <> ref' <> ": not found")
              Just result -> resolveRootDefinition result
        Nothing -> do
          -- Next, look at the properties of the object. They also need to be resolved because they might contain refs.
          props <- case Ref.properties root of
            Nothing -> Right Nothing
            Just props' ->
              Just <$> resolveMap' (Map.toList props')

          -- Same game for the anyOf alternatives
          anyOfs <- case Ref.anyOf root of
            Nothing -> Right Nothing
            Just anyOfs' ->
              Just
                <$> foldM
                  (\prevList newElement -> (: prevList) <$> resolveRootDefinition newElement)
                  mempty
                  anyOfs'
          pure
            ( JsonSchemaObject
                (Ref.title root)
                (Ref.description root)
                (Ref.type_ root)
                props
                (Ref.required root)
                (Ref.enum root)
                mempty
                anyOfs
            )
   in traverse resolveRootDefinition rootDefs

-- Here it starts, and we assume we have no prior resolved definitions in our definitions map (see the mempty)
fromRef :: Ref.JsonSchemaObjectRef -> Either Text JsonSchemaObject
fromRef root =
  case Ref.ref root of
    -- The currently looked-at node is a ref itself (shouldn't apply to the root node)
    Just ref' -> Left ("root element is a ref to " <> ref' <> " - that's not supported!")
    Nothing -> do
      rootDefs <- resolveRootDefinitions (fromMaybe mempty (Ref.definitions root))
      resolveSingleDefinition rootDefs root
