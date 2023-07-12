{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module JsonSchemaNew where

import Data.Aeson (FromJSON (parseJSON), withObject, withText, (.!=), (.:), (.:?))
import Data.Functor ((<$>))
import qualified Data.Map as Map
import Data.Maybe (Maybe (..), fromMaybe)
import Data.Text (Text, unpack)
import Data.Traversable (Traversable (..))
import Debug.Trace (traceShowId)
import Text.Show (Show)
import Prelude (Applicative (pure, (<*>)), Eq ((==)), MonadFail (fail), Monoid (mempty), Semigroup ((<>)), ($))

data SchemaTypeEnum
  = SchemaTypeObject
  | SchemaTypeInteger
  | SchemaTypeString
  | SchemaTypeNumber
  | SchemaTypeBoolean
  | SchemaTypeArray
  deriving (Show)

instance FromJSON SchemaTypeEnum where
  parseJSON = withText "Type" $ \v ->
    if v == "object"
      then pure SchemaTypeObject
      else
        if v == "integer"
          then pure SchemaTypeInteger
          else
            if v == "number"
              then pure SchemaTypeNumber
              else
                if v == "boolean"
                  then pure SchemaTypeBoolean
                  else
                    if v == "array"
                      then pure SchemaTypeArray
                      else
                        if v == "string"
                          then pure SchemaTypeString
                          else fail $ "unknown schema type " <> unpack v

data RefSchemaObject = RefSchemaObject
  { refObjectTitle :: Text,
    refType :: SchemaTypeEnum,
    refProperties :: Map.Map Text RefOrSchema,
    refRequired :: [Text],
    refDefinitions :: (Text, RefSchemaType)
  }
  deriving (Show)

data SchemaObject = SchemaObject
  { objectTitle :: Text,
    type_ :: SchemaTypeEnum,
    properties :: Map.Map Text SchemaType,
    required :: [Text],
    definitions :: Map.Map Text SchemaType
  }
  deriving (Show)

data SchemaEnum = SchemaEnum
  { enumTitle :: Text,
    enum :: [Text]
  }
  deriving (Show)

data RefSchemaType
  = RefUnion [RefOrSchema]
  | RefObject RefSchemaObject
  | RefNumber
  | RefInteger
  | RefString
  | RefEnum SchemaEnum
  | RefArray RefOrSchema
  | RefBoolean
  deriving (Show)

data SchemaType
  = Union [SchemaType]
  | Object SchemaObject
  | Number
  | Integer
  | String
  | Enum SchemaEnum
  | Array SchemaType
  | Boolean
  deriving (Show)

data RefOrSchema = Ref Text | Schema RefSchemaType deriving (Show)

instance FromJSON RefOrSchema where
  parseJSON orig =
    withObject
      "Object"
      ( \v -> do
          ref <- v .:? "$ref"
          case ref of
            Nothing -> Schema <$> parseJSON orig
            Just ref' -> pure (Ref ref')
      )
      orig

instance FromJSON SchemaEnum where
  parseJSON = withObject "Enum" $ \v -> SchemaEnum <$> (v .: "title") <*> (v .: "enum")

instance FromJSON RefSchemaObject where
  parseJSON = withObject "Object" $ \v ->
    RefSchemaObject
      <$> (v .: "title")
      <*> (v .: "type")
      <*> (v .: "properties")
      <*> (v .:? "required" .!= mempty)
      <*> (v .:? "definitions" .!= mempty)

instance FromJSON RefSchemaType where
  parseJSON orig =
    withObject
      "Object"
      ( \v -> do
          anyOf <- v .:? "anyOf"
          case anyOf of
            Just anyOf' -> RefUnion <$> traverse parseJSON anyOf'
            Nothing -> do
              jsonEnum :: Maybe [Text] <- v .:? "enum"
              case jsonEnum of
                Just _ -> RefEnum <$> parseJSON orig
                Nothing -> do
                  jsonType_ <- v .: "type"
                  case jsonType_ of
                    SchemaTypeString -> pure RefString
                    SchemaTypeNumber -> pure RefNumber
                    SchemaTypeBoolean -> pure RefBoolean
                    SchemaTypeInteger -> pure RefInteger
                    SchemaTypeObject -> RefObject <$> parseJSON orig
                    SchemaTypeArray -> RefArray <$> (v .: "items")
      )
      orig

{-
 Plan for next time:

 - Resolve refs in refDefinitions, using just definitions inside there
 - Write a funtion "resolveRefs" (see below for skeleton) that can resolve RefSchemaType to Maybe SchemaType but which assumes the root type is an object (and takes the definitions from there).
 - Write a second function "resolveRefs'" (also already below) that takes resolved definitions and a root object and resolves the whole object recursively
 -}
resolveRefs :: RefSchemaType -> Maybe SchemaType
resolveRefs (RefObject o) =
  resolveRefs' (refDefinitions o) = -- but shit we have to resolve internal definitions in refDefinitions also

resolveRefs' :: RefSchemaType -> Maybe SchemaType
resolveRefs' RefNumber = Just Number
resolveRefs' RefInteger = Just Integer
resolveRefs' RefString = Just String
resolveRefs' RefBoolean = Just Boolean
resolveRefs' (RefEnum e) = Just (Enum e)
resolveRefs' (RefObject o) =
  let resolveDefinition :: Map Text SchemaType -> (Text, RefSchemaType) -> Maybe (Map Text SchemaType)
      
--   Object (refObjectTitle o) (refType o) (refProperties o) (refRequired o) (refDefinitions o)
