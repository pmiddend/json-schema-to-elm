{-# LANGUAGE OverloadedStrings #-}

module JsonSchemaProcessed where

import Control.Applicative (Applicative (pure))
import Data.Either (Either)
import qualified Data.Map as Map
import Data.Maybe (Maybe (Just, Nothing), fromMaybe)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Traversable (Traversable (traverse))
import JsonSchemaObject (JsonSchemaObject (additionalProperties, anyOf, definitions, enum, items, properties, required, title, type_))
import JsonSchemaTypeEnum (SchemaTypeEnum (..))
import Text.Show (Show)
import Utils (ErrorMessage, makeError, pShowStrict)
import Prelude ()

data JsonSchemaProcessed
  = JsonSchemaProcessedArray {arrayItems :: JsonSchemaProcessed}
  | JsonSchemaProcessedEnum {enumTitle :: Text, enumItems :: [Text]}
  | JsonSchemaProcessedObject
      { objectTitle :: Text,
        objectProperties :: Map.Map Text JsonSchemaProcessed,
        objectDefinitions :: Map.Map Text JsonSchemaProcessed,
        objectRequired :: [Text]
      }
  | JsonSchemaProcessedDict
      { dictProperties :: JsonSchemaProcessed
      }
  | JsonSchemaProcessedUnion {unionTitle :: Text, unionItems :: [JsonSchemaProcessed]}
  | JsonSchemaProcessedInt
  | JsonSchemaProcessedString {stringTitle :: Text}
  | JsonSchemaProcessedNumber {numberTitle :: Text}
  | JsonSchemaProcessedBoolean {booleanTitle :: Text}
  deriving (Show)

fromObject :: JsonSchemaObject -> Either ErrorMessage JsonSchemaProcessed
fromObject j = case type_ j of
  Just SchemaTypeInteger -> pure JsonSchemaProcessedInt
  Just SchemaTypeString -> case title j of
    Nothing -> makeError ("string without a title: " <> pShowStrict j)
    Just title' -> pure (JsonSchemaProcessedString title')
  Just SchemaTypeNumber -> case title j of
    Nothing -> makeError ("number without a title: " <> pShowStrict j)
    Just title' -> pure (JsonSchemaProcessedNumber title')
  Just SchemaTypeBoolean -> case title j of
    Nothing -> makeError ("boolean without a title: " <> pShowStrict j)
    Just title' -> pure (JsonSchemaProcessedBoolean title')
  Just SchemaTypeObject ->
    case additionalProperties j of
      Just additionalProps' -> do
        resolved <- fromObject additionalProps'
        pure (JsonSchemaProcessedDict resolved)
      Nothing ->
        case title j of
          Nothing -> makeError ("object without a title: " <> pShowStrict j)
          Just title' -> case properties j of
            Nothing -> makeError ("object without properties: " <> pShowStrict j)
            Just properties' -> do
              props <- traverse fromObject properties'
              defs <- traverse fromObject (definitions j)
              pure (JsonSchemaProcessedObject title' props defs (fromMaybe [] (required j)))
  Just SchemaTypeArray -> case items j of
    Nothing -> makeError ("array without items: " <> pShowStrict j)
    Just items' -> do
      newItems <- fromObject items'
      pure (JsonSchemaProcessedArray newItems)
  Nothing ->
    case enum j of
      Just enum' ->
        case title j of
          Just title' -> pure (JsonSchemaProcessedEnum title' enum')
          Nothing -> makeError ("enum without title: " <> pShowStrict j)
      Nothing -> case anyOf j of
        Nothing -> makeError ("object without type, enum and any of (what is this?!): " <> pShowStrict j)
        Just anyOf' -> case title j of
          Nothing -> makeError ("any of without a title: " <> pShowStrict j)
          Just title' -> do
            subElements <- traverse fromObject anyOf'
            pure (JsonSchemaProcessedUnion title' subElements)
