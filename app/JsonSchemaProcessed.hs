{-# LANGUAGE OverloadedStrings #-}

module JsonSchemaProcessed where

import Control.Applicative (Applicative (pure))
import Data.Either (Either (Left))
import qualified Data.Map as Map
import Data.Maybe (Maybe (Just, Nothing), fromMaybe)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Traversable (Traversable (traverse))
import JsonSchemaObject (JsonSchemaObject (anyOf, definitions, enum, items, properties, required, title, type_))
import JsonSchemaTypeEnum (SchemaTypeEnum (..))
import Text.Show (Show)
import Utils (pShowStrict)
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
  | JsonSchemaProcessedUnion {unionTitle :: Text, unionItems :: [JsonSchemaProcessed]}
  | JsonSchemaProcessedInt
  | JsonSchemaProcessedString {stringTitle :: Text}
  | JsonSchemaProcessedNumber {numberTitle :: Text}
  | JsonSchemaProcessedBoolean {booleanTitle :: Text}
  deriving (Show)

fromObject :: JsonSchemaObject -> Either Text JsonSchemaProcessed
fromObject j = case type_ j of
  Just SchemaTypeInteger -> pure JsonSchemaProcessedInt
  Just SchemaTypeString -> case title j of
    Nothing -> Left ("string without a title: " <> pShowStrict j)
    Just title' -> pure (JsonSchemaProcessedString title')
  Just SchemaTypeNumber -> case title j of
    Nothing -> Left ("number without a title: " <> pShowStrict j)
    Just title' -> pure (JsonSchemaProcessedNumber title')
  Just SchemaTypeBoolean -> case title j of
    Nothing -> Left ("boolean without a title: " <> pShowStrict j)
    Just title' -> pure (JsonSchemaProcessedBoolean title')
  Just SchemaTypeObject -> case title j of
    Nothing -> Left ("object without a title: " <> pShowStrict j)
    Just title' -> case properties j of
      Nothing -> Left ("object without properties: " <> pShowStrict j)
      Just properties' -> do
        props <- traverse fromObject properties'
        defs <- traverse fromObject (definitions j)
        pure (JsonSchemaProcessedObject title' props defs (fromMaybe [] (required j)))
  Just SchemaTypeArray -> case items j of
    Nothing -> Left ("array without items: " <> pShowStrict j)
    Just items' -> do
      newItems <- fromObject items'
      pure (JsonSchemaProcessedArray newItems)
  Nothing ->
    case enum j of
      Just enum' ->
        case title j of
          Just title' -> pure (JsonSchemaProcessedEnum title' enum')
          Nothing -> Left ("enum without title: " <> pShowStrict j)
      Nothing -> case anyOf j of
        Nothing -> Left ("object without type, enum and any of (what is this?!): " <> pShowStrict j)
        Just anyOf' -> case title j of
          Nothing -> Left ("any of without a title: " <> pShowStrict j)
          Just title' -> do
            subElements <- traverse fromObject anyOf'
            pure (JsonSchemaProcessedUnion title' subElements)
