{-# LANGUAGE OverloadedStrings #-}

module JsonSchemaProcessed where

import Control.Applicative (Applicative (pure))
import Control.Monad ((>>=))
import Data.Either (Either)
import qualified Data.List.NonEmpty as NE
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
  | JsonSchemaProcessedEnum {enumTitle :: Text, enumItems :: NE.NonEmpty Text}
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
  | JsonSchemaProcessedOptional {optionalItem :: JsonSchemaProcessed}
  | JsonSchemaProcessedInt {intTitle :: Maybe Text}
  | JsonSchemaProcessedNull
  | JsonSchemaProcessedString {stringTitle :: Maybe Text}
  | JsonSchemaProcessedNumber {numberTitle :: Maybe Text}
  | JsonSchemaProcessedBoolean {booleanTitle :: Maybe Text}
  deriving (Show)

-- Here, we convert between the "raw" JSON representation (with references resolved) to our "processed" one that's
-- much easier to digest and turn into Elm code
fromObject :: JsonSchemaObject -> Either ErrorMessage JsonSchemaProcessed
fromObject j = case type_ j of
  Just SchemaTypeNull -> pure JsonSchemaProcessedNull
  Just SchemaTypeInteger -> pure (JsonSchemaProcessedInt (title j))
  Just SchemaTypeString -> do
    -- You can have strings with an additional enum property!
    --
    -- "CollimatorStatus": {
    --   "enum": [
    --     "colli_in",
    --     "colli_out",
    --     "colli_unknown"
    --   ],
    --   "title": "CollimatorStatus",
    --   "type": "string"
    -- },
    case enum j >>= NE.nonEmpty of
      Just enum' ->
        case title j of
          Just title' -> pure (JsonSchemaProcessedEnum title' enum')
          Nothing -> makeError ("enum without title: " <> pShowStrict j)
      Nothing -> pure (JsonSchemaProcessedString (title j))
  Just SchemaTypeNumber -> pure (JsonSchemaProcessedNumber (title j))
  Just SchemaTypeBoolean -> pure (JsonSchemaProcessedBoolean (title j))
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
    case enum j >>= NE.nonEmpty of
      Just enum' ->
        case title j of
          Just title' -> pure (JsonSchemaProcessedEnum title' enum')
          Nothing -> makeError ("enum without title: " <> pShowStrict j)
      Nothing -> case anyOf j of
        Nothing -> makeError ("object without type, enum and any of (what is this?!): " <> pShowStrict j)
        Just anyOf' -> do
          subElements <- traverse fromObject anyOf'
          case subElements of
            [JsonSchemaProcessedNull, optionalItem'] -> pure (JsonSchemaProcessedOptional optionalItem')
            [optionalItem', JsonSchemaProcessedNull] -> pure (JsonSchemaProcessedOptional optionalItem')
            _ ->
              case title j of
                Nothing -> makeError ("anyOf without a title and not an optional value: " <> pShowStrict j)
                Just title' -> pure (JsonSchemaProcessedUnion title' subElements)
