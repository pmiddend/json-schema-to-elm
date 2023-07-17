{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module JsonSchemaObjectRef where

import Control.Applicative ((<*>))
import Data.Aeson (FromJSON (parseJSON), withObject, (.:?))
import Data.Functor ((<$>))
import qualified Data.Map as Map
import Data.Maybe (Maybe)
import Data.Text (Text)
import GHC.Generics (Generic)
import JsonSchemaTypeEnum
import Text.Show (Show)
import Prelude ()

type ObjectRefMap = Map.Map Text JsonSchemaObjectRef

data JsonSchemaObjectRef = JsonSchemaObjectRef
  { title :: Maybe Text,
    description :: Maybe Text,
    type_ :: Maybe SchemaTypeEnum,
    ref :: Maybe Text,
    properties :: Maybe ObjectRefMap,
    required :: Maybe [Text],
    items :: Maybe JsonSchemaObjectRef,
    enum :: Maybe [Text],
    definitions :: Maybe ObjectRefMap,
    anyOf :: Maybe [JsonSchemaObjectRef]
  }
  deriving (Show, Generic)

instance FromJSON JsonSchemaObjectRef where
  parseJSON = withObject "JsonSchemaObjectRef" \v ->
    JsonSchemaObjectRef
      <$> v .:? "title"
      <*> v .:? "description"
      <*> v .:? "type"
      <*> v .:? "$ref"
      <*> v .:? "properties"
      <*> v .:? "required"
      <*> v .:? "items"
      <*> v .:? "enum"
      <*> v .:? "definitions"
      <*> v .:? "anyOf"
