{-# LANGUAGE OverloadedStrings #-}

module JsonSchemaTypeEnum where

import Data.Aeson (FromJSON (parseJSON), withText)
import Data.Text (unpack)

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
