{-# LANGUAGE OverloadedStrings #-}

module Elm where

import Data.Fix (Fix (..))
import Data.Functor ((<$>))
import Data.List (elem)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import Data.Text (Text, intercalate, pack, unpack)
import JsonSchema (Subschema, SubschemaF (required, title, type_), SubschemaTypeF (SubschemaTypeArray, SubschemaTypeBoolean, SubschemaTypeEnum, SubschemaTypeInteger, SubschemaTypeNumber, SubschemaTypeObject, SubschemaTypeString, enumMembers, properties))
import Text.Casing (fromSnake, toCamel)
import Prelude ((.))

data ToElmMode = Flat | Deep

caseConvert :: Text -> Text
caseConvert = pack . toCamel . fromSnake . unpack

jsonSchemaToElm :: ToElmMode -> Subschema -> Text
jsonSchemaToElm toElmMode (Fix s) = case type_ s of
  SubschemaTypeObject {properties = props} ->
    case toElmMode of
      Flat -> title s
      Deep ->
        let addMaybe :: Text -> Text -> Text
            addMaybe name resolvedType =
              if name `elem` fromMaybe [] (required s)
                then resolvedType
                else "Maybe " <> resolvedType
            makeMember :: (Text, Subschema) -> Text
            makeMember (name, type_') = caseConvert name <> ": " <> addMaybe name (jsonSchemaToElm Flat type_')
            members = intercalate ", " (makeMember <$> Map.toList props)
         in "type alias " <> title s <> "{ " <> members <> " }"
  SubschemaTypeArray a -> "List " <> jsonSchemaToElm Flat a
  SubschemaTypeEnum {enumMembers = members} -> case toElmMode of
    Flat -> title s
    Deep -> "type " <> title s <> " = " <> intercalate " | " (caseConvert <$> members)
  SubschemaTypeInteger -> "Int"
  SubschemaTypeString -> "String"
  SubschemaTypeNumber -> "Float"
  SubschemaTypeBoolean -> "Boolean"
