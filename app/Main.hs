{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (eitherDecodeStrict)
import Data.ByteString (getContents)
import Data.Either (Either (Left, Right))
-- import Elm (ToElmMode (Deep), jsonSchemaToElm)
-- import JsonSchema (RefOrSubschema, Subschema, SubschemaF (definitions, required, title, type_), SubschemaTypeF (SubschemaTypeArray, SubschemaTypeBoolean, SubschemaTypeEnum, SubschemaTypeInteger, SubschemaTypeNumber, SubschemaTypeObject, SubschemaTypeString, enumMembers), resolveRefs)
-- import JsonSchemaNew (SchemaType, SchemaTypeEnum)

import Data.Maybe (Maybe (..))
import Data.Monoid (Monoid (..))
import Data.Semigroup ((<>))
import Data.String (String)
import Data.Text (Text, pack)
import Data.Text.IO (putStrLn)
-- import Elm (ElmType, Union (Union), UnionAlternative (UnionAlternative), unionToElm)

import Elm (Module)
import JsonSchemaObject (JsonSchemaObject (title, type_), fromRef)
import JsonSchemaObjectRef (JsonSchemaObjectRef)
import JsonSchemaTypeEnum (SchemaTypeEnum (..))
import System.IO (IO)
import Text.Pretty.Simple (pPrint)
import Text.Show (Show (..))
import Prelude (Maybe (Just), print, undefined)

-- printSubschema :: Subschema -> IO ()
-- printSubschema = printSubschema' 0
--   where
--     printSubschema' :: Int -> Subschema -> IO ()
--     printSubschema' indent (Fix ss) = do
--       putStrLn (replicate indent " " <> title ss <> ", required: " <> intercalate ", " (fromMaybe [] (required ss)) <> ", type: ")
--       printSubschemaType indent (type_ ss)
--     printSubschemaType :: Int -> SubschemaTypeF Subschema -> IO ()
--     printSubschemaType indent SubschemaTypeInteger = putStrLn (replicate indent " " <> "integer")
--     printSubschemaType indent SubschemaTypeString = putStrLn (replicate indent " " <> "string")
--     printSubschemaType indent SubschemaTypeNumber = putStrLn (replicate indent " " <> "number")
--     printSubschemaType indent SubschemaTypeBoolean = putStrLn (replicate indent " " <> "boolean")
--     printSubschemaType indent (SubschemaTypeEnum {enumMembers = m}) = putStrLn (replicate indent " " <> "enum(" <> intercalate "," m <> ")")
--     printSubschemaType indent (SubschemaTypeArray items) = do
--       putStrLn (replicate indent " " <> "array, subtype:")
--       printSubschema' (indent + 2) items
--     printSubschemaType indent (SubschemaTypeObject props) = do
--       putStrLn (replicate indent " " <> "object, props:")
--       forM_ (Map.toList props) $ \(name, type_') -> do
--         putStrLn (replicate (indent + 1) " " <> name)
--         printSubschema' (indent + 2) type_'

schemaToElm :: JsonSchemaObject -> Either Text Module
schemaToElm j = case type_ j of
  Just SchemaTypeObject ->
    case title j of
      Nothing -> Left "object type has no title, what do I name the type alias then?"
      Just title' ->
        let alias = decTypeAlias title'
         in Right (module_ "MyOutput" Everything [] [])
  t -> Left ("unknown type " <> pack (show t))

main :: IO ()
main = do
  stdin <- getContents
  case eitherDecodeStrict stdin :: Either String JsonSchemaObjectRef of
    Left e -> putStrLn ("error decoding JSON: " <> pack e)
    Right v -> do
      putStrLn "decoded:"
      pPrint v
      case fromRef v of
        Left e -> putStrLn ("error derefing: " <> e)
        Right v' -> do
          putStrLn "derefed:"
          pPrint v'

-- print (unionToElm (Union "TestUnion" [UnionAlternative "A1" ["Foo", "Bar"], UnionAlternative "A2" ["Qux", "Quux"]]))

-- case eitherDecodeStrict stdin :: Either String (SubschemaF RefOrSubschema) of
--   Left e -> putStrLn (pack e)
--   Right v -> case resolveRefs v of
--     Left e' -> putStrLn e'
--     Right resolved -> do
--       -- printSubschema resolved
--       -- putStrLn (jsonSchemaToElm Deep resolved)
--       -- case definitions (unFix resolved) of
--       --   Nothing -> pure ()
--       --   Just definitions -> putStrLn (jsonSchemaToElm Deep definitions)
--       for_ (definitions (unFix resolved)) (mapM_ (putStrLn . jsonSchemaToElm Deep))
--       putStrLn (jsonSchemaToElm Deep resolved)
