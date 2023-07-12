{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (eitherDecodeStrict)
import Data.ByteString (getContents)
import Data.Either (Either (Left, Right))
-- import Elm (ToElmMode (Deep), jsonSchemaToElm)
-- import JsonSchema (RefOrSubschema, Subschema, SubschemaF (definitions, required, title, type_), SubschemaTypeF (SubschemaTypeArray, SubschemaTypeBoolean, SubschemaTypeEnum, SubschemaTypeInteger, SubschemaTypeNumber, SubschemaTypeObject, SubschemaTypeString, enumMembers), resolveRefs)
-- import JsonSchemaNew (SchemaType, SchemaTypeEnum)

import Data.Monoid (Monoid (..))
import Data.Semigroup ((<>))
import Data.String (String)
import Data.Text (pack)
import Data.Text.IO (putStrLn)
import JsonSchemaObject (fromRef)
import JsonSchemaObjectRef
import System.IO (IO)
import Text.Pretty.Simple (pPrint)
import Prelude (print)

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
