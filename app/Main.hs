{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_)
import Data.Aeson (eitherDecodeStrict)
import Data.ByteString (getContents)
import Data.Either (Either (Left, Right))
import Data.Fix (Fix (..))
import Data.Function (($))
import Data.Int (Int)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import Data.String (String)
import Data.Text (intercalate, pack, replicate)
import Data.Text.IO (putStrLn)
import JsonSchema (RefOrSubschema, Subschema, SubschemaF (required, title, type_), SubschemaTypeF (SubschemaTypeArray, SubschemaTypeBoolean, SubschemaTypeEnum, SubschemaTypeInteger, SubschemaTypeNumber, SubschemaTypeObject, SubschemaTypeString, enumMembers), resolveRefs)
import System.IO (IO)
import Prelude (Num ((+)))

printSubschema :: Subschema -> IO ()
printSubschema = printSubschema' 0
  where
    printSubschema' :: Int -> Subschema -> IO ()
    printSubschema' indent (Fix ss) = do
      putStrLn (replicate indent " " <> title ss <> ", required: " <> intercalate ", " (fromMaybe [] (required ss)) <> ", type: ")
      printSubschemaType indent (type_ ss)
    printSubschemaType :: Int -> SubschemaTypeF Subschema -> IO ()
    printSubschemaType indent SubschemaTypeInteger = putStrLn (replicate indent " " <> "integer")
    printSubschemaType indent SubschemaTypeString = putStrLn (replicate indent " " <> "string")
    printSubschemaType indent SubschemaTypeNumber = putStrLn (replicate indent " " <> "number")
    printSubschemaType indent SubschemaTypeBoolean = putStrLn (replicate indent " " <> "boolean")
    printSubschemaType indent (SubschemaTypeEnum {enumMembers = m}) = putStrLn (replicate indent " " <> "enum(" <> intercalate "," m <> ")")
    printSubschemaType indent (SubschemaTypeArray items) = do
      putStrLn (replicate indent " " <> "array, subtype:")
      printSubschema' (indent + 2) items
    printSubschemaType indent (SubschemaTypeObject props) = do
      putStrLn (replicate indent " " <> "object, props:")
      forM_ (Map.toList props) $ \(name, type_') -> do
        putStrLn (replicate (indent + 1) " " <> name)
        printSubschema' (indent + 2) type_'

main :: IO ()
main = do
  stdin <- getContents
  case eitherDecodeStrict stdin :: Either String (SubschemaF RefOrSubschema) of
    Left e -> putStrLn (pack e)
    Right v -> case resolveRefs v of
      Left e' -> putStrLn e'
      Right resolved -> printSubschema resolved
