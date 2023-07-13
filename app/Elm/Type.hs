{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune #-}

-- | Ast for declaring types
module Elm.Type (TypeDec (..)) where

import Control.Monad (Functor (fmap), Monad (return), mapM)
import Control.Monad.Writer (tell)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.List (intersperse, unzip, zip)
import Data.Maybe (Maybe (Just, Nothing))
import Data.Semigroup ((<>))
import Data.String (IsString (fromString))
import Data.Text (Text, pack, unpack)
import Elm.Classes (Generate (generate))
import Elm.GenError (GenError (Error, WarningList))
import Text.PrettyPrint (hsep, lbrace, parens, punctuate, rbrace, text, (<+>))
import Prelude ()

-- | Data type to represent types
data TypeDec
  = -- | A type with type paramaters
    Params Text [TypeDec]
  | -- | A function type
    TApp [TypeDec]
  | -- | A tuple type
    TTuple [TypeDec]
  | -- | A record type
    TRecord (Maybe Text) [(Text, TypeDec)]

instance IsString TypeDec where
  fromString x = Params (pack x) []

instance Generate TypeDec where
  generate typeDec =
    case typeDec of
      Params type_ params -> do
        docParams <- mapM vopParam params
        return $ text (unpack type_) <+> hsep docParams
      TApp decs -> do
        docDecs <- mapM vopTApp decs
        return . hsep . intersperse "->" $ docDecs
      TTuple [] -> do
        return "()"
      TTuple [item] -> do
        tell $ WarningList ["Attempt to create a one item tuple"]
        parens <$> generate item
      TTuple items -> do
        docItems <- mapM generate items
        return . parens . hsep . punctuate "," $ docItems
      TRecord Nothing [] -> do
        tell $ Error "Unable to create a record type with no base and no constraints"
        return ""
      TRecord (Just str) [] -> do
        tell $ WarningList ["You are creating a record type from " <> str <> " with no constraints"]
        return . text . unpack $ str
      TRecord Nothing constraints -> do
        cDoc <- generateTRecordList constraints
        return $ lbrace <+> cDoc <+> rbrace
      TRecord (Just str) constraints -> do
        cDoc <- generateTRecordList constraints
        return $ lbrace <+> text (unpack str) <+> text "|" <+> cDoc <+> rbrace
    where
      generateTRecordList constraints = do
        let (keys, values) = unzip constraints
        let docKeys = text . unpack <$> keys
        docValues <- mapM generate values
        let docList = zip docKeys docValues
        return . hsep . punctuate "," . fmap (\(a, b) -> a <+> ":" <+> b) $ docList

      vopParam type_ =
        case type_ of
          Params str [] ->
            return . text . unpack $ str
          _ ->
            parens <$> generate type_

      vopTApp type_ =
        case type_ of
          TApp _ ->
            parens <$> generate type_
          _ ->
            generate type_
