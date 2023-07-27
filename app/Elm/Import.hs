{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall -Werror #-}
{-# OPTIONS_HADDOCK prune #-}

-- | Ast for expressing imports
module Elm.Import where

import Control.Monad (Functor (fmap), Monad (return), mapM)
import Data.Function (($), (.))
import Data.Maybe (Maybe (Just, Nothing), maybe)
import Data.Semigroup ((<>))
import Data.Text (Text, unpack)
import Elm.Classes (Generate (generate))
import Text.PrettyPrint (empty, hsep, parens, punctuate, text, (<+>))
import Prelude ()

-- | Possible ways to expose an import
data ImportType
  = Everything
  | Select [ImportItem]

-- | Possible ways to expose a sub import
data ImportItem
  = Item Text
  | ItemExposing Text [Text]
  | ItemEvery Text

-- | A full import
data Import = Import Text (Maybe Text) (Maybe ImportType)

instance Generate ImportItem where
  generate item =
    case item of
      Item str ->
        return . text . unpack $ str
      ItemExposing str [] ->
        return $ text (unpack str) <> "()"
      ItemExposing str exposedItems ->
        return $ text (unpack str) <> (parens . hsep . punctuate "," . fmap (text . unpack) $ exposedItems)
      ItemEvery str ->
        return $ text (unpack str) <> "(..)"

instance Generate ImportType where
  generate item =
    case item of
      Everything ->
        return "(..)"
      Select [] -> do
        return "()"
      Select items -> do
        docItems <- mapM generate items
        return $ parens . hsep . punctuate "," $ docItems

instance Generate Import where
  generate (Import name as exposing) = do
    let asDoc = maybe empty (\str -> "as" <+> text (unpack str)) as
    exposingDoc <-
      case exposing of
        Nothing ->
          return empty
        Just e -> do
          docE <- generate e
          return $ "exposing" <+> docE
    return $ "import" <+> text (unpack name) <+> asDoc <+> exposingDoc
