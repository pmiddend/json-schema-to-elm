{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall -Werror #-}
{-# OPTIONS_HADDOCK prune #-}

-- | Ast for expressing imports
module Elm.Import where

import Control.Monad (Functor (fmap), Monad (return), mapM)
import Data.Function (($), (.))
import Data.Maybe (Maybe (Just, Nothing), fromMaybe)
import Data.Semigroup ((<>))
import Data.String (String)
import Elm.Classes (Generate (generate))
import Text.PrettyPrint (empty, hsep, parens, punctuate, text, (<+>))

-- | Possible ways to expose an import
data ImportType
  = Everything
  | Select [ImportItem]

-- | Possible ways to expose a sub import
data ImportItem
  = Item String
  | ItemExposing String [String]
  | ItemEvery String

-- | A full import
data Import = Import String (Maybe String) (Maybe ImportType)

instance Generate ImportItem where
  generate item =
    case item of
      Item str ->
        return . text $ str
      ItemExposing str [] ->
        return $ text str <> "()"
      ItemExposing str exposedItems ->
        return $ text str <> (parens . hsep . punctuate "," . fmap text $ exposedItems)
      ItemEvery str ->
        return $ text str <> "(..)"

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
    let asDoc = fromMaybe empty $ fmap (\str -> "as" <+> text str) $ as
    exposingDoc <-
      case exposing of
        Nothing ->
          return empty
        Just e -> do
          docE <- generate e
          return $ "exposing" <+> docE
    return $ "import" <+> text name <+> asDoc <+> exposingDoc
