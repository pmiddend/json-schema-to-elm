{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wall -Werror #-}
{-# OPTIONS_HADDOCK prune #-}

-- | Top level declerations
module Elm.Declaration where

import Control.Monad (Functor (fmap), Monad (return), mapM)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.List (head, tail, unzip, zip)
import Data.Text (Text, unpack)
import Elm.Classes (Generate (generate))
import Elm.Expression (Expr)
import Elm.Type (TypeDec)
import Text.PrettyPrint (hsep, nest, text, vcat, ($+$), (<+>))
import Prelude ()

-- | Used to declare functions, variables, and types
data Dec
  = -- | Declare a function
    Dec Text TypeDec [Expr] Expr
  | -- | Declare a type
    DecType Text [Text] [(Text, [TypeDec])]
  | -- | Declare a type alias
    DecTypeAlias Text [Text] TypeDec

instance Generate Dec where
  generate dec =
    case dec of
      Dec name type_ params value -> do
        typeDoc <- generate type_
        paramDocs <- mapM generate params
        valueDoc <- generate value
        return $ text (unpack name) <+> ":" <+> typeDoc $+$ text (unpack name) <+> hsep paramDocs <+> "=" $+$ nest 4 valueDoc
      DecType name params instances -> do
        let (keys, values) = unzip instances
        let keyDocs = text . unpack <$> keys
        valueDocs <- mapM (mapM generate) values
        let instanceDocs = (\(key, values') -> key <+> hsep values') <$> zip keyDocs valueDocs
        let paramDocs = text . unpack <$> params
        return $
          "type"
            <+> text (unpack name)
            <+> hsep paramDocs
            $+$ nest 4 ("=" <+> head instanceDocs $+$ (vcat . fmap ((<+>) "|") . tail $ instanceDocs))
      DecTypeAlias name params type_ -> do
        typeDoc <- generate type_
        let paramDocs = text . unpack <$> params
        return $ "type alias" <+> text (unpack name) <+> hsep paramDocs <+> "=" $+$ nest 4 typeDoc
