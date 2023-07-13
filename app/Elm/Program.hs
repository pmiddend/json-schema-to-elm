{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall -Werror #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_HADDOCK prune #-}

-- {-# LANGUAGE Safe              #-}

-- | Module for creating a program
module Elm.Program where

import Control.Monad (Monad (return), mapM)
import Control.Monad.Writer (runWriter)
import Data.Function (($), (.))
import Data.Functor (Functor (fmap))
import Data.List (or)
import Data.Semigroup ((<>))
import Data.Text (Text, isInfixOf, pack, splitOn, unlines, unpack)
import Elm.Classes (Generate (generate))
import Elm.Declaration (Dec)
import Elm.GenError (GenError)
import Elm.Import (Import, ImportType)
import Text.PrettyPrint (render, text, vcat, ($+$), (<+>))

-- | Program type
data Program = Program Text ImportType [Import] [Dec]

instance Generate Program where
  generate (Program name exports imports declerations) = do
    exportDoc <- generate exports
    importDocs <- mapM generate imports
    decDocs <- mapM generate declerations
    return $
      "module"
        <+> text (unpack name)
        <+> "exposing"
        <+> exportDoc
        $+$ vcat importDocs
        $+$ vcat decDocs

-- | Convert a program to a string of code with newlines between declerations
renderProgram :: Program -> (Text, GenError)
renderProgram program =
  let (doc, parseError) = runWriter $ generate program
      str :: Text
      str = pack (render doc)
   in ((unlines . fmap addNewline . splitOn "\n" $ str) <> "\n", parseError)
  where
    addNewline :: Text -> Text
    addNewline line =
      if or $ fmap (`isInfixOf` line) [":", "type"]
        then "\n\n" <> line
        else line
