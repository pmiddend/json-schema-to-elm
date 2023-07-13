{-# LANGUAGE Safe #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall -Werror #-}
{-# OPTIONS_HADDOCK -prune #-}

-- | A series of helper classes to make writing an ast easier
module Elm.Classes where

import Control.Monad.Writer (Writer)
import Elm.GenError (GenError)
import Text.PrettyPrint (Doc)

class Generate a where
  generate :: a -> Writer GenError Doc
