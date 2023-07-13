{-# LANGUAGE Safe #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall -Werror #-}

module Elm.GenError (GenError (..)) where

import Data.Eq (Eq)
import Data.Function (($))
import Data.Monoid (Monoid (mempty))
import Data.Semigroup (Semigroup ((<>)))
import Data.Text (Text)
import Text.Show (Show)

-- | The error type
data GenError
  = WarningList [Text]
  | Error Text
  deriving (Eq, Show)

instance Semigroup GenError where
  (Error x) <> _ = Error x
  _ <> (Error y) = Error y
  (WarningList x) <> (WarningList y) = WarningList $ x <> y

instance Monoid GenError where
  mempty = WarningList []
