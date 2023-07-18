module Utils where

import Data.Function ((.))
import Data.Maybe (Maybe (Just, Nothing))
import Data.Text (Text, pack)
import Data.Text.Lazy (toStrict)
import Text.Pretty.Simple (pShow)
import Text.Show (Show (show))
import Prelude ()

packShow :: Show a => a -> Text
packShow = pack . show

pShowStrict :: Show a => a -> Text
pShowStrict = toStrict . pShow

type ErrorMessage = Text

safeHead :: [a] -> Maybe a
safeHead l = case l of
  (x : _) -> Just x
  _ -> Nothing
