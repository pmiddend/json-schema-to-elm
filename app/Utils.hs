module Utils where

import Control.Applicative (Applicative (pure))
import Data.Either (Either (Left))
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

newtype ErrorMessage = ErrorMessage Text

makeError :: Text -> Either ErrorMessage b
makeError = Left . ErrorMessage

retrieveError :: ErrorMessage -> Text
retrieveError (ErrorMessage e) = e

returnError :: Applicative t => Text -> t (Either ErrorMessage b)
returnError = pure . makeError

safeHead :: [a] -> Maybe a
safeHead l = case l of
  (x : _) -> Just x
  _ -> Nothing
