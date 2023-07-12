{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module JsonSchemaObject where

import Control.Applicative (Applicative (..))
import Control.Monad (foldM)
import Data.Aeson (FromJSON)
import Data.Bool (not)
import Data.Either (Either (Left, Right))
import Data.Foldable (Foldable)
import Data.Functor ((<$>))
import qualified Data.Map as Map
import Data.Maybe (Maybe (Just, Nothing), fromMaybe)
import Data.Monoid (Monoid (..))
import Data.Ord (Ord)
import Data.Semigroup ((<>))
import Data.Text (Text, drop, isPrefixOf, length, unpack)
import GHC.Generics (Generic)
import qualified JsonSchemaObjectRef as Ref
import JsonSchemaTypeEnum
import Text.Show (Show)
import Prelude ()

type ObjectMap = Map.Map Text JsonSchemaObject

data JsonSchemaObject = JsonSchemaObject
  { title :: Maybe Text,
    description :: Maybe Text,
    type_ :: Maybe SchemaTypeEnum,
    properties :: Maybe ObjectMap,
    required :: Maybe [Text],
    enum :: Maybe [Text],
    definitions :: ObjectMap,
    anyOf :: Maybe [JsonSchemaObject]
  }
  deriving (Show, Generic)

instance FromJSON JsonSchemaObject

expectedRefPrefix :: Text
expectedRefPrefix = "#/definitions/"

resolveMap :: (Foldable t, Ord k) => Ref.JsonSchemaObjectRef -> t (k, Ref.JsonSchemaObjectRef) -> Either Text (Map.Map k JsonSchemaObject)
resolveMap root =
  foldM
    (\prevMap (newKey, newElement) -> (\resolvedEl -> Map.insert newKey resolvedEl prevMap) <$> resolveSingleDefinition (fromMaybe mempty (Ref.definitions root)) newElement)
    mempty

resolveSingleDefinition :: Ref.ObjectRefMap -> Ref.JsonSchemaObjectRef -> Either Text JsonSchemaObject
resolveSingleDefinition refdefs root =
  case Ref.ref root of
    Just ref' ->
      if not (expectedRefPrefix `isPrefixOf` ref')
        then Left ("reference doesn't start with \"" <> expectedRefPrefix <> "\": " <> ref')
        else case Map.lookup (drop (length expectedRefPrefix) ref') refdefs of
          Nothing -> Left ("looking up #/definitions/" <> ref' <> ": not found")
          Just result -> resolveSingleDefinition refdefs result
    Nothing -> do
      defs <- resolveMap root (Map.toList (fromMaybe mempty (Ref.definitions root)))
      props <- case Ref.properties root of
        Nothing -> Right Nothing
        Just props' ->
          Just <$> resolveMap root (Map.toList props')

      anyOfs <- case Ref.anyOf root of
        Nothing -> Right Nothing
        Just anyOfs' ->
          Just
            <$> foldM
              (\prevList newElement -> (: prevList) <$> resolveSingleDefinition (fromMaybe mempty (Ref.definitions root)) newElement)
              mempty
              anyOfs'
      pure
        ( JsonSchemaObject
            (Ref.title root)
            (Ref.description root)
            (Ref.type_ root)
            props
            (Ref.required root)
            (Ref.enum root)
            defs
            anyOfs
        )

fromRef :: Ref.JsonSchemaObjectRef -> Either Text JsonSchemaObject
fromRef = resolveSingleDefinition mempty
