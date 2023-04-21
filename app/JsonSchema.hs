{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module JsonSchema where

import Control.Applicative (pure)
import Control.Monad (fail, foldM)
import Data.Aeson (FromJSON (parseJSON), withObject, (.:), (.:?))
import Data.Either (Either (Left, Right))
import Data.Fix (Fix, wrapFix)
import Data.Foldable (Foldable)
import Data.Function (($))
import Data.Functor (Functor, (<$>))
import qualified Data.Map as Map
import Data.Maybe (Maybe (Just, Nothing))
import Data.Semigroup ((<>))
import Data.Text (Text, breakOnEnd, isPrefixOf, unpack)
import Data.Traversable (Traversable, traverse)
import Text.Show (Show)
import Prelude (snd)

data SubschemaTypeF a
  = SubschemaTypeObject {properties :: Map.Map Text a}
  | SubschemaTypeArray a
  | SubschemaTypeEnum {enumMembers :: [Text]}
  | SubschemaTypeInteger
  | SubschemaTypeString
  | SubschemaTypeNumber
  | SubschemaTypeBoolean
  deriving (Show, Foldable, Traversable, Functor)

data RefOrSubschema
  = RefOrSubschemaRef Text
  | RefOrSubschemaSchema (SubschemaF RefOrSubschema)
  deriving (Show)

data SubschemaF a = SubschemaF
  { title :: Text,
    required :: Maybe [Text],
    type_ :: SubschemaTypeF a,
    definitions :: Maybe (Map.Map Text a)
  }
  deriving (Show, Foldable, Functor, Traversable)

type Subschema = Fix SubschemaF

instance FromJSON RefOrSubschema where
  parseJSON originalValue = withObject "RefOrSubschema" processor originalValue
    where
      processor v = do
        ref <- v .:? "$ref"
        case ref of
          Nothing -> RefOrSubschemaSchema <$> parseJSON originalValue
          Just ref' -> pure (RefOrSubschemaRef ref')

instance FromJSON (SubschemaF RefOrSubschema) where
  parseJSON = withObject "Subschema" $ \v -> do
    title' <- v .: "title"
    type' :: Maybe Text <- v .:? "type"
    required' <- v .:? "required"
    definitions' <- v .:? "definitions"
    resolvedType <- case type' of
      Just "integer" -> pure SubschemaTypeInteger
      Just "string" -> pure SubschemaTypeString
      Just "number" -> pure SubschemaTypeNumber
      Just "boolean" -> pure SubschemaTypeBoolean
      Just "array" -> do
        items' <- v .: "items"
        pure (SubschemaTypeArray items')
      Just "object" -> do
        properties' <- v .: "properties"
        subProperties <- traverse parseJSON properties'
        pure (SubschemaTypeObject subProperties)
      Just unknownType -> fail ("unknown schema type \"" <> unpack unknownType <> "\"")
      Nothing -> SubschemaTypeEnum <$> (v .: "enum")

    pure (SubschemaF title' required' resolvedType definitions')

resolveRefs :: SubschemaF RefOrSubschema -> Either Text Subschema
resolveRefs s =
  case resolvedDefs of
    Left e -> Left e
    Right resolvedDefs' -> wrapFix <$> traverse (resolveRef resolvedDefs') s
  where
    resolvedDefs :: Either Text (Map.Map Text Subschema)
    resolvedDefs = case definitions s of
      Nothing -> Right Map.empty
      Just defs -> traverse (resolveRef Map.empty) defs

    resolveRef :: Map.Map Text Subschema -> RefOrSubschema -> Either Text Subschema
    resolveRef _ (RefOrSubschemaSchema x) = resolveRefs x
    resolveRef defs (RefOrSubschemaRef ref) =
      if "#/definitions/" `isPrefixOf` ref
        then case Map.lookup (snd (breakOnEnd "/" ref)) defs of
          Nothing -> Left ("ref " <> ref <> ": not found in definitions in root")
          Just v -> Right v
        else Left ("ref " <> ref <> ": doesn't start with \"#/definitions\"")
