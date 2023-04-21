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
import Data.Maybe (Maybe (Just, Nothing), fromMaybe)
import Data.Semigroup ((<>))
import Data.Text (Text, breakOnEnd, intercalate, isPrefixOf, unpack)
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

-- From a schema involving "ref" elements, create a schema that has the refs resolved.
-- We have a helper function resolveRefs', because when resolving references, we want to start the definition search at the root element, and thus have to keep that
resolveRefs :: SubschemaF RefOrSubschema -> Either Text Subschema
resolveRefs topRoot = resolveRefs' topRoot
  where
    -- Little weird wrapFix contraption here, but that's just to make the types work out
    resolveRefs' root = wrapFix <$> traverse resolveRef root
    resolveRef :: RefOrSubschema -> Either Text Subschema
    resolveRef (RefOrSubschemaSchema x) = resolveRefs' x
    resolveRef (RefOrSubschemaRef ref) =
      if "#/definitions/" `isPrefixOf` ref
        then -- Separate the prefix from the suffix (and ignore intermediate paths, so someting like
        -- #/definitions/foo/bar
        -- is not supported yet

          let actualDefinition = snd (breakOnEnd "/" ref)
           in case Map.lookup actualDefinition (fromMaybe Map.empty (definitions topRoot)) of
                Nothing -> Left ("ref " <> actualDefinition <> ": not found in definitions in root, keys are " <> intercalate "," (Map.keys (fromMaybe Map.empty (definitions topRoot))))
                Just v -> case resolveRef v of
                  Left e -> Left ("ref " <> actualDefinition <> ": error resolving: " <> e)
                  Right resolvedRef -> Right resolvedRef
        else Left ("ref " <> ref <> ": doesn't start with \"#/definitions\"")
