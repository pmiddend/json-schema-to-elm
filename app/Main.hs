{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative (Applicative (pure))
import Control.Monad (foldM)
import Data.Aeson (eitherDecodeStrict)
import Data.Bool (Bool (..))
import Data.ByteString (getContents)
import Data.Either (Either (Left, Right))
import Data.Foldable (Foldable (elem, foldr))
import Data.Function ((.))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.List (length, take, zipWith)
import qualified Data.Map as Map
import Data.Maybe (Maybe (..), fromMaybe)
import Data.Semigroup ((<>))
import Data.String (String)
import Data.Text (Text, pack, singleton)
import Data.Text.IO (putStrLn)
import Data.Text.Manipulate (toCamel, toPascal)
import Data.Traversable (Traversable (traverse))
import Elm (Dec, Expr, Module, Type, app, bool, case_, decFunction, decType, decTypeAlias, importEvery, let_, list, module_, op, render, string, tapp, tparam, tparams, trecord, tvar, var)
import JsonSchemaObject (JsonSchemaObject (anyOf, definitions, enum, properties, required, title, type_), fromRef)
import JsonSchemaObjectRef (JsonSchemaObjectRef)
import JsonSchemaTypeEnum (SchemaTypeEnum (..))
import System.IO (IO)
import Text.Pretty.Simple (pPrint)
import Text.Show (Show (..))
import Prelude ()

packShow :: Show a => a -> Text
packShow = pack . show

buildDecoderName :: Text -> Text
buildDecoderName v = "decode" <> v

schemaTypeToElmDecoder :: JsonSchemaObject -> Either Text Expr
schemaTypeToElmDecoder j = case type_ j of
  Just SchemaTypeInteger -> Right (var "Decode.int")
  Just SchemaTypeNumber -> Right (var "Decode.float")
  Just SchemaTypeString -> Right (var "Decode.string")
  _ -> case title j of
    Nothing -> Left ("unknown type to name decoder for (no title): " <> packShow j)
    Just title' -> Right (var (buildDecoderName (toPascal title')))

schemaTypeToElmShallow :: JsonSchemaObject -> Either Text Type
schemaTypeToElmShallow j = case type_ j of
  Just SchemaTypeInteger -> Right (tvar "Int")
  Just SchemaTypeString -> Right (tvar "String")
  Just SchemaTypeNumber -> Right (tvar "Float")
  Just SchemaTypeBoolean -> Right (tvar "Boolean")
  Just SchemaTypeArray -> Left "array not supported yet"
  Just SchemaTypeObject ->
    case title j of
      Nothing -> Left "record without a title"
      Just title' -> Right (tvar (toPascal title'))
  Nothing ->
    case title j of
      Nothing -> Left "got something without a title, don't know how to shallow-print that"
      Just title' ->
        case anyOf j of
          Nothing -> Right (tvar (toPascal title'))
          Just anyOfs -> do
            subElements <- traverse schemaTypeToElmShallow anyOfs
            pure (tparams ("Union" <> packShow (length anyOfs)) subElements)

schemaTypeToElmName :: JsonSchemaObject -> Either Text Text
schemaTypeToElmName j = case type_ j of
  Just SchemaTypeInteger -> Right "Int"
  Just SchemaTypeString -> Right "String"
  Just SchemaTypeNumber -> Right "Float"
  Just SchemaTypeBoolean -> Right "Boolean"
  Just SchemaTypeArray -> Left "array not supported yet"
  Just SchemaTypeObject ->
    case title j of
      Nothing -> Left "record without a title"
      Just title' -> Right (toPascal title')
  Nothing ->
    case title j of
      Nothing -> Left "got something without a title, don't know how to shallow-print that"
      Just title' ->
        case anyOf j of
          Nothing -> Right (toPascal title')
          Just _ -> do
            Left "anyOf decoding not supported yet"

schemaObjectToDeclaration :: JsonSchemaObject -> Either Text Dec
schemaObjectToDeclaration j = case type_ j of
  Just SchemaTypeObject ->
    case title j of
      Nothing -> Left "record without a title"
      Just title' ->
        case properties j of
          Nothing -> Left ("record " <> title' <> " without properties")
          Just properties' ->
            let requiredProps :: [Text]
                requiredProps = fromMaybe [] (required j)
                propToRecordField :: Text -> JsonSchemaObject -> Either Text (Text, Type)
                propToRecordField propName obj = case schemaTypeToElmShallow obj of
                  Left e -> Left e
                  Right v ->
                    Right
                      ( if propName `elem` requiredProps
                          then (toCamel propName, v)
                          else (toCamel propName, tparam "Maybe" v)
                      )
             in do
                  recordResults <-
                    foldM
                      (\prevList (propName, prop) -> (: prevList) <$> propToRecordField propName prop)
                      []
                      (Map.toList properties')
                  pure
                    ( decTypeAlias
                        title'
                        []
                        (trecord recordResults)
                    )
  Just x ->
    case title j of
      Nothing -> Left ("schema entry without a title " <> packShow x)
      Just title' -> Left ("schema entry " <> title' <> " is neither object nor enum")
  Nothing ->
    case title j of
      Nothing -> Left ("schema entry without a title " <> packShow j)
      Just title' ->
        case enum j of
          Just enum' ->
            let makeConstructor x = (toPascal x, [])
             in Right (decType title' [] (makeConstructor <$> enum'))
          Nothing -> Left ("schema entry " <> title' <> " is neither object nor enum")

unionDeclaration :: Int -> Dec
unionDeclaration n =
  decType
    ("Union" <> packShow n)
    (singleton <$> take n ['a' .. 'z'])
    (zipWith (\c i -> ("Union" <> packShow n <> "Member" <> packShow i, [tvar (singleton c)])) ['a' .. 'z'] [0 .. n])

schemaToElm :: Text -> JsonSchemaObject -> Either Text Module
schemaToElm moduleName j = case type_ j of
  Just SchemaTypeObject -> do
    rootDeclaration <- schemaObjectToDeclaration j
    definitions' <- traverse schemaObjectToDeclaration (Map.elems (definitions j))
    definitionsDecoders <- traverse buildDecoder (Map.elems (definitions j))
    rootDecoder <- buildDecoder j
    let unions = unionDeclaration <$> [0 .. 7]
    pure
      ( module_
          moduleName
          importEvery
          []
          (rootDeclaration : constantStringDecoder : rootDecoder : (definitions' <> unions <> definitionsDecoders))
      )
  t -> Left ("unknown type " <> packShow t)

buildDecoder :: JsonSchemaObject -> Either Text Dec
buildDecoder j = case type_ j of
  Just SchemaTypeObject ->
    case title j of
      Nothing -> Left "no title found, cannot build decoder"
      Just title' -> case properties j of
        Nothing -> Left "no properties found, cannot build decoder"
        Just properties' ->
          let title'' = toPascal title'
              fnName :: Text
              fnName = buildDecoderName title''
              fnType :: Type
              fnType = tparam "Decode.Decoder" (tvar title'')
              fnParams :: [Expr]
              fnParams = []
              requiredProps :: [Text]
              requiredProps = fromMaybe [] (required j)
              makeSingleDecoder propName obj = case schemaTypeToElmDecoder obj of
                Left e -> Left e
                Right v ->
                  Right
                    ( app
                        [ var "required",
                          string (toCamel propName),
                          if propName `elem` requiredProps then v else app [var "nullable", v]
                        ]
                    )
           in do
                finalName <- schemaTypeToElmName j
                decoders <-
                  foldM
                    (\prevList (propName, prop) -> (: prevList) <$> makeSingleDecoder propName prop)
                    []
                    (Map.toList properties')
                pure
                  ( decFunction
                      fnName
                      fnType
                      fnParams
                      ( foldr
                          (\newDecoder oldDecoders -> app [newDecoder, oldDecoders])
                          ( app
                              ["Decode.succeed", var finalName]
                          )
                          decoders
                      )
                  )
  Nothing -> case enum j of
    Nothing -> Left ("no type in JSON schema, and no enum; don't know how to decode: " <> packShow j)
    Just enum' -> do
      finalName <- schemaTypeToElmName j
      let fnName = "decode" <> finalName
          fnType = tparam "Decode.Decoder" (tvar finalName)
          fnParams = []
      pure
        ( decFunction
            fnName
            fnType
            fnParams
            ( app
                [var "Decode.oneOf", list ((\e -> app [var constantStringDecoderName, string e, var (toPascal e)]) <$> enum')]
            )
        )
  _ -> Left ("unknown type " <> packShow (type_ j) <> ", cannot build decoder")

constantStringDecoderName :: Text
constantStringDecoderName = "decodeConstantString"

constantStringDecoder :: Dec
constantStringDecoder =
  decFunction
    constantStringDecoderName
    (tapp [tvar "String", tvar "a", tparam "Decoder" "a"])
    [var "c", var "enumCtor"]
    ( let_
        (app [var "Decode.andThen", var "evaluate", var "Decode.string"])
        [ ( app
              [var "evaluate", var "s"],
            case_
              (op "==" (var "s") (var "c"))
              [ (bool True, app [var "Decode.succeed", var "enumCtor"]),
                (bool False, app [var "Decode.fail", string "couldn't decode"])
              ]
          )
        ]
    )

main :: IO ()
main = do
  stdin <- getContents
  case eitherDecodeStrict stdin :: Either String JsonSchemaObjectRef of
    Left e -> putStrLn ("error decoding JSON: " <> pack e)
    Right v -> do
      putStrLn "decoded:"
      pPrint v
      case fromRef v of
        Left e -> putStrLn ("error derefing: " <> e)
        Right v' -> do
          putStrLn "derefed:"
          pPrint v'
          case schemaToElm "MyModule" v' of
            Left e -> putStrLn ("error elming: " <> e)
            Right v'' -> putStrLn (render v'')
