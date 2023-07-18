{-# LANGUAGE NamedFieldPuns #-}
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
import Data.List (length, reverse, take, zipWith)
import qualified Data.Map as Map
import Data.Maybe (Maybe (..), maybe)
import Data.Monoid (Monoid (mempty))
import Data.Semigroup ((<>))
import Data.String (String)
import Data.Text (Text, pack, singleton)
import Data.Text.IO (putStrLn)
import Data.Text.Manipulate (toCamel, toPascal)
import Data.Traversable (Traversable (traverse))
import Elm
  ( Dec,
    Expr,
    Module,
    Type,
    app,
    bool,
    case_,
    decFunction,
    decType,
    decTypeAlias,
    importEvery,
    importSome,
    import_,
    let_,
    list,
    module_,
    op,
    render,
    select,
    string,
    tapp,
    tparam,
    tparams,
    trecord,
    tuple,
    tvar,
    var,
  )
import JsonSchemaObject
  ( JsonSchemaObject,
    fromRef,
  )
import JsonSchemaObjectRef (JsonSchemaObjectRef)
import JsonSchemaProcessed (JsonSchemaProcessed (JsonSchemaProcessedArray, JsonSchemaProcessedBoolean, JsonSchemaProcessedDict, JsonSchemaProcessedEnum, JsonSchemaProcessedInt, JsonSchemaProcessedNumber, JsonSchemaProcessedObject, JsonSchemaProcessedString, JsonSchemaProcessedUnion, arrayItems, dictProperties, enumItems, enumTitle, objectDefinitions, objectProperties, objectRequired, objectTitle, unionItems), fromObject)
import System.Environment (getArgs)
import System.IO (IO)
import Utils (ErrorMessage, pShowStrict, packShow, safeHead)
import Prelude ()

buildDecoderName :: Text -> Text
buildDecoderName v = "decode" <> v

-- | Given a JSON schema, give the name of the object, or similar for non-named items (used when declaring record components on declaration)
schemaTypeToElmShallow :: JsonSchemaProcessed -> Either ErrorMessage Type
schemaTypeToElmShallow j = case j of
  JsonSchemaProcessedInt {} -> Right (tvar "Int")
  JsonSchemaProcessedString {} -> Right (tvar "String")
  JsonSchemaProcessedNumber {} -> Right (tvar "Float")
  JsonSchemaProcessedBoolean {} -> Right (tvar "Boolean")
  JsonSchemaProcessedArray {arrayItems} -> do
    subType <- schemaTypeToElmShallow arrayItems
    pure (tparam "List" subType)
  JsonSchemaProcessedObject {objectTitle} -> Right (tvar (toPascal objectTitle))
  JsonSchemaProcessedDict {dictProperties} -> do
    subType <- schemaTypeToElmShallow dictProperties
    Right (tparams "Dict" [tvar "String", subType])
  JsonSchemaProcessedUnion {unionItems} -> do
    subItems <- traverse schemaTypeToElmShallow unionItems
    pure (tparams ("Union" <> packShow (length unionItems)) subItems)
  JsonSchemaProcessedEnum {enumTitle} -> Right (tvar (toPascal enumTitle))

-- | Give the name for a JSON schema (mostly Pascal cases its title, but also returns something for the built-in types)
schemaTypeToElmName :: JsonSchemaProcessed -> Either ErrorMessage Text
schemaTypeToElmName j = case j of
  JsonSchemaProcessedInt {} -> Right "Int"
  JsonSchemaProcessedString {} -> Right "String"
  JsonSchemaProcessedNumber {} -> Right "Float"
  JsonSchemaProcessedBoolean {} -> Right "Boolean"
  JsonSchemaProcessedArray {} -> Left "array not supported in Elm naming"
  JsonSchemaProcessedDict {} -> Left "dict not supported in Elm naming"
  JsonSchemaProcessedObject {objectTitle} -> Right (toPascal objectTitle)
  JsonSchemaProcessedUnion {} -> Left "any of not supported in Elm naming"
  JsonSchemaProcessedEnum {enumTitle} -> Right (toPascal enumTitle)

-- | Declaration for the actual Elm data type for a JSON schema
schemaObjectToDeclaration :: JsonSchemaProcessed -> Either ErrorMessage Dec
schemaObjectToDeclaration j = case j of
  JsonSchemaProcessedObject {objectTitle, objectProperties, objectRequired} ->
    let propToRecordField :: Text -> JsonSchemaProcessed -> Either Text (Text, Type)
        propToRecordField propName obj = do
          v <- schemaTypeToElmShallow obj
          pure
            ( if propName `elem` objectRequired
                then (toCamel propName, v)
                else (toCamel propName, tparam "Maybe" v)
            )
     in do
          recordResults <-
            foldM
              (\prevList (propName, prop) -> (: prevList) <$> propToRecordField propName prop)
              []
              (Map.toList objectProperties)
          pure
            ( decTypeAlias
                objectTitle
                []
                (trecord recordResults)
            )
  JsonSchemaProcessedEnum {enumTitle, enumItems} ->
    let makeConstructor x = (toPascal x, [])
     in Right (decType enumTitle [] (makeConstructor <$> enumItems))
  _ -> Left ("schema entry is neither object nor enum, cannot provide declaration: " <> pShowStrict j)

unionDeclaration :: Int -> Dec
unionDeclaration n =
  decType
    ("Union" <> packShow n)
    (singleton <$> take n ['a' .. 'z'])
    (zipWith (\c i -> ("Union" <> packShow n <> "Member" <> packShow i, [tvar (singleton c)])) ['a' .. 'z'] [1 .. n])

unionDecoder :: Int -> Dec
unionDecoder n =
  let typeLetters = singleton <$> take n ['a' .. 'z']
      -- Generate "Union$x a b c ..."
      unionType = tparams ("Union" <> packShow n) (tvar <$> typeLetters)
   in decFunction
        ("decodeUnion" <> packShow n)
        ( tapp
            ( (tparam "Decode.Decoder" . tvar <$> typeLetters)
                <> [tparam "Decode.Decoder" unionType]
            )
        )
        (var <$> typeLetters)
        (app ["Decode.oneOf", list (zipWith (\typeIndex typeLetter -> app [var "Decode.map", var ("Union" <> packShow n <> "Member" <> packShow typeIndex), var typeLetter]) [1 ..] typeLetters)])

{-
Encoders for unions in general look like this:

encodeUnion2 a b c x = case x of
  Union2Member0 a -> a
  Union2Member1 b -> b
  Union2Member2 c -> c
 -}
unionEncoder :: Int -> Dec
unionEncoder n =
  let typeLetters = singleton <$> take n ['a' .. 'z']
      -- Generate "Union$x a b c ..."
      unionType = tparams ("Union" <> packShow n) (tvar <$> typeLetters)
      unionVar = var "un"
   in decFunction
        ("encodeUnion" <> packShow n)
        -- Generate "(a -> Value) -> (b -> Value) ... -> Unionx a b ... -> Encode.Value"
        -- TODO: Can we clean this so it looks like the dual of the decoder?
        ( tapp
            ( ((\t -> tapp [tvar t, tvar "Encode.Value"]) <$> typeLetters)
                <> [unionType, tvar "Encode.Value"]
            )
        )
        -- Arguments, let the sub-encoders be the same name as the encoders
        ((var <$> typeLetters) <> [unionVar])
        ( case_
            unionVar
            (zipWith (\i t -> (app [var ("Union" <> packShow n <> "Member" <> packShow i), var "x"], app [var t, var "x"])) [1 ..] typeLetters)
        )

maybeEncoder :: Dec
maybeEncoder =
  decFunction
    "encodeMaybe"
    ( tapp
        [ tapp [tvar "a", tvar "Encode.Value"],
          tparam "Maybe" (tvar "a"),
          tvar "Encode.Value"
        ]
    )
    [var "decoder", var "maybeValue"]
    (case_ (var "maybeValue") [(var "Nothing", var "Encode.null"), (app [var "Just", "x"], app [var "decoder", var "x"])])

-- | The root function: generate an Elm file from a schema
schemaToElm :: Text -> JsonSchemaObject -> Either Text Module
schemaToElm moduleName j = do
  processed <- fromObject j
  case processed of
    JsonSchemaProcessedObject {objectDefinitions} -> do
      rootDeclaration <- schemaObjectToDeclaration processed
      definitions' <- traverse schemaObjectToDeclaration (Map.elems objectDefinitions)
      definitionsDecoders <- traverse schemaTypeToDecoderDeclaration (Map.elems objectDefinitions)
      definitionsEncoders <- traverse schemaTypeToEncoder (Map.elems objectDefinitions)
      rootEncoder <- schemaTypeToEncoder processed
      rootDecoder <- schemaTypeToDecoderDeclaration processed
      let unionIndices = [1 .. 3]
          unions =
            (unionDeclaration <$> unionIndices)
              <> (unionDecoder <$> unionIndices)
              <> (unionEncoder <$> unionIndices)
          imports =
            [ import_ "Json.Decode" (Just "Decode") Nothing,
              import_ "Json.Encode" (Just "Encode") Nothing,
              import_ "Json.Decode.Pipeline" (Just "DecodePipeline") Nothing,
              import_ "Dict" Nothing (Just (importSome [select "Dict"]))
            ]
      pure
        ( module_
            moduleName
            importEvery
            imports
            (rootDeclaration : constantStringDecoder : rootDecoder : rootEncoder : maybeEncoder : (definitions' <> unions <> definitionsDecoders <> definitionsEncoders))
        )
    t -> Left ("root element is not an object: " <> pShowStrict t)

-- | Given a JSON schema, give an expression in order to decode the schema. The type should match whatever schemaTypeToDecoderDeclaration returns
schemaTypeToElmDecoderExpression :: JsonSchemaProcessed -> Either Text Expr
schemaTypeToElmDecoderExpression j = case j of
  JsonSchemaProcessedArray {arrayItems} -> do
    subDecoder <- schemaTypeToElmDecoderExpression arrayItems
    pure (app ["Decode.list", subDecoder])
  JsonSchemaProcessedEnum {enumTitle} -> pure (var (buildDecoderName (toPascal enumTitle)))
  JsonSchemaProcessedObject {objectTitle} -> pure (var (buildDecoderName (toPascal objectTitle)))
  JsonSchemaProcessedDict {dictProperties} -> do
    subDecoder <- schemaTypeToElmDecoderExpression dictProperties
    pure (app ["Decode.dict", subDecoder])
  JsonSchemaProcessedUnion {unionItems} -> do
    anyOfElements <- traverse schemaTypeToElmDecoderExpression unionItems
    pure (app (var ("decodeUnion" <> packShow (length unionItems)) : anyOfElements))
  JsonSchemaProcessedInt {} -> pure (var "Decode.int")
  JsonSchemaProcessedString {} -> pure (var "Decode.string")
  JsonSchemaProcessedNumber {} -> pure (var "Decode.float")
  JsonSchemaProcessedBoolean {} -> pure (var "Decode.bool")

-- | Given a schema, create a declaration for a decoder function for this schema
schemaTypeToDecoderDeclaration :: JsonSchemaProcessed -> Either Text Dec
schemaTypeToDecoderDeclaration j = case j of
  JsonSchemaProcessedObject {objectTitle, objectProperties, objectRequired} ->
    let title'' = toPascal objectTitle
        fnName :: Text
        fnName = buildDecoderName title''
        fnType :: Type
        fnType = tparam "Decode.Decoder" (tvar title'')
        fnParams :: [Expr]
        fnParams = []
        makeSingleDecoder propName obj = case schemaTypeToElmDecoderExpression obj of
          Left e -> Left e
          Right v ->
            Right
              ( app
                  [ var "DecodePipeline.required",
                    string (toCamel propName),
                    if propName `elem` objectRequired then v else app [var "Decode.nullable", v]
                  ]
              )
     in do
          finalName <- schemaTypeToElmName j
          decoders <-
            foldM
              (\prevList (propName, prop) -> (: prevList) <$> makeSingleDecoder propName prop)
              []
              (Map.toList objectProperties)
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
                    (reverse decoders)
                )
            )
  JsonSchemaProcessedEnum {enumItems} -> do
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
              [var "Decode.oneOf", list ((\e -> app [var constantStringDecoderName, string e, var (toPascal e)]) <$> enumItems)]
          )
      )
  _ -> Left ("cannot build decoder for " <> pShowStrict j)

constantStringDecoderName :: Text
constantStringDecoderName = "decodeConstantString"

constantStringDecoder :: Dec
constantStringDecoder =
  decFunction
    constantStringDecoderName
    (tapp [tvar "String", tvar "a", tparam "Decode.Decoder" "a"])
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

schemaTypeToEncoderShallow :: Maybe Expr -> JsonSchemaProcessed -> Either Text Expr
schemaTypeToEncoderShallow identifier j =
  case j of
    JsonSchemaProcessedArray {arrayItems} -> do
      -- Omit identifier so we can do...
      --
      -- Encode.list encodeMyStuff list
      --
      -- instead of...
      --
      -- Encode.list (encodeMyStuff list) list
      subEncoder <- schemaTypeToEncoderShallow Nothing arrayItems
      pure (app (["Encode.list", subEncoder] <> maybe [] pure identifier))
    JsonSchemaProcessedDict {dictProperties} -> do
      -- Omit identifier so we can do...
      --
      -- Encode.dict encodeMyStuff list
      --
      -- instead of...
      --
      -- Encode.dict (encodeMyStuff list) list
      subEncoder <- schemaTypeToEncoderShallow Nothing dictProperties
      pure (app (["Encode.dict", "identity", subEncoder] <> maybe [] pure identifier))
    JsonSchemaProcessedEnum {} -> do
      title' <- schemaTypeToElmName j
      pure (app ([var ("encode" <> title')] <> maybe [] pure identifier))
    JsonSchemaProcessedObject {} -> do
      title' <- schemaTypeToElmName j
      pure (app ([var ("encode" <> title')] <> maybe [] pure identifier))
    JsonSchemaProcessedUnion {unionItems} -> do
      subElements <- traverse (schemaTypeToEncoderShallow Nothing) unionItems
      pure (app ([var ("encodeUnion" <> packShow (length unionItems))] <> subElements <> maybe [] pure identifier))
    JsonSchemaProcessedInt {} -> pure (app (["Encode.int"] <> maybe [] pure identifier))
    JsonSchemaProcessedString {} -> pure (app (["Encode.string"] <> maybe [] pure identifier))
    JsonSchemaProcessedNumber {} -> pure (app (["Encode.float"] <> maybe [] pure identifier))
    JsonSchemaProcessedBoolean {} -> pure (app (["Encode.bool"] <> maybe [] pure identifier))

schemaTypeToEncoder :: JsonSchemaProcessed -> Either Text Dec
schemaTypeToEncoder j =
  case j of
    JsonSchemaProcessedEnum {enumItems} -> do
      title' <- schemaTypeToElmName j
      let inputVar = var "v"
          body = case_ inputVar ((\e -> (var (toPascal e), app [var "Encode.string", string e])) <$> enumItems)
      pure (decFunction ("encode" <> title') (tapp [tvar title', tvar "Encode.Value"]) [inputVar] body)
    JsonSchemaProcessedObject {objectRequired, objectProperties} -> do
      title' <- schemaTypeToElmName j
      let inputVarName = "v"
          transducer prevList (propName, prop) =
            let subVarName = var (inputVarName <> "." <> toCamel propName)
             in if propName `elem` objectRequired
                  then do
                    shallowEncoder <- schemaTypeToEncoderShallow (Just subVarName) prop
                    pure (tuple [string propName, shallowEncoder] : prevList)
                  else do
                    shallowEncoder <- schemaTypeToEncoderShallow Nothing prop
                    let realEncoder = app [var "encodeMaybe", shallowEncoder, subVarName]
                    pure (tuple [string propName, realEncoder] : prevList)
      -- The value (say "v") might be "Maybe a", so "Encode.int v" doesn't work.
      -- Instead, we have to do "encodeMaybe Encode.int v"
      objectArgs <-
        foldM
          transducer
          mempty
          (Map.toList objectProperties)
      let body = app ["Encode.object", list objectArgs]
      pure
        ( decFunction
            ("encode" <> title')
            (tapp [tvar title', tvar "Encode.Value"])
            [var inputVarName]
            body
        )
    _ -> Left ("type doesn't need an encoder: " <> pShowStrict j)

main :: IO ()
main = do
  args <- getArgs
  stdin <- getContents
  case eitherDecodeStrict stdin :: Either String JsonSchemaObjectRef of
    Left e -> putStrLn ("error decoding JSON: " <> pack e)
    Right v -> do
      -- putStrLn "decoded:"
      -- pPrint v
      case fromRef v of
        Left e -> putStrLn ("error derefing: " <> e)
        Right v' ->
          case safeHead args of
            Nothing -> putStrLn "please give the module name as first parameter"
            Just moduleName -> do
              -- putStrLn "derefed:"
              -- pPrint v'
              case schemaToElm (pack moduleName) v' of
                Left e -> putStrLn ("error elming: " <> e)
                Right v'' -> putStrLn (render v'')
