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
import Data.Maybe (Maybe (..), fromMaybe, maybe)
import Data.Monoid (Monoid (mempty))
import Data.Semigroup ((<>))
import Data.String (String)
import Data.Text (Text, pack, singleton)
import Data.Text.IO (putStrLn)
import Data.Text.Lazy (toStrict)
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
    import_,
    let_,
    list,
    module_,
    op,
    render,
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
  ( JsonSchemaObject
      ( anyOf,
        definitions,
        enum,
        items,
        properties,
        required,
        title,
        type_
      ),
    ObjectMap,
    fromRef,
  )
import JsonSchemaObjectRef (JsonSchemaObjectRef)
import JsonSchemaTypeEnum (SchemaTypeEnum (..))
import System.Environment (getArgs)
import System.IO (IO)
import Text.Pretty.Simple (pShow)
import Text.Show (Show (..))
import Prelude (Num ((+), (-)))

packShow :: Show a => a -> Text
packShow = pack . show

pShowStrict :: Show a => a -> Text
pShowStrict = toStrict . pShow

buildDecoderName :: Text -> Text
buildDecoderName v = "decode" <> v

-- | Given a JSON schema, give the name for the schema (is used when declaring records)
schemaTypeToElmShallow :: JsonSchemaObject -> Either Text Type
schemaTypeToElmShallow j = case type_ j of
  Just SchemaTypeInteger -> Right (tvar "Int")
  Just SchemaTypeString -> Right (tvar "String")
  Just SchemaTypeNumber -> Right (tvar "Float")
  Just SchemaTypeBoolean -> Right (tvar "Boolean")
  Just SchemaTypeArray ->
    case items j of
      Nothing -> Left ("list without items? " <> pShowStrict j)
      Just items' -> do
        subType <- schemaTypeToElmShallow items'
        Right (tparam "List" subType)
  Just SchemaTypeObject ->
    case title j of
      Nothing -> Left ("record without a title: " <> pShowStrict j)
      Just title' -> Right (tvar (toPascal title'))
  Nothing ->
    case title j of
      Nothing -> Left ("got something without a title, don't know how to shallow-print that: " <> pShowStrict j)
      Just title' ->
        case anyOf j of
          Nothing -> Right (tvar (toPascal title'))
          Just anyOfs -> do
            subElements <- traverse schemaTypeToElmShallow anyOfs
            pure (tparams ("Union" <> packShow (length anyOfs - 1)) subElements)

-- | Give the name for a JSON schema (mostly Pascal cases its title, but also returns something for the built-in types)
schemaTypeToElmName :: JsonSchemaObject -> Either Text Text
schemaTypeToElmName j = case type_ j of
  Just SchemaTypeInteger -> Right "Int"
  Just SchemaTypeString -> Right "String"
  Just SchemaTypeNumber -> Right "Float"
  Just SchemaTypeBoolean -> Right "Boolean"
  Just SchemaTypeArray -> Left "array not supported for elm name"
  Just SchemaTypeObject ->
    case title j of
      Nothing -> Left ("record without a title: " <> pShowStrict j)
      Just title' -> Right (toPascal title')
  Nothing ->
    case title j of
      Nothing -> Left ("got something without a title, don't know how to shallow-print that: " <> pShowStrict j)
      Just title' ->
        case anyOf j of
          Nothing -> Right (toPascal title')
          Just _ -> do
            Left "anyOf decoding not supported for elm name"

-- | Declaration for the actual Elm data type for a JSON schema
schemaObjectToDeclaration :: JsonSchemaObject -> Either Text Dec
schemaObjectToDeclaration j = case type_ j of
  Just SchemaTypeObject ->
    case title j of
      Nothing -> Left ("record without a title: " <> pShowStrict j)
      Just title' ->
        case properties j of
          Nothing -> Left ("record " <> title' <> " without properties: " <> pShowStrict j)
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
      Nothing -> Left ("schema entry without a title " <> pShowStrict x)
      Just title' -> Left ("schema entry " <> title' <> " is neither object nor enum")
  Nothing ->
    case title j of
      Nothing -> Left ("schema entry without a title " <> pShowStrict j)
      Just title' ->
        case enum j of
          Just enum' ->
            let makeConstructor x = (toPascal x, [])
             in Right (decType title' [] (makeConstructor <$> enum'))
          Nothing -> Left ("schema entry " <> title' <> " is neither object nor enum: " <> pShowStrict j)

unionDeclaration :: Int -> Dec
unionDeclaration n =
  decType
    ("Union" <> packShow n)
    (singleton <$> take (n + 1) ['a' .. 'z'])
    (zipWith (\c i -> ("Union" <> packShow n <> "Member" <> packShow i, [tvar (singleton c)])) ['a' .. 'z'] [0 .. n])

unionDecoder :: Int -> Dec
unionDecoder n =
  let typeLetters = singleton <$> take (n + 1) ['a' .. 'z']
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
        (app ["Decode.oneOf", list (zipWith (\typeIndex typeLetter -> app [var "Decode.map", var ("Union" <> packShow n <> "Member" <> packShow typeIndex), var typeLetter]) [0 ..] typeLetters)])

{-
Encoders for unions in general look like this:

encodeUnion2 a b c x = case x of
  Union2Member0 a -> a
  Union2Member1 b -> b
  Union2Member2 c -> c
 -}
unionEncoder :: Int -> Dec
unionEncoder n =
  let typeLetters = singleton <$> take (n + 1) ['a' .. 'z']
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
            (zipWith (\i t -> (app [var ("Union" <> packShow n <> "Member" <> packShow i), var "x"], app [var t, var "x"])) [0 ..] typeLetters)
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
schemaToElm moduleName j = case type_ j of
  Just SchemaTypeObject -> do
    rootDeclaration <- schemaObjectToDeclaration j
    definitions' <- traverse schemaObjectToDeclaration (Map.elems (definitions j))
    definitionsDecoders <- traverse schemaTypeToDecoderDeclaration (Map.elems (definitions j))
    definitionsEncoders <- traverse schemaTypeToEncoder (Map.elems (definitions j))
    rootEncoder <- schemaTypeToEncoder j
    rootDecoder <- schemaTypeToDecoderDeclaration j
    let unionIndices = [0 .. 2]
        unions =
          (unionDeclaration <$> unionIndices)
            <> (unionDecoder <$> unionIndices)
            <> (unionEncoder <$> unionIndices)
        imports =
          [ import_ "Json.Decode" (Just "Decode") Nothing,
            import_ "Json.Encode" (Just "Encode") Nothing,
            import_ "Json.Decode.Pipeline" (Just "DecodePipeline") Nothing
          ]
    pure
      ( module_
          moduleName
          importEvery
          imports
          (rootDeclaration : constantStringDecoder : rootDecoder : rootEncoder : maybeEncoder : (definitions' <> unions <> definitionsDecoders <> definitionsEncoders))
      )
  t -> Left ("unknown type " <> pShowStrict t)

-- | Given a JSON schema, give an expression in order to decode the schema. The type should match whatever schemaTypeToDecoderDeclaration returns
schemaTypeToElmDecoderExpression :: JsonSchemaObject -> Either Text Expr
schemaTypeToElmDecoderExpression j = case type_ j of
  Just SchemaTypeInteger -> Right (var "Decode.int")
  Just SchemaTypeNumber -> Right (var "Decode.float")
  Just SchemaTypeString -> Right (var "Decode.string")
  Just SchemaTypeArray -> do
    case items j of
      Nothing -> Left ("array without items? " <> pShowStrict j)
      Just items' -> do
        subDecoder <- schemaTypeToElmDecoderExpression items'
        pure (app ["Decode.list", subDecoder])
  _ ->
    case anyOf j of
      Just anyOf' -> do
        anyOfElements <- traverse schemaTypeToElmDecoderExpression anyOf'
        pure (app (var ("decodeUnion" <> packShow (length anyOf' - 1)) : anyOfElements))
      Nothing ->
        case title j of
          Nothing -> Left ("unknown type to name decoder for (no title): " <> pShowStrict j)
          Just title' -> Right (var (buildDecoderName (toPascal title')))

-- | Given a schema, create a declaration for a decoder function for this schema
schemaTypeToDecoderDeclaration :: JsonSchemaObject -> Either Text Dec
schemaTypeToDecoderDeclaration j = case type_ j of
  Just SchemaTypeObject ->
    case title j of
      Nothing -> Left ("no title found, cannot build decoder for " <> pShowStrict j)
      Just title' -> case properties j of
        Nothing -> Left ("no properties found, cannot build decoder for " <> pShowStrict j)
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
              makeSingleDecoder propName obj = case schemaTypeToElmDecoderExpression obj of
                Left e -> Left e
                Right v ->
                  Right
                    ( app
                        [ var "DecodePipeline.required",
                          string (toCamel propName),
                          if propName `elem` requiredProps then v else app [var "Decode.nullable", v]
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
                          (reverse decoders)
                      )
                  )
  Nothing -> case enum j of
    Nothing -> case anyOf j of
      Nothing -> Left ("no type in JSON schema, and no enum; don't know how to decode: " <> pShowStrict j)
      Just anyOf' -> Left "don't know how to decode anyof"
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
  _ -> Left ("unknown type, cannot build decoder for " <> pShowStrict j)

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

safeHead :: [a] -> Maybe a
safeHead l = case l of
  (x : _) -> Just x
  _ -> Nothing

requireProps :: JsonSchemaObject -> Either Text ObjectMap
requireProps j = case properties j of
  Nothing -> Left ("need properties in object: " <> pShowStrict j)
  Just props' -> pure props'

-- TODO: Add getters along the lines of "getArray" which returns both items and the other necessary props
requireItems :: JsonSchemaObject -> Either Text JsonSchemaObject
requireItems j = case items j of
  Nothing -> Left ("need items in array: " <> pShowStrict j)
  Just items' -> pure items'

schemaTypeToEncoderShallow :: Maybe Expr -> JsonSchemaObject -> Either Text Expr
schemaTypeToEncoderShallow identifier j =
  case type_ j of
    Nothing -> case enum j of
      Nothing ->
        case anyOf j of
          Nothing -> Left ("cannot create (shallow) encoder, no type, not an enum, not anyOf, for " <> pShowStrict j)
          Just anyOf' -> do
            subElements <- traverse (schemaTypeToEncoderShallow Nothing) anyOf'
            pure (app ([var ("encodeUnion" <> packShow (length anyOf' - 1))] <> subElements <> maybe [] pure identifier))
      Just _ -> do
        title' <- schemaTypeToElmName j
        pure (app ([var ("encode" <> title')] <> maybe [] pure identifier))
    Just SchemaTypeInteger -> pure (app (["Encode.int"] <> maybe [] pure identifier))
    Just SchemaTypeString -> pure (app (["Encode.string"] <> maybe [] pure identifier))
    Just SchemaTypeNumber -> pure (app (["Encode.float"] <> maybe [] pure identifier))
    Just SchemaTypeArray -> do
      items' <- requireItems j
      -- Omit identifier so we can do...
      --
      -- Encode.list encodeMyStuff list
      --
      -- instead of...
      --
      -- Encode.list (encodeMyStuff list) list
      subEncoder <- schemaTypeToEncoderShallow Nothing items'
      pure (app (["Encode.list", subEncoder] <> maybe [] pure identifier))
    Just SchemaTypeObject -> do
      title' <- schemaTypeToElmName j
      pure (app ([var ("encode" <> title')] <> maybe [] pure identifier))
    _ -> Left ("cannot create (shallow) encoder for " <> pShowStrict j)

schemaTypeToEncoder :: JsonSchemaObject -> Either Text Dec
schemaTypeToEncoder j =
  case type_ j of
    Nothing -> case enum j of
      Nothing -> Left ("cannot create encoder for " <> pShowStrict j)
      Just enum' -> do
        title' <- schemaTypeToElmName j
        let inputVar = var "v"
            body = case_ inputVar ((\e -> (var (toPascal e), app [var "Encode.string", string e])) <$> enum')
        pure (decFunction ("encode" <> title') (tapp [tvar title', tvar "Encode.Value"]) [inputVar] body)
    Just SchemaTypeObject -> do
      title' <- schemaTypeToElmName j
      props <- requireProps j
      let inputVarName = "v"
          transducer prevList (propName, prop) =
            let subVarName = var (inputVarName <> "." <> toCamel propName)
             in if propName `elem` fromMaybe [] (required j)
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
          (Map.toList props)
      let body = app ["Encode.object", list objectArgs]
      pure
        ( decFunction
            ("encode" <> title')
            (tapp [tvar title', tvar "Encode.Value"])
            [var inputVarName]
            body
        )
    Just _ -> Left ("type " <> packShow (type_ j) <> " doesn't need an encoder")

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
