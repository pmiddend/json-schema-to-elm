{-# LANGUAGE OverloadedStrings #-}

module ElmConstants where

import Data.Text (Text)
import Elm (Expr, Type, app, tparam, tparams, tvar, var)

intTypeString :: Text
intTypeString = "Int"

dictTypeString :: Text
dictTypeString = "Dict"

stringTypeString :: Text
stringTypeString = "String"

floatTypeString :: Text
floatTypeString = "Float"

booleanTypeString :: Text
booleanTypeString = "Boolean"

intTypeVar :: Type
intTypeVar = tvar intTypeString

stringTypeVar :: Type
stringTypeVar = tvar stringTypeString

floatTypeVar :: Type
floatTypeVar = tvar floatTypeString

booleanTypeVar :: Type
booleanTypeVar = tvar booleanTypeString

listTypeVar :: Type -> Type
listTypeVar = tparam "List"

maybeTypeVar :: Type -> Type
maybeTypeVar = tparam "Maybe"

dictTypeVar :: Type -> Type -> Type
dictTypeVar a b = tparams dictTypeString [a, b]

nothingVar :: Expr
nothingVar = var "Nothing"

justVar :: Expr -> Expr
justVar a = app [var "Just", a]

identityVar :: Expr
identityVar = var "identity"
