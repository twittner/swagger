-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

-- | Construction of "Data.Swagger.Model.Api" values. For example:
--
-- @
-- declare "http://petstore.swagger.wordnik.com/api" "1.2" $ do
--    apiVersion \"1.0.0\"
--    resourcePath \"/store\"
--    model foo
--    model bar
--    produces \"application/json\"
--    produces \"text/html\"
--    produces \"text/plain\"
--    api \"\/store\/order\/{orderId}\" $ do
--        operation \"GET\" \"foo\" $ do
--            summary \"give me some foo\"
--            notes   \"but only the good one\"
--            returns (ref foo)
--            parameter Header \"type\" (string $ enum [\"bar\", \"baz\"]) $ do
--                description \"specifies the type of foo\"
--                optional
--            parameter Query \"format\" (string $ enum [\"plain\", \"html\"]) $
--                description \"output format\"
--            parameter Query \"size\" (int32 $ min 1 . max 100 . def 10) $
--                description \"amount of foo\"
--            produces \"application/json\"
--            produces \"text/html\"
--            response 200 \"OK\" (model foo)
--            response 400 \"Bad Request\" end
--        operation \"POST\" \"foo\" $ do
--            summary \"something else\"
--            deprecated
-- @
module Data.Swagger.Build.Api
    ( -- * data types
      -- ** Re-exports
      Api.ApiDecl
    , Api.API
    , Api.Operation
    , Api.Parameter
    , Api.ParamType (..)
    , Api.Response
    , Api.Model
    , Api.Property
    , Api.DataType
    , Api.Primitive
    , Api.Items

      -- ** primitive construction
    , int32
    , int32'
    , int64
    , int64'
    , float
    , float'
    , bool
    , bool'
    , double
    , double'
    , string
    , string'
    , bytes
    , bytes'
    , date
    , date'
    , dateTime
    , dateTime'

      -- ** primitive modifiers
    , Data.Swagger.Build.Api.def
    , Data.Swagger.Build.Api.enum
    , Data.Swagger.Build.Api.min
    , Data.Swagger.Build.Api.max

      -- ** data-type constructors
    , ref
    , array
    , unique

      -- * builder types
    , ApiDeclSt
    , ApiDeclBuilder
    , ApiSt
    , ApiBuilder
    , OperationSt
    , OperationBuilder
    , ParameterSt
    , ParameterBuilder
    , ResponseSt
    , ResponseBuilder
    , ModelSt
    , ModelBuilder
    , PropertySt
    , PropertyBuilder

      -- * API declaration
    , declare
    , Data.Swagger.Build.Api.apiVersion
    , Data.Swagger.Build.Api.resourcePath
    , api
    , model

      -- * operation
    , operation
    , returns
    , parameter
    , file
    , body
    , Data.Swagger.Build.Api.summary
    , Data.Swagger.Build.Api.notes
    , response
    , Data.Swagger.Build.Util.produces
    , authorisation
    , Data.Swagger.Build.Util.Auth (..)

      -- * parameter
    , multiple

      -- * model
    , defineModel
    , property
    , children

      -- * various
    , Data.Swagger.Build.Util.description
    , optional
    , Data.Swagger.Build.Util.consumes
    , Data.Swagger.Build.Api.deprecated
    , Data.Swagger.Build.Util.end
    ) where

import Control.Applicative hiding (optional)
import Control.Monad.Trans.State.Strict
import Data.Function (on)
import Data.Int
import Data.List (groupBy)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Swagger.Build.Util
import Data.Swagger.Model.Api as Api
import Data.Swagger.Model.Authorisation (Scope)
import Prelude

-----------------------------------------------------------------------------
-- Primitive types

prim :: PrimType -> Primitive a
prim t = Primitive t Nothing Nothing Nothing Nothing

int32 :: (Primitive Int32 -> Primitive Int32) -> DataType
int32 f = Prim . f $ prim PrimInt32

int64 :: (Primitive Int64 -> Primitive Int64) -> DataType
int64 f = Prim . f $ prim PrimInt64

float :: (Primitive Float -> Primitive Float) -> DataType
float f = Prim . f $ prim PrimFloat

double :: (Primitive Double -> Primitive Double) -> DataType
double f = Prim . f $ prim PrimDouble

string :: (Primitive String -> Primitive String) -> DataType
string f = Prim . f $ prim PrimString

bytes :: (Primitive String -> Primitive String) -> DataType
bytes f = Prim . f $ prim PrimByte

bool :: (Primitive Bool -> Primitive Bool) -> DataType
bool f = Prim . f $ prim PrimBool

date :: (Primitive UTCTime -> Primitive UTCTime) -> DataType
date f = Prim . f $ prim PrimDate

dateTime :: (Primitive UTCTime -> Primitive UTCTime) -> DataType
dateTime f = Prim . f $ prim PrimDateTime

int32' :: DataType
int32' = int32 id

int64' :: DataType
int64' = int64 id

float' :: DataType
float' = float id

double' :: DataType
double' = double id

string' :: DataType
string' = string id

bytes' :: DataType
bytes' = bytes id

bool' :: DataType
bool' = bool id

date' :: DataType
date' = date id

dateTime' :: DataType
dateTime' = dateTime id

-- | Default value of some primitive type.
def :: a -> Primitive a -> Primitive a
def a t = t { defaultValue = Just a }

-- | Enumerate valid values of some primitive type.
enum :: [a] -> Primitive a -> Primitive a
enum a t = t { Api.enum = Just a }

-- | Minimum value of some primitive type.
min :: a -> Primitive a -> Primitive a
min a t = t { minVal = Just a }

-- | Maximum value of some primitive type.
max :: a -> Primitive a -> Primitive a
max a t = t { maxVal = Just a }

-----------------------------------------------------------------------------
-- Data types

ref :: Model -> DataType
ref = Ref . modelId

array :: DataType -> DataType
array (Prim  t) = Array (PrimItems t) Nothing
array (Ref   t) = Array (ModelItems t :: Items ()) Nothing
array t@(Array _ _) = t

-- | Specify that array elements are unique.
unique :: DataType -> DataType
unique (Array t _) = Array t (Just True)
unique t           = t

-----------------------------------------------------------------------------
-- Api Decl

type ApiDeclSt = Common '["produces", "consumes", "models", "authorisations"] ApiDecl
type ApiDeclBuilder = State ApiDeclSt ()

-- | Create an API declaration given a base URL, a swagger version, and
-- other API declaration values.
declare :: Text -> Text -> ApiDeclBuilder -> ApiDecl
declare b v s = value $ execState s start
  where
    start   = common $ ApiDecl v b [] Nothing Nothing Nothing Nothing Nothing Nothing
    mmmm  c = map (\m -> (modelId m, m)) <$> modls c
    value c = (other c) { apiProduces       = prod c
                        , apiConsumes       = cons c
                        , models            = mmmm c
                        , apiAuthorisations = toAuthObj <$> auths c
                        }

apiVersion :: Text -> ApiDeclBuilder
apiVersion v = modify $ \c -> c { other = (other c) { Api.apiVersion = Just v } }

resourcePath :: Text -> ApiDeclBuilder
resourcePath p = modify $ \c -> c { other = (other c) { Api.resourcePath = Just p } }

-----------------------------------------------------------------------------
-- API

type ApiSt = Common '["description"] API
type ApiBuilder = State ApiSt ()

-- | Add one API object to an API declaration given some path and other API
-- object values.
api :: Text -> ApiBuilder -> ApiDeclBuilder
api p s = modify $ \c -> do
    let d = other c
    c { other = d { apis = value (execState s start) : apis d } }
  where
    start   = common $ API p [] Nothing
    value c = (other c) { apiDescription = descr c }

type OperationSt = Common '["produces", "consumes", "authorisations"] Operation
type OperationBuilder = State OperationSt ()

-- | Add one operation object to an API object given an HTTP method,
-- a nickname and other operation specific values.
operation :: Text -> Text -> OperationBuilder -> ApiBuilder
operation m n s = modify $ \c -> do
    let o = value (execState s start)
        a = other c
    c { other = a { operations = o : operations a } }
  where
    start   = common $ Operation m n (Left ()) [] Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    value c = (other c) { Api.produces   = prod c
                        , Api.consumes   = cons c
                        , authorisations = toAuthObj <$> auths c
                        }

-----------------------------------------------------------------------------
-- Operation

type ParameterSt = Common '["description", "required"] Parameter
type ParameterBuilder = State ParameterSt ()

returns :: DataType -> OperationBuilder
returns t = modify $ \c -> c { other = (other c) { returnType = Right t } }

-- | Add one parameter object to an operation object given the 'ParamType',
-- the parameter name and the actual data-type plus some other parameter
-- values.
parameter :: ParamType -> Text -> DataType -> ParameterBuilder -> OperationBuilder
parameter p n t s = modify $ \c -> do
    let op = other c
    c { other = op { parameters = value (execState s start) : parameters op } }
  where
    start   = common $ Parameter p (Right t) n Nothing Nothing Nothing
    value c = (other c) { Api.description = descr c, Api.required = reqrd c }

-- | Like 'parameter' but specific for file uploads.
file :: Text -> ParameterBuilder -> OperationBuilder
file n s = modify $ \c -> do
    let op = other c
    c { other = op { Api.consumes = Just ["multipart/form-data"]
                   , parameters   = value (execState s start) : parameters op
                   }
      }
  where
    start   = common $ Parameter Form (Left File) n Nothing Nothing Nothing
    value c = (other c) { Api.description = descr c, Api.required = reqrd c }

-- | Like 'parameter' but specific for request body parameters. Sets
-- 'ParamType' to 'Body' and uses as name \"body\" which is the only valid
-- name for request bodies.
body :: DataType -> ParameterBuilder -> OperationBuilder
body = parameter Body "body"

summary :: Text -> OperationBuilder
summary t = modify $ \c -> c { other = (other c) { Api.summary = Just t } }

notes :: Text -> OperationBuilder
notes t = modify $ \c -> c { other = (other c) { Api.notes = Just t } }

type ResponseSt = Common '["models"] Response
type ResponseBuilder = State ResponseSt ()

-- | Add one response message object to an operation given a status code
-- and some message plus response message specific values.
response :: Int -> Text -> ResponseBuilder -> OperationBuilder
response i m s = modify $ \x -> do
    let r = value $ execState s start
        o = other x
    x { other = o { responses = maybe (Just [r]) (Just . (r:)) (responses o) } }
  where
    start   = common $ Response i m Nothing
    value c = (other c) { responseModel = modelId . head <$> modls c }

deprecated :: OperationBuilder
deprecated = modify $ \c -> c { other = (other c) { Api.deprecated = Just True } }

-----------------------------------------------------------------------------
-- Parameter

multiple :: ParameterBuilder
multiple = modify $ \c -> c { other = (other c) { allowMultiple = Just True } }

-----------------------------------------------------------------------------
-- Model

type ModelSt = Common '["description"] Model
type ModelBuilder = State ModelSt ()

type PropertySt = Common '["description", "required"] Property
type PropertyBuilder = State PropertySt ()

-- | Construct a complex data-type (aka \"Model\") given some identifier
-- and model-specific values.
defineModel :: ModelId -> ModelBuilder -> Model
defineModel m s = value (execState s start)
  where
    start   = common $ Model m [] Nothing Nothing Nothing Nothing
    value c = (other c) { modelDescription = descr c }

-- | Add a property to a model given a name, type and other propertu
-- values.
property :: PropertyName -> DataType -> PropertyBuilder -> ModelBuilder
property n t s = modify $ \c -> do
    let r = execState s $ common (Property t Nothing)
        p = (other r) { propDescription = descr r }
        m = other c
        x = maybe (Just [n]) (Just . (n:)) (requiredProps m)
        y = if Just True /= reqrd r then requiredProps m else x
    c { other = m { properties = (n, p) : properties m , requiredProps = y } }

-- | Specify a sub-typing relationship for a model by given
-- a \"discriminator\" property name and all sub-types.
children :: PropertyName -> [Model] -> ModelBuilder
children d tt = modify $ \c -> c { other = (other c) { subTypes = Just tt, discriminator = Just d } }

-----------------------------------------------------------------------------
-- Helpers

toAuthObj :: [(Text, Maybe Scope)] -> [(Text, [Scope])]
toAuthObj = map (\g -> (fst (head g), catMaybes $ map snd g)) . groupBy ((==) `on` fst)

