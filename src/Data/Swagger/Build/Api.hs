-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Data.Swagger.Build.Api where

import Control.Monad.Trans.State.Strict
import Data.Int
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Swagger.Build.Util
import Data.Swagger.Model.Api as Api

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

def :: a -> Primitive a -> Primitive a
def a t = t { defaultValue = Just a }

enum :: [a] -> Primitive a -> Primitive a
enum a t = t { Api.enum = Just a }

min :: a -> Primitive a -> Primitive a
min a t = t { minVal = Just a }

max :: a -> Primitive a -> Primitive a
max a t = t { maxVal = Just a }

-----------------------------------------------------------------------------
-- Data types

model :: Model -> DataType
model = Ref . modelId

array :: DataType -> DataType
array (Prim  t) = Array (PrimItems t) Nothing
array (Ref   t) = Array (ModelItems t :: Items ()) Nothing
array t@(Array _ _) = t

unique :: DataType -> DataType
unique (Array t _) = Array t (Just True)
unique t           = t

-----------------------------------------------------------------------------
-- Fields occuring in multiple locations

data Common f a = Common
    { descr :: Maybe Text
    , reqrd :: Maybe Bool
    , other :: a
    }

common :: a -> Common f a
common = Common Nothing Nothing

description :: Elem "description" f => Text -> State (Common f a) ()
description d = modify $ \c -> c { descr = Just d }

required :: Elem "required" f => State (Common f a) ()
required = modify $ \c -> c { reqrd = Just True }

-----------------------------------------------------------------------------
-- Operation

type ParameterSt = Common '["description", "required"] Parameter

operation :: Text -> Text -> State Operation () -> Operation
operation m n s = execState s start
  where
    start = Operation m n (Left ()) [] Nothing Nothing Nothing Nothing Nothing Nothing Nothing

returns :: DataType -> State Operation ()
returns t = modify $ \op -> op { returnType = Right t }

parameter :: ParamType -> Text -> DataType -> State ParameterSt () -> State Operation ()
parameter p n t s = modify $ \op ->
    op { parameters = value (execState s start) : parameters op }
  where
    start   = common $ Parameter p (Right t) n Nothing Nothing Nothing
    value c = (other c) { Api.description = descr c, Api.required = reqrd c }

file :: Text -> State ParameterSt () -> State Operation ()
file n s = modify $ \op ->
    op { Api.consumes = Just ["multipart/form-data"]
       , parameters   = value (execState s start) : parameters op
       }
  where
    start   = common $ Parameter Form (Left File) n Nothing Nothing Nothing
    value c = (other c) { Api.description = descr c, Api.required = reqrd c }

summary :: Text -> State Operation ()
summary t = modify $ \op -> op { Api.summary = Just t }

notes :: Text -> State Operation ()
notes t = modify $ \op -> op { Api.notes = Just t }

response :: Int -> Text -> State Response () -> State Operation ()
response c m s = modify $ \op -> do
    let r = execState s start
    op { responses = maybe (Just [r]) (Just . (r:)) (responses op) }
  where
    start = Response c m Nothing

produces :: Text -> State Operation ()
produces t = modify $ \op ->
    op { Api.produces = maybe (Just [t]) (Just . (t:)) (Api.produces op) }

consumes :: Text -> State Operation ()
consumes t = modify $ \op ->
    op { Api.consumes = maybe (Just [t]) (Just . (t:)) (Api.consumes op) }

deprecated :: State Operation ()
deprecated = modify $ \op -> op { Api.deprecated = Just True }

-----------------------------------------------------------------------------
-- Response

responseModel :: Model -> State Response ()
responseModel m = modify $ \r -> r { Api.responseModel = Just (modelId m) }

-----------------------------------------------------------------------------
-- Parameter

multiple :: State ParameterSt ()
multiple = modify $ \c -> c { other = (other c) { allowMultiple = Just True } }

-----------------------------------------------------------------------------
-- Model

type ModelSt    = Common '["description"] Model
type PropertySt = Common '["description", "required"] Property

defineModel :: ModelId -> State ModelSt () -> Model
defineModel m s = value (execState s start)
  where
    start   = common $ Model m [] Nothing Nothing Nothing Nothing
    value c = (other c) { modelDescription = descr c }

property :: PropertyName -> DataType -> State PropertySt () -> State ModelSt ()
property n t s = modify $ \c -> do
    let r = execState s $ common (Property t Nothing)
        p = other r
        m = other c
        x = maybe (Just [n]) (Just . (n:)) (requiredProps m)
        y = if Just True /= reqrd r then requiredProps m else x
    c { other = m { properties = (n, p) : properties m , requiredProps = y } }

subtypes :: [ModelId] -> State ModelSt ()
subtypes tt = modify $ \c -> c { other = (other c) { subTypes = Just tt } }

discriminator :: PropertyName -> State ModelSt ()
discriminator n = modify $ \c -> c { other = (other c) { Api.discriminator = Just n } }

-----------------------------------------------------------------------------
-- Helpers

done :: Monad m => m ()
done = return ()
