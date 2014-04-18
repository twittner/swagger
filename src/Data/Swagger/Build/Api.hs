-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE OverloadedStrings #-}

module Data.Swagger.Build.Api where

import Control.Applicative
import Control.Monad.Trans.State.Strict
import Data.Aeson (ToJSON)
import Data.ByteString (ByteString)
import Data.Int
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Swagger.Model.Api as Api

-----------------------------------------------------------------------------
-- Primitive types

prim :: PrimType -> Primitive a
prim t = Primitive t Nothing Nothing Nothing Nothing

int32 :: Primitive Int32
int32 = prim PrimInt32

int64 :: Primitive Int64
int64 = prim PrimInt64

float :: Primitive Float
float = prim PrimFloat

double :: Primitive Double
double = prim PrimDouble

string :: Primitive String
string = prim PrimString

bytes :: Primitive ByteString
bytes = prim PrimByte

bool :: Primitive Bool
bool = prim PrimBool

date :: Primitive UTCTime
date = prim PrimDate

date_time :: Primitive UTCTime
date_time = prim PrimDateTime

def :: a -> State (Primitive a) ()
def a = modify $ \p -> p { defaultValue = Just a }

enum :: [a] -> State (Primitive a) ()
enum a = modify $ \p -> p { Api.enum = Just a }

min :: a -> State (Primitive a) ()
min a = modify $ \p -> p { minVal = Just a }

max :: a -> State (Primitive a) ()
max a = modify $ \p -> p { maxVal = Just a }

-----------------------------------------------------------------------------
-- Data types

primitive :: (Show a, ToJSON a) => Primitive a -> DataType
primitive = Prim

primitives :: (Show a, ToJSON a) => Primitive a -> Items a
primitives = PrimItems

model :: ModelId -> DataType
model = Ref

models :: ModelId -> Items ()
models = ModelItems

array :: (Show a, ToJSON a) => Items a -> DataType
array = flip Array Nothing

set :: (Show a, ToJSON a) => Items a -> DataType
set = flip Array (Just True)

-----------------------------------------------------------------------------
-- Operation

operation :: Text -> Text -> State Operation () -> Operation
operation m n s = start `with` s
  where
    start = Operation m n (Left ()) [] Nothing Nothing Nothing Nothing Nothing Nothing Nothing

returns :: DataType -> State Operation ()
returns t = modify $ \op -> op { returnType = Right t }

parameter :: ParamType -> Text -> DataType -> State Parameter () -> State Operation ()
parameter p n t s = modify $ \op -> op { parameters = start `with` s : parameters op }
  where
    start = Parameter p (Right t) n Nothing (Just True) Nothing

file :: Text -> State Parameter () -> State Operation ()
file n s = modify $ \op ->
    op { Api.consumes = Just ["multipart/form-data"]
       , parameters   = start `with` s : parameters op }
  where
    start = Parameter Form (Left File) n Nothing (Just True) Nothing

summary :: Text -> State Operation ()
summary t = modify $ \op -> op { Api.summary = Just t }

notes :: Text -> State Operation ()
notes t = modify $ \op -> op { Api.notes = Just t }

response :: Int -> Text -> State Response () -> State Operation ()
response c m s = modify $ \op -> do
    let r = start `with` s
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

response_model :: ModelId -> State Response ()
response_model m = modify $ \r -> r { responseModel = Just m }

-----------------------------------------------------------------------------
-- Parameter

optional :: State Parameter ()
optional = modify $ \p -> p { Api.required = Nothing }

description :: Text -> State Parameter ()
description d = modify $ \p -> p { Api.description = Just d }

multiple :: State Parameter ()
multiple = modify $ \p -> p { allowMultiple = Just True }

-----------------------------------------------------------------------------
-- Model

defineModel :: ModelId -> State Model () -> Model
defineModel m s = start `with` s
  where
    start = Model m [] Nothing Nothing Nothing Nothing

property :: PropertyName -> DataType -> State Property () -> State Model ()
property p t s = modify $ \m ->
    m { properties = (p, start `with` s) : properties m
      , requiredProps = maybe (Just [p]) (Just . (p:)) (requiredProps m)
      }
  where
    start = Property t Nothing

optional_property :: State Model ()
optional_property = modify $ \m -> m { requiredProps = tail <$> requiredProps m }

model_description :: Text -> State Model ()
model_description t = modify $ \m -> m { modelDescription = Just t }

subtypes :: [ModelId] -> State Model ()
subtypes tt = modify $ \m -> m { subTypes = Just tt }

discriminator :: PropertyName -> State Model ()
discriminator n = modify $ \m -> m { Api.discriminator = Just n }

-----------------------------------------------------------------------------
-- Property

property_description :: Text -> State Property ()
property_description t = modify $ \p -> p { propDescription = Just t }

-----------------------------------------------------------------------------
-- Helpers

with :: c -> State c a -> c
with = flip execState

done :: Monad m => m ()
done = return ()
