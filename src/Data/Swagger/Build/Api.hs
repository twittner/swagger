-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Data.Swagger.Build.Api where

import Control.Monad.Trans.State.Strict
import Data.Aeson (ToJSON)
import Data.ByteString (ByteString)
import Data.Int
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Swagger.Model.Api as Api
import Data.Vinyl

-----------------------------------------------------------------------------
-- Fields occuring in multiple locations

descriptionF :: "description" ::: Maybe Text
descriptionF = Field

requiredF :: "required" ::: Maybe Bool
requiredF = Field

description :: IElem ("description" ::: Maybe Text) f => Text -> State (PlainRec f) ()
description d = modify (descriptionF `rPut` Just d)

required :: IElem ("required" ::: Maybe Bool) f => State (PlainRec f) ()
required = modify (requiredF `rPut` Just True)

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

dateTime :: Primitive UTCTime
dateTime = prim PrimDateTime

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

model :: Model -> DataType
model = Ref . modelId

models :: Model -> Items ()
models = ModelItems . modelId

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

parameter :: ParamType -> Text -> DataType -> State (PlainRec P) () -> State Operation ()
parameter p n t s = modify $ \op ->
    op { parameters = value (start `with` s) : parameters op }
  where
    start   = mkP $ Parameter p (Right t) n Nothing Nothing Nothing
    value r = let ds = descriptionF `rGet` r
                  rq = requiredF `rGet` r
                  p' = paramF `rGet` r
              in p' { Api.description = ds, Api.required = rq }

file :: Text -> State (PlainRec P) () -> State Operation ()
file n s = modify $ \op ->
    op { Api.consumes = Just ["multipart/form-data"]
       , parameters   = value (start `with` s) : parameters op
       }
  where
    start   = mkP $ Parameter Form (Left File) n Nothing Nothing Nothing
    value r = let ds = descriptionF `rGet` r
                  rq = requiredF `rGet` r
                  p' = paramF `rGet` r
              in p' { Api.description = ds, Api.required = rq }

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

responseModel :: Model -> State Response ()
responseModel m = modify $ \r -> r { Api.responseModel = Just (modelId m) }

-----------------------------------------------------------------------------
-- Parameter

type P = [ "description" ::: Maybe Text
         , "required"    ::: Maybe Bool
         , "parameter"   ::: Parameter
         ]

paramF :: "parameter" ::: Parameter
paramF = Field

mkP :: Parameter -> PlainRec P
mkP p = descriptionF =: Nothing <+> requiredF =: Nothing <+> paramF =: p

multiple :: State (PlainRec P) ()
multiple = modify (rMod paramF (\p -> p { allowMultiple = Just True }))

-----------------------------------------------------------------------------
-- Model

type M = [ "description" ::: Maybe Text, "model" ::: Model ]

mkM :: Model -> PlainRec M
mkM m = descriptionF =: Nothing <+> modelF =: m

modelF :: "model" ::: Model
modelF = Field

defineModel :: ModelId -> State (PlainRec M) () -> Model
defineModel m s = value (start `with` s)
  where
    start   = mkM $ Model m [] Nothing Nothing Nothing Nothing
    value r = (modelF `rGet` r) { modelDescription = descriptionF `rGet` r}

type PR = [ "description" ::: Maybe Text , "required" ::: Maybe Bool ]

mkPR :: PlainRec PR
mkPR = descriptionF =: Nothing <+> requiredF =: Nothing

property :: PropertyName -> DataType -> State (PlainRec PR) () -> State (PlainRec M) ()
property n t s = modify $ rMod modelF $ \m -> do
    let r = mkPR `with` s
        p = Property t (descriptionF `rGet` r)
    m { properties    = (n, p) : properties m
      , requiredProps = if Just True /= requiredF `rGet` r
                            then requiredProps m
                            else maybe (Just [n]) (Just . (n:)) (requiredProps m)
      }

subtypes :: [ModelId] -> State (PlainRec M) ()
subtypes tt = modify (rMod modelF $ \m -> m { subTypes = Just tt })

discriminator :: PropertyName -> State (PlainRec M) ()
discriminator n = modify (rMod modelF $ \m -> m { Api.discriminator = Just n })

-----------------------------------------------------------------------------
-- Helpers

with :: c -> State c a -> c
with = flip execState

done :: Monad m => m ()
done = return ()
