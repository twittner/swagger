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

pempty :: PrimType -> Primitive a
pempty t = Primitive t Nothing Nothing Nothing Nothing

int32 :: (Primitive Int32 -> Primitive Int32) -> DataType
int32 f = Prim . f $ pempty PrimInt32

int64 :: (Primitive Int64 -> Primitive Int64) -> DataType
int64 f = Prim . f $ pempty PrimInt64

float :: (Primitive Float -> Primitive Float) -> DataType
float f = Prim . f $ pempty PrimFloat

double :: (Primitive Double -> Primitive Double) -> DataType
double f = Prim . f $ pempty PrimDouble

string :: (Primitive String -> Primitive String) -> DataType
string f = Prim . f $ pempty PrimString

bytes :: (Primitive String -> Primitive String) -> DataType
bytes f = Prim . f $ pempty PrimByte

bool :: (Primitive Bool -> Primitive Bool) -> DataType
bool f = Prim . f $ pempty PrimBool

date :: (Primitive UTCTime -> Primitive UTCTime) -> DataType
date f = Prim . f $ pempty PrimDate

dateTime :: (Primitive UTCTime -> Primitive UTCTime) -> DataType
dateTime f = Prim . f $ pempty PrimDateTime

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

prim :: (ToJSON a, Show a) => Primitive a -> DataType
prim = Prim

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
