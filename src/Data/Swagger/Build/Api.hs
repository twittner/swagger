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
import Data.Swagger.Build.Builder
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

int32 :: Builder f (Primitive Int32) -> Builder ("type" ': f) (Primitive Int32)
int32 (Builder p) = Builder $ p { primType = PrimInt32 }

int64 :: Builder f (Primitive Int64) -> Builder ("type" ': f) (Primitive Int64)
int64 (Builder p) = Builder $ p { primType = PrimInt64 }

float :: Builder f (Primitive Float) -> Builder ("type" ': f) (Primitive Float)
float (Builder p) = Builder $ p { primType = PrimFloat }

double :: Builder f (Primitive Double) -> Builder ("type" ': f) (Primitive Double)
double (Builder p) = Builder $ p { primType = PrimDouble }

string :: Builder f (Primitive String) -> Builder ("type" ': f) (Primitive String)
string (Builder p) = Builder $ p { primType = PrimString }

bytes :: Builder f (Primitive ByteString) -> Builder ("type" ': f) (Primitive ByteString)
bytes (Builder p) = Builder $ p { primType = PrimByte }

bool :: Builder f (Primitive Bool) -> Builder ("type" ': f) (Primitive Bool)
bool (Builder p) = Builder $ p { primType = PrimBool }

date :: Builder f (Primitive UTCTime) -> Builder ("type" ': f) (Primitive UTCTime)
date (Builder p) = Builder $ p { primType = PrimDate }

dateTime :: Builder f (Primitive UTCTime) -> Builder ("type" ': f) (Primitive UTCTime)
dateTime (Builder p) = Builder $ p { primType = PrimDateTime }

def :: a -> Builder f (Primitive a) -> Builder f (Primitive a)
def a (Builder p) = Builder $ p { defaultValue = Just a }

enum :: [a] -> Builder f (Primitive a) -> Builder f (Primitive a)
enum a (Builder p) = Builder $ p { Api.enum = Just a }

min :: a -> Builder f (Primitive a) -> Builder f (Primitive a)
min a (Builder p) = Builder $ p { minVal = Just a }

max :: a -> Builder f (Primitive a) -> Builder f (Primitive a)
max a (Builder p) = Builder $ p { maxVal = Just a }

-----------------------------------------------------------------------------
-- Data types

primitive :: (Show a, ToJSON a)
          => (Builder '[] (Primitive a) -> Builder '["type"] (Primitive a))
          -> DataType
primitive f = Prim . fromBuilder . f $ p
  where
    p = Builder $ Primitive undefined Nothing Nothing Nothing Nothing

model :: Model -> DataType
model = Ref . modelId

array :: Maybe Bool -> DataType -> DataType
array u (Prim  t) = Array (PrimItems t) u
array u (Ref   t) = Array (ModelItems t :: Items ()) u
array u (Array t _) = Array t u

regular, unique :: Maybe Bool
regular = Nothing
unique  = Just True

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
