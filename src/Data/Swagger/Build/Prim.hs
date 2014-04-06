module Data.Swagger.Build.Prim
    ( -- * primitives
      int32
    , int64
    , float
    , double
    , string
    , bytes
    , date
    , dateTime

      -- * modifiers
    , def
    , enum
    , min
    , max
    ) where

import Data.ByteString (ByteString)
import Data.Int
import Data.Time (UTCTime)
import Prelude hiding (min, max)
import Data.Swagger.Model.Api hiding (enum)
import Data.Swagger.Model.Util

import qualified Data.Swagger.Model.Api as Api

int32 :: Primitive Int32 -> Complete (Primitive Int32)
int32 p = Complete $ p { primType = PrimInt32 }

int64 :: Primitive Int64 -> Complete (Primitive Int64)
int64 p = Complete $ p { primType = PrimInt64 }

float :: Primitive Float -> Complete (Primitive Float)
float p = Complete $ p { primType = PrimFloat }

double :: Primitive Double -> Complete (Primitive Double)
double p = Complete $ p { primType = PrimDouble }

string :: Primitive String -> Complete (Primitive String)
string p = Complete $ p { primType = PrimString }

bytes :: Primitive ByteString -> Complete (Primitive ByteString)
bytes p = Complete $ p { primType = PrimByte }

date :: Primitive UTCTime -> Complete (Primitive UTCTime)
date p = Complete $ p { primType = PrimDate }

dateTime :: Primitive UTCTime -> Complete (Primitive UTCTime)
dateTime p = Complete $ p { primType = PrimDateTime }

def :: a -> Primitive a -> Primitive a
def d p = p { defaultValue = Just d }

enum :: [a] -> Primitive a -> Primitive a
enum e p = p { Api.enum = Just e }

min :: a -> Primitive a -> Primitive a
min x p = p { minVal = Just x }

max :: a -> Primitive a -> Primitive a
max x p = p { maxVal = Just x }

