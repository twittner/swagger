module Data.Swagger.Build
    ( module B
    , datatype
    , model
    , file
      -- * Arrays and Sets
    , array
    , set
    , complexArray
    , complexSet
    ) where

import Data.Aeson hiding (Array)
import Data.Swagger.Build.Prim as B
import Data.Swagger.Model.Api
import Data.Swagger.Model.Util

file :: File
file = File

------------------------------------------------------------------------------
-- DataType

model :: ModelId -> DataType
model = Ref

datatype :: (Show a, ToJSON a) => (Primitive a -> Complete (Primitive a)) -> DataType
datatype f = Prim . get . f $ prim

array :: (Show a, ToJSON a) => (Primitive a -> Complete (Primitive a)) -> DataType
array f = flip Array Nothing $ PrimItems . get . f $ prim

set :: (Show a, ToJSON a) => (Primitive a -> Complete (Primitive a)) -> DataType
set f = flip Array (Just True) $ PrimItems . get . f $ prim

complexArray :: ModelId -> DataType
complexArray m = Array (ModelItems m :: Items ()) Nothing

complexSet :: ModelId -> DataType
complexSet m = Array (ModelItems m :: Items ()) (Just True)

prim :: Primitive a
prim = Primitive PrimInt32 Nothing Nothing Nothing Nothing

