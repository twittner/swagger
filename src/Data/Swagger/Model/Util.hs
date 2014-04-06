-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Data.Swagger.Model.Util where

import Data.Aeson hiding (Array)
import Data.Aeson.Types (Pair)
import Data.Text (Text)

fromPairs :: ToJSON a => [(Text, a)] -> Value
fromPairs = object . map (\p -> fst p .= toJSON (snd p))

infixr 5 #

(#) :: Pair -> [Pair] -> [Pair]
(_, Null) # pp = pp
p         # pp = p:pp

{-# INLINE (#) #-}

newtype Complete a = Complete { get :: a }

