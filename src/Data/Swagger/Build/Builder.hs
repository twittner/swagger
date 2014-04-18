-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Swagger.Build.Builder where

import Data.Type.Bool
import Data.Type.Equality
import GHC.TypeLits

newtype Builder (s :: [Symbol]) a = Builder { fromBuilder :: a }

type Elem a b = IsElem a b ~ True

type family IsElem a b where
    IsElem a '[] = False
    IsElem a (h ': t) = a == h || IsElem a t

