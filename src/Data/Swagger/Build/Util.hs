-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Swagger.Build.Util where

import Data.Type.Bool
import Data.Type.Equality

type Elem a b = IsElem a b ~ True

type family IsElem a b where
    IsElem a '[] = False
    IsElem a (h ': t) = a == h || IsElem a t

