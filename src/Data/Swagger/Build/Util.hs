-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Swagger.Build.Util where

import Control.Monad.Trans.State.Strict
import Data.Swagger.Model.Api as Api
import Data.Swagger.Model.Authorisation (Scope)
import Data.Text (Text)
import Data.Type.Bool
import Data.Type.Equality

type Elem a b = IsElem a b ~ True

type family IsElem a b where
    IsElem a '[] = False
    IsElem a (h ': t) = a == h || IsElem a t

-----------------------------------------------------------------------------
-- Fields occuring in multiple locations

data Common f a = Common
    { descr :: Maybe Text
    , reqrd :: Maybe Bool
    , prod  :: Maybe [Text]
    , cons  :: Maybe [Text]
    , modls :: Maybe [Model]
    , auths :: Maybe [(Text, Maybe Scope)]
    , other :: a
    }

common :: a -> Common f a
common = Common Nothing (Just True) Nothing Nothing Nothing Nothing

description :: Elem "description" f => Text -> State (Common f a) ()
description d = modify $ \c -> c { descr = Just d }

optional :: Elem "required" f => State (Common f a) ()
optional = modify $ \c -> c { reqrd = Nothing }

produces :: Elem "produces" f => Text -> State (Common f a) ()
produces t = modify $ \c -> c { prod = maybe (Just [t]) (Just . (t:)) (prod c) }

consumes :: Elem "consumes" f => Text -> State (Common f a) ()
consumes t = modify $ \c -> c { cons = maybe (Just [t]) (Just . (t:)) (cons c) }

model :: Elem "models" f => Model -> State (Common f a) ()
model m = modify $ \c -> c { modls = maybe (Just [m]) (Just . (m:)) (modls c) }

data Auth = Basic | ApiKey | OAuth2 Scope | None

authorisation :: Elem "authorisations" f => Auth -> State (Common f a) ()
authorisation a = modify $ \c ->
    c { auths = maybe (Just (f a)) (Just . (f a ++)) (auths c) }
  where
    f Basic      = [("basic", Nothing)]
    f ApiKey     = [("apiKey", Nothing)]
    f (OAuth2 s) = [("oauth2", Just s)]
    f None       = []

end :: Monad m => m ()
end = return ()

