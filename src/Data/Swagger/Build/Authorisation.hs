-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE OverloadedStrings #-}

module Data.Swagger.Build.Authorisation where

import Data.Text (Text)
import Data.Swagger.Model.Authorisation as Auth

basic :: Authorisation
basic = BasicAuth

apiKey :: PassMethod -> Text -> Authorisation
apiKey m n = ApiKey m n
