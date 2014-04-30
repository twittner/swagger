-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Data.Swagger.Build.Resource
    ( Data.Swagger.Build.Resource.resources
    , Data.Swagger.Build.Resource.api
    , Data.Swagger.Build.Resource.apiVersion
    , Data.Swagger.Build.Resource.info
    , Data.Swagger.Build.Resource.termsOfServiceUrl
    , Data.Swagger.Build.Resource.contact
    , Data.Swagger.Build.Resource.license
    , Data.Swagger.Build.Resource.licenseUrl
    , Data.Swagger.Build.Resource.authorisation
    , Data.Swagger.Build.Util.end

      -- * builder types
    , ResourcesBuilder
    , InfoBuilder
    ) where

import Control.Monad.Trans.State.Strict
import Data.Text (Text)
import Data.Swagger.Build.Util hiding (authorisation)
import Data.Swagger.Model.Authorisation as A
import Data.Swagger.Model.Resource as R

type ResourcesBuilder = State Resources ()

-- | Construct a resource listing object given a swagger version and some
-- resource objects.
resources :: Text -> ResourcesBuilder -> Resources
resources v s = execState s start
  where
    start = Resources v [] Nothing Nothing Nothing

type ResourceSt = Common '["description"] Resource
type ResourceBuilder = State ResourceSt ()

-- | Add one resource object to a resource listing given a path and some
-- resource specific values.
api :: Text -> ResourceBuilder -> ResourcesBuilder
api p s = modify $ \r ->
    r { apis = value (execState s start) : apis r }
  where
    start   = common $ Resource p Nothing
    value c = (other c) { R.description = descr c }

apiVersion :: Text -> ResourcesBuilder
apiVersion v = modify $ \r -> r { R.apiVersion = Just v }

type InfoSt = Common '["description"] Info
type InfoBuilder = State InfoSt ()

-- | Set the info object of a resource listing object given a title and
-- other infor object specific values.
info :: Text -> InfoBuilder -> ResourcesBuilder
info t s = modify $ \r ->
    r { R.info = Just $ value (execState s start) }
  where
    start   = common $ Info t Nothing Nothing Nothing Nothing Nothing
    value c = (other c) { infoDescription = descr c }

termsOfServiceUrl :: Text -> InfoBuilder
termsOfServiceUrl u = modify $ \c -> c { other = (other c) { R.termsOfServiceUrl = Just u } }

contact :: Text -> InfoBuilder
contact u = modify $ \c -> c { other = (other c) { R.contact = Just u } }

license :: Text -> InfoBuilder
license u = modify $ \c -> c { other = (other c) { R.license = Just u } }

licenseUrl :: Text -> InfoBuilder
licenseUrl u = modify $ \c -> c { other = (other c) { R.licenseUrl = Just u } }

-- | Add a authorisation object to a resource listing with the given name.
authorisation :: Text -> Authorisation -> ResourcesBuilder
authorisation n a = modify $ \r -> let x = (n, a) in
    r { authorisations = maybe (Just [x]) (Just . (x:)) (authorisations r) }

