-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Data.Swagger.Build.Authorisation
    ( -- * basic auth
      Data.Swagger.Build.Authorisation.basic

      -- * api key
    , Data.Swagger.Build.Authorisation.apiKey
    , PassMethod (..)

      -- * oauth2
    , Data.Swagger.Build.Authorisation.oauth2
    , Data.Swagger.Build.Authorisation.scope
    , Data.Swagger.Build.Authorisation.implicit
    , Data.Swagger.Build.Authorisation.authCode
    , Data.Swagger.Build.Authorisation.tokenEndpoint
    , Data.Swagger.Build.Authorisation.tokenRequestEndpoint
    , Data.Swagger.Build.Authorisation.clientIdName
    , Data.Swagger.Build.Authorisation.clientSecretName
    , Data.Swagger.Build.Authorisation.token
    , Data.Swagger.Build.Util.end

      -- * builder types
    , OAuth2Builder
    , ScopeSt
    , ScopeBuilder
    , ImplicitBuilder
    , TokenEndpointBuilder
    , TokenReqEndpointBuilder

    ) where

import Control.Monad.Trans.State.Strict
import Data.Text (Text)
import Data.Swagger.Build.Util hiding (Auth (..))
import Data.Swagger.Model.Authorisation as Auth

basic :: Authorisation
basic = BasicAuth

apiKey :: PassMethod -> Text -> Authorisation
apiKey m n = ApiKey m n

-----------------------------------------------------------------------------
-- OAuth2

type OAuth2Builder = State [Scope] ()

oauth2 :: GrantTypes -> OAuth2Builder -> Authorisation
oauth2 t s =
    case execState s [] of
        [] -> OAuth2 Nothing t
        ss -> OAuth2 (Just ss) t

type ScopeSt = Common '["description"] Scope
type ScopeBuilder = State ScopeSt ()

-- | Add one scope with the given name to an OAuth2 object.
scope :: Text -> ScopeBuilder -> OAuth2Builder
scope t s = modify $ \o -> value (execState s start) : o
  where
    start   = common $ Scope t Nothing
    value c = (other c) { Auth.description = descr c }

type ImplicitBuilder = State (TokenName ImplicitGrant) ()

-- | Construct an implicit grant type with the given login endpoint and
-- some optional token name.
implicit :: Text -> ImplicitBuilder -> GrantTypes
implicit e s = GrantTypes (Just $ value $ execState s start) Nothing
  where
    start   = mkTokenName $ ImplicitGrant (LoginEndpoint e) Nothing
    value t = (unwrap t) { Auth.tokenName = tname t }

-- | Construct an authentorisation code based grant type object.
authCode :: TokenRequestEndpoint -> TokenEndpoint -> GrantTypes
authCode r e = GrantTypes Nothing (Just $ AuthCode r e)

type TokenEndpointBuilder = State (TokenName TokenEndpoint) ()

tokenEndpoint :: Text -> TokenEndpointBuilder -> TokenEndpoint
tokenEndpoint u s = value $ execState s start
  where
    start   = mkTokenName $ TokenEndpoint u Nothing
    value t = (unwrap t) { tokenEndpointTokenName = tname t }

type TokenReqEndpointBuilder = State TokenRequestEndpoint ()

tokenRequestEndpoint :: Text -> TokenReqEndpointBuilder -> TokenRequestEndpoint
tokenRequestEndpoint u s = execState s start
  where
    start = TokenRequestEndpoint u Nothing Nothing

clientIdName :: Text -> TokenReqEndpointBuilder
clientIdName n = modify $ \x -> x { Auth.clientIdName = Just n }

clientSecretName :: Text -> TokenReqEndpointBuilder
clientSecretName n = modify $ \x -> x { Auth.clientSecretName = Just n }

-----------------------------------------------------------------------------
-- Helpers

data TokenName a = TokenName
    { tname  :: Maybe Text
    , unwrap :: a
    }

token :: Text -> State (TokenName a) ()
token n = modify $ \t -> t { tname = Just n }

mkTokenName :: a -> TokenName a
mkTokenName = TokenName Nothing
