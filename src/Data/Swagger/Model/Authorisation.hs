-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE    ExtendedDefaultRules    #-}
{-# LANGUAGE    OverloadedStrings       #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | The <https://github.com/wordnik/swagger-spec/blob/master/versions/1.2.md#515-authorization-object Authorisation Object>
-- part of the swagger specification. For construction please consider
-- using "Data.Swagger.Build.Authorisation".
module Data.Swagger.Model.Authorisation where

import Data.Aeson
import Data.Swagger.Model.Util
import Data.Text (Text)

default (Text)

-- | Cf. <https://github.com/wordnik/swagger-spec/blob/master/versions/1.2.md#515-authorization-object Authorization Object>
data Authorisation
    = BasicAuth
    | ApiKey
        { passAs  :: PassMethod
        , keyname :: Text
        }
    | OAuth2
        { scopes     :: Maybe [Scope]
        , grantTypes :: GrantTypes
        }
    deriving Show

data PassMethod
    = PassAsHeader
    | PassAsQuery
    deriving (Eq, Show)

-- | Cf. <https://github.com/wordnik/swagger-spec/blob/master/versions/1.2.md#516-scope-object Scope Object>
data Scope = Scope
    { scope       :: Text
    , description :: Maybe Text
    } deriving Show

-- | Cf. <https://github.com/wordnik/swagger-spec/blob/master/versions/1.2.md#517-grant-types-object Grant Types Object>
data GrantTypes = GrantTypes
    { implicit :: Maybe ImplicitGrant
    , authCode :: Maybe AuthCode
    } deriving Show

-- | Cf. <https://github.com/wordnik/swagger-spec/blob/master/versions/1.2.md#518-implicit-object Implicit Object>
data ImplicitGrant = ImplicitGrant
    { loginEndpoint :: LoginEndpoint
    , tokenName     :: Maybe Text
    } deriving Show

newtype LoginEndpoint = LoginEndpoint
    { loginUrl :: Text
    } deriving Show

-- | Cf. <https://github.com/wordnik/swagger-spec/blob/master/versions/1.2.md#519-authorization-code-object Authorization Code Object>
data AuthCode = AuthCode
    { tokenRequestEndpoint :: TokenRequestEndpoint
    , tokenEndpoint        :: TokenEndpoint
    } deriving Show

-- | Cf. <https://github.com/wordnik/swagger-spec/blob/master/versions/1.2.md#5111-token-request-endpoint-object Token Request Endpoint Object>
data TokenRequestEndpoint = TokenRequestEndpoint
    { tokenRequestUrl  :: Text
    , clientIdName     :: Maybe Text
    , clientSecretName :: Maybe Text
    } deriving Show

-- | Cf. <https://github.com/wordnik/swagger-spec/blob/master/versions/1.2.md#5112-token-endpoint-object Token Endpoint Object>
data TokenEndpoint = TokenEndpoint
    { tokenEndpointUrl       :: Text
    , tokenEndpointTokenName :: Maybe Text
    } deriving Show

-----------------------------------------------------------------------------
-- JSON instances

instance ToJSON Authorisation where
    toJSON BasicAuth    = object [ "type" .= "basicAuth" ]
    toJSON (ApiKey p k) = object
        $ "type"    .= "apiKey"
        # "passAs"  .= p
        # "keyname" .= k
        # []
    toJSON (OAuth2 s g) = object
        $ "type"       .= "oauth2"
        # "scopes"     .= s
        # "grantTypes" .= g
        # []

instance ToJSON PassMethod where
    toJSON PassAsHeader = "header"
    toJSON PassAsQuery  = "query"

instance ToJSON Scope where
    toJSON a = object
        $ "scope"       .= scope a
        # "description" .= description a
        # []

instance ToJSON GrantTypes where
    toJSON a = object
        $ "implicit"           .= implicit a
        # "authorization_code" .= authCode a
        # []

instance ToJSON ImplicitGrant where
    toJSON a = object
        $ "loginEndpoint" .= loginEndpoint a
        # "tokenName"     .= tokenName a
        # []

instance ToJSON AuthCode where
    toJSON a = object
        $ "tokenRequestEndpoint" .= tokenRequestEndpoint a
        # "tokenEndpoint"        .= tokenEndpoint a
        # []

instance ToJSON LoginEndpoint where
    toJSON a = object [ "url" .= loginUrl a ]

instance ToJSON TokenRequestEndpoint where
    toJSON a = object
        $ "url"              .= tokenRequestUrl a
        # "clientIdName"     .= clientIdName a
        # "clientSecretName" .= clientSecretName a
        # []

instance ToJSON TokenEndpoint where
    toJSON a = object
        $ "url"       .= tokenEndpointUrl a
        # "tokenName" .= tokenEndpointTokenName a
        # []

