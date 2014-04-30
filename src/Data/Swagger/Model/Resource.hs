-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE    ExtendedDefaultRules    #-}
{-# LANGUAGE    OverloadedStrings       #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | The <https://github.com/wordnik/swagger-spec/blob/master/versions/1.2.md#51-resource-listing Resource Listing>
-- part of the swagger specification. For construction please consider
-- using "Data.Swagger.Build.Resource".
module Data.Swagger.Model.Resource where

import Data.Aeson
import Data.Swagger.Model.Authorisation (Authorisation)
import Data.Swagger.Model.Util
import Data.Text (Text)

default (Text)

-- | Cf. <https://github.com/wordnik/swagger-spec/blob/master/versions/1.2.md#51-resource-listing Resource Listing Object>
data Resources = Resources
    { swaggerVersion :: Text
    , apis           :: [Resource]
    , apiVersion     :: Maybe Text
    , info           :: Maybe Info
    , authorisations :: Maybe [(Text, Authorisation)]
    } deriving Show

-- | Cf. <https://github.com/wordnik/swagger-spec/blob/master/versions/1.2.md#512-resource-object Resource Object>
data Resource = Resource
    { path        :: Text
    , description :: Maybe Text
    } deriving Show

-- | Cf. <https://github.com/wordnik/swagger-spec/blob/master/versions/1.2.md#513-info-object Info Object>
data Info = Info
    { title             :: Text
    , infoDescription   :: Maybe Text
    , termsOfServiceUrl :: Maybe Text
    , contact           :: Maybe Text
    , license           :: Maybe Text
    , licenseUrl        :: Maybe Text
    } deriving Show

-----------------------------------------------------------------------------
-- JSON instances

instance ToJSON Resources where
    toJSON a = object
        $ "swaggerVersion" .= swaggerVersion a
        # "apis"           .= apis a
        # "apiVersion"     .= apiVersion a
        # "info"           .= info a
        # "authorizations" .= authorisations a
        # []

instance ToJSON Resource where
    toJSON a = object
        $ "path"        .= path a
        # "description" .= description a
        # []

instance ToJSON Info where
    toJSON a = object
        $ "title"             .= title a
        # "description"       .= infoDescription a
        # "termsOfServiceUrl" .= termsOfServiceUrl a
        # "contact"           .= contact a
        # "license"           .= license a
        # "licenseUrl"        .= licenseUrl a
        # []
