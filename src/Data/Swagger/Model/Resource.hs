-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE    ExtendedDefaultRules    #-}
{-# LANGUAGE    OverloadedStrings       #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Data.Swagger.Model.Resource where

import Data.Aeson
import Data.Swagger.Model.Authorisation (Authorisation)
import Data.Swagger.Model.Util
import Data.Text (Text)

default (Text)

data Resources = Resources
    { swaggerVersion :: Text
    , apis           :: [Resource]
    , apiVersion     :: Maybe Text
    , info           :: Maybe Info
    , authorisations :: Maybe [(Text, Authorisation)]
    } deriving Show

data Resource = Resource
    { path        :: Text
    , description :: Maybe Text
    } deriving Show

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
