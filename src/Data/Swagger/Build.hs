-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Data.Swagger.Build
    ( -- * data types
      -- ** primitives
      B.int32
    , B.int32'
    , B.int64
    , B.int64'
    , B.float
    , B.float'
    , B.bool
    , B.bool'
    , B.double
    , B.double'
    , B.string
    , B.string'
    , B.bytes
    , B.bytes'
    , B.date
    , B.date'
    , B.dateTime
    , B.dateTime'

      -- ** modifiers
    , B.def
    , B.enum
    , B.min
    , B.max

      -- ** constructors
    , B.ref
    , B.array
    , B.unique

      -- * builders
    , B.ApiDeclBuilder
    , B.ApiBuilder
    , B.OperationBuilder
    , B.ParameterBuilder
    , B.ModelBuilder
    , B.PropertyBuilder

      -- * API declaration
    , B.declare
    , B.apiVersion
    , B.resourcePath
    , B.api
    , B.model

      -- * operation
    , B.operation
    , B.returns
    , B.parameter
    , B.file
    , B.body
    , B.summary
    , B.notes
    , B.response
    , B.produces
    , B.authorisation

      -- * parameter
    , B.multiple
    , M.ParamType (..)

      -- * model
    , B.defineModel
    , B.property
    , B.children

      -- * general
    , B.description
    , B.optional
    , B.consumes
    , B.deprecated
    , B.end

      -- * authorisation
    , B.Auth (..)
    , A.basic
    , A.apiKey
    , L.Authorisation
    , L.PassMethod (..)
    ) where

import qualified Data.Swagger.Build.Api as B
import qualified Data.Swagger.Model.Api as M

import qualified Data.Swagger.Build.Authorisation as A
import qualified Data.Swagger.Model.Authorisation as L
