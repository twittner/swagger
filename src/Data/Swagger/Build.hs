-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Data.Swagger.Build
    ( -- * primitive types
      B.int32
    , B.int64
    , B.float
    , B.double
    , B.string
    , B.bytes
    , B.date
    , B.dateTime

      -- ** modifiers
    , B.def
    , B.enum
    , B.min
    , B.max

      -- * data-types
    , B.primitive
    , B.model
    , B.array
    , B.set
    , B.primitives
    , B.models

      -- * operation
    , B.operation
    , B.returns
    , B.parameter
    , B.file
    , B.summary
    , B.notes
    , B.response
    , B.produces
    , B.consumes
    , B.deprecated

      -- * response
    , B.responseModel

      -- * parameter
    , B.multiple
    , M.ParamType (..)

      -- * model
    , B.defineModel
    , B.property
    , B.subtypes
    , B.discriminator

      -- * general
    , B.description
    , B.required
    , B.done
    , B.with
    ) where

import qualified Data.Swagger.Build.Api as B
import qualified Data.Swagger.Model.Api as M
