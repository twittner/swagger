-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE OverloadedStrings #-}

module Test.Api where

import Data.Aeson
import Data.Swagger.Build
import Data.Swagger.Model.Api (Model)
import Prelude hiding (min, max)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.ByteString.Lazy.Char8 as B

tests :: TestTree
tests = testGroup "example declarations"
    [ testCase "operation foo" renderFoo ]

renderFoo :: IO ()
renderFoo = B.putStrLn $ encode operationFoo where
  operationFoo = operation "GET" "foo" $ do
    summary "give me some foo"
    notes   "but only the good one"
    returns (model foo)
    parameter Header "type" (primitive $ string `with` enum ["bar", "baz"]) $ do
        description "specifies the type of foo"
        required
    parameter Query "format" (primitive $ string `with` enum ["plain", "html"]) $
        description "output format"
    parameter Query "size"
        (primitive $ int32 `with` do min 1 >> max 100 >> def 10) $
        description "amount of foo"
    produces "application/json"
    produces "plain/html"
    response 200 "OK" (responseModel foo)
    response 400 "Bad Request" done

foo :: Model
foo = defineModel "Foo" done

