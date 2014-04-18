-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE OverloadedStrings #-}

module Test.Api where

import Data.Aeson
import Data.Swagger.Build
import Data.Swagger.Model.Api (Model, Operation)
import Prelude hiding (min, max)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.ByteString.Lazy.Char8 as B

tests :: TestTree
tests = testGroup "example declarations"
    [ testCase "operation foo" (render operationFoo)
    , testCase "model foo" (render foo)
    ]
  where
    render :: ToJSON a => a -> IO ()
    render = B.putStrLn . encode

operationFoo :: Operation
operationFoo = operation "GET" "foo" $ do
    summary "give me some foo"
    notes   "but only the good one"
    returns (model foo)
    parameter Header "type" (primitive $ string . enum ["bar", "baz"]) $ do
        description "specifies the type of foo"
        required
    parameter Query "format" (primitive $ string . enum ["plain", "html"]) $
        description "output format"
    parameter Query "size" (primitive $ int32 . min 1 . max 100 . def 10) $
        description "amount of foo"
    produces "application/json"
    produces "plain/html"
    response 200 "OK" (responseModel foo)
    response 400 "Bad Request" done

foo :: Model
foo = defineModel "Foo" $ do
    description "A bottle of foo"
    property "rabbit" (array unique $ primitive int32) $
        description "A foo's rabbit"
    property "white" (primitive $ bool . def False) $ do
        description "a white rabbit?"
        required
    property "bar" (model bar) done

bar :: Model
bar = defineModel "Bar" $
    property "foo" (model foo) done

