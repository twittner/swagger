-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE OverloadedStrings #-}

module Test.Api where

import Data.Aeson
import Data.Swagger.Build
import Data.Swagger.Model.Api (Model, ApiDecl)
import Prelude hiding (min, max)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.ByteString.Lazy.Char8 as B

tests :: TestTree
tests = testGroup "example declarations"
    [ testCase "api" (render apiDecl)
    , testCase "model foo" (render foo)
    ]
  where
    render :: ToJSON a => a -> IO ()
    render = B.putStrLn . encode

apiDecl :: ApiDecl
apiDecl = declare "http://petstore.swagger.wordnik.com/api" "1.2" $ do
    apiVersion "1.0.0"
    resourcePath "/store"
    model foo
    model bar
    produces "application/json"
    produces "text/html"
    produces "text/plain"
    api "/store/order/{orderId}" $ do
        operation "GET" "foo" $ do
            summary "give me some foo"
            notes   "but only the good one"
            returns (ref foo)
            parameter Header "type" (string $ enum ["bar", "baz"]) $ do
                description "specifies the type of foo"
                optional
            parameter Query "format" (string $ enum ["plain", "html"]) $
                description "output format"
            parameter Query "size" (int32 $ min 1 . max 100 . def 10) $
                description "amount of foo"
            produces "application/json"
            produces "text/html"
            response 200 "OK" (responseModel foo)
            response 400 "Bad Request" end
        operation "POST" "foo" $ do
            summary "something else"
            deprecated

foo :: Model
foo = defineModel "Foo" $ do
    description "A bottle of foo"
    property "rabbit" (array int32') $
        description "A foo's rabbit"
    property "white" (bool $ def False) $ do
        description "a white rabbit?"
        optional
    property "bar" (ref bar) end

bar :: Model
bar = defineModel "Bar" $
    property "foo" (ref foo) end

