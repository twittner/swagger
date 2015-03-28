-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE    ExtendedDefaultRules    #-}
{-# LANGUAGE    GADTs                   #-}
{-# LANGUAGE    OverloadedStrings       #-}
{-# LANGUAGE    StandaloneDeriving      #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | The <https://github.com/wordnik/swagger-spec/blob/master/versions/1.2.md#52-api-declaration API Declaration>
-- part of the swagger specification. For construction please consider
-- using "Data.Swagger.Build.Api".
module Data.Swagger.Model.Api
    ( ApiDecl      (..)
    , API          (..)
    , Operation    (..)
    , Parameter    (..)
    , ParamType    (..)
    , Response     (..)
    , Model        (..)
    , Property     (..)
    , DataType     (..)
    , Primitive    (..)
    , Items        (..)
    , PrimType     (..)
    , File         (..)
    , ModelId
    , PropertyName
    ) where

import Control.Applicative
import Data.Aeson hiding (Array)
import Data.Aeson.Types (Pair)
import Data.Swagger.Model.Authorisation (Scope)
import Data.Swagger.Model.Util
import Data.Text (Text)
import Prelude

default (Text)

-- | Cf. <https://github.com/wordnik/swagger-spec/blob/master/versions/1.2.md#52-api-declaration API Declaration>
data ApiDecl = ApiDecl
    { swaggerVersion    :: Text
    , basePath          :: Text
    , apis              :: [API]
    , apiVersion        :: Maybe Text
    , resourcePath      :: Maybe Text
    , models            :: Maybe [(Text, Model)]
    , apiProduces       :: Maybe [Text]
    , apiConsumes       :: Maybe [Text]
    , apiAuthorisations :: Maybe [(Text, [Scope])]
    } deriving Show

-- | Cf. <https://github.com/wordnik/swagger-spec/blob/master/versions/1.2.md#522-api-object API Object>
data API = API
    { path           :: Text
    , operations     :: [Operation]
    , apiDescription :: Maybe Text
    } deriving Show

-- | Cf. <https://github.com/wordnik/swagger-spec/blob/master/versions/1.2.md#523-operation-object Operation Object>
data Operation = Operation
    { method         :: Text
    , nickname       :: Text
    , returnType     :: Either () DataType
    , parameters     :: [Parameter]
    , summary        :: Maybe Text
    , notes          :: Maybe Text
    , authorisations :: Maybe [(Text, [Scope])]
    , responses      :: Maybe [Response]
    , produces       :: Maybe [Text]
    , consumes       :: Maybe [Text]
    , deprecated     :: Maybe Bool
    } deriving Show

-- | Cf. <https://github.com/wordnik/swagger-spec/blob/master/versions/1.2.md#524-parameter-object Parameter Object>
data Parameter = Parameter
    { paramType     :: ParamType
    , inputType     :: Either File DataType
    , paramName     :: Text
    , description   :: Maybe Text
    , required      :: Maybe Bool
    , allowMultiple :: Maybe Bool
    } deriving Show

data ParamType
    = Path
    | Query
    | Body
    | Header
    | Form
    deriving (Eq, Show)

-- | Cf. <https://github.com/wordnik/swagger-spec/blob/master/versions/1.2.md#525-response-message-object Response Message Object>
data Response = Response
    { code          :: Int
    , message       :: Text
    , responseModel :: Maybe ModelId
    } deriving Show

type ModelId      = Text
type PropertyName = Text

-- | Cf. <https://github.com/wordnik/swagger-spec/blob/master/versions/1.2.md#527-model-object Model Object>
data Model = Model
    { modelId          :: ModelId
    , properties       :: [(PropertyName, Property)]
    , modelDescription :: Maybe Text
    , requiredProps    :: Maybe [PropertyName]
    , subTypes         :: Maybe [Model]
    , discriminator    :: Maybe PropertyName
    } deriving Show

-- | Cf. <https://github.com/wordnik/swagger-spec/blob/master/versions/1.2.md#529-property-object Property Object>
data Property = Property
    { propertyType    :: DataType
    , propDescription :: Maybe Text
    } deriving Show

data DataType where
    Prim  :: (Show a, ToJSON a) => Primitive a -> DataType
    Array :: (Show a, ToJSON a) => Items a -> Maybe Bool -> DataType
    Ref   :: ModelId -> DataType

deriving instance Show DataType

-- | Cf. <https://github.com/wordnik/swagger-spec/blob/master/versions/1.2.md#431-primitives Primitive Types>
data Primitive a = Primitive
    { primType     :: PrimType
    , defaultValue :: Maybe a
    , enum         :: Maybe [a]
    , minVal       :: Maybe a
    , maxVal       :: Maybe a
    } deriving Show

data Items a
    = PrimItems (Primitive a)
    | ModelItems ModelId
    deriving Show

data PrimType
    = PrimInt32
    | PrimInt64
    | PrimFloat
    | PrimDouble
    | PrimString
    | PrimByte
    | PrimBool
    | PrimDate
    | PrimDateTime
    deriving Show

data File = File deriving Show

-----------------------------------------------------------------------------
-- JSON instances

instance ToJSON ApiDecl where
    toJSON a = object
        $ "swaggerVersion" .= swaggerVersion a
        # "apiVersion"     .= apiVersion a
        # "basePath"       .= basePath a
        # "resourcePath"   .= resourcePath a
        # "apis"           .= apis a
        # "models"         .= (fromPairs <$> models a)
        # "produces"       .= apiProduces a
        # "consumes"       .= apiConsumes a
        # "authorizations" .= (fromAuth <$> apiAuthorisations a)
        # []

instance ToJSON API where
    toJSON a = object
        $ "path"        .= path a
        # "description" .= apiDescription a
        # "operations"  .= operations a
        # []

instance ToJSON Operation where
    toJSON a = object
        $ "method"           .= method a
        # "summary"          .= summary a
        # "notes"            .= notes a
        # "nickname"         .= nickname a
        # "authorizations"   .= (fromAuth <$> authorisations a)
        # "parameters"       .= parameters a
        # "responseMessages" .= responses a
        # "produces"         .= produces a
        # "consumes"         .= consumes a
        # "deprecated"       .= deprecated a
        # either (const ["type" .= "void"]) (fromType False) (returnType a)

instance ToJSON Parameter where
    toJSON a = object
        $ "paramType"     .= paramType a
        # "name"          .= paramName a
        # "description"   .= description a
        # "required"      .= required a
        # "allowMultiple" .= allowMultiple a
        # either (const ["type" .= "File"]) (fromType False) (inputType a)

instance ToJSON ParamType where
    toJSON Path   = "path"
    toJSON Query  = "query"
    toJSON Body   = "body"
    toJSON Header = "header"
    toJSON Form   = "form"

instance ToJSON Response where
    toJSON a = object
        $ "code"          .= code a
        # "message"       .= message a
        # "responseModel" .= responseModel a
        # []

instance ToJSON Model where
    toJSON a = object
        $ "id"            .= modelId a
        # "description"   .= modelDescription a
        # "required"      .= requiredProps a
        # "properties"    .= fromPairs (properties a)
        # "subTypes"      .= (map modelId <$> subTypes a)
        # "discriminator" .= discriminator a
        # []

instance ToJSON Property where
    toJSON a = object
        $ "description" .= propDescription a
        # fromType True (propertyType a)

fromType :: Bool -> DataType -> [Pair]
fromType _     (Prim    p) = fromPrim p
fromType _     (Array i b) = fromArray i b
fromType False (Ref     r) = ["type" .= r]
fromType True  (Ref     r) = ["$ref" .= r]

fromPrim :: ToJSON a => Primitive a -> [Pair]
fromPrim p =
      "defaultValue" .= defaultValue p
    # "enum"         .= enum p
    # "minimum"      .= minVal p
    # "maximum"      .= maxVal p
    # fromPrimType (primType p)

fromArray :: ToJSON a => Items a -> Maybe Bool -> [Pair]
fromArray i b =
      "type"        .= "array"
    # "items"       .= fromPairs (fromItems i)
    # "uniqueItems" .= b
    # []

fromPrimType :: PrimType -> [Pair]
fromPrimType PrimInt32    = ["type" .= "integer", "format" .= "int32" ]
fromPrimType PrimInt64    = ["type" .= "integer", "format" .= "int64" ]
fromPrimType PrimFloat    = ["type" .= "number", "format" .= "float" ]
fromPrimType PrimDouble   = ["type" .= "number", "format" .= "double" ]
fromPrimType PrimString   = ["type" .= "string"]
fromPrimType PrimByte     = ["type" .= "string", "format" .= "byte" ]
fromPrimType PrimBool     = ["type" .= "boolean"]
fromPrimType PrimDate     = ["type" .= "string", "format" .= "date" ]
fromPrimType PrimDateTime = ["type" .= "string", "format" .= "date-time" ]

fromItems :: ToJSON a => Items a -> [Pair]
fromItems (ModelItems i) = [ "$ref" .= i ]
fromItems (PrimItems  p) = fromPrim p

fromAuth :: [(Text, [Scope])] -> Value
fromAuth = fromPairs . map (\x -> (fst x, map toJSON (snd x)))

