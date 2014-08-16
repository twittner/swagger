# Swagger Spec Implementation

This library implements the [Swagger 1.2 Specification][1] and provides some
support to conveniently build API descriptions. Here are some examples:

## Model definition

```haskell
import Data.Swagger.Build.Api

foo :: Model
foo = defineModel "Foo" $ do
    description "Some Foo model"
    property "header" bytes' $
        description "Foo's header property"

bar :: Model
bar = defineModel "Bar" $ do
    description "Some other model"
    property "header" (string $ enum ["bar", "baz"]) $
        description "lorem ipsum"
```

## API declaration

```haskell
import Data.Swagger.Build.Api

declare "http://petstore.swagger.wordnik.com/api" "1.2" $ do
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
           response 200 "OK" (model foo)
           response 400 "Bad Request" end
       operation "POST" "foo" $ do
           summary "something else"
           deprecated
```

## Resource listing


```haskell
import Data.Swagger.Build.Resource

resources "1.2" $ do
    apiVersion "1.0"
    api "/foo" $
        description "This is Foo's API"
```

The whole swagger model is an instance of Aeson's `ToJSON` type-class,
consequently it can be directly encoded to JSON.

A complete example can be found in the [wiki][3].

---
[1]: https://github.com/wordnik/swagger-spec/blob/master/versions/1.2.md
[2]: http://hackage.haskell.org/package/wai-routing
[3]: https://github.com/twittner/swagger/wiki/Example-usage
