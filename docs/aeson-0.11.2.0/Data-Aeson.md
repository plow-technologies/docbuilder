-   [Source](src/Data-Aeson.html)
-   [Contents](index.html)
-   [Index](doc-index.html)

aeson-0.11.2.0: Fast JSON parsing and encoding

|              |                                                        |
|--------------|--------------------------------------------------------|
| Copyright    | (c) 2011-2016 Bryan O'Sullivan (c) 2011 MailRank, Inc. |
| License      | BSD3                                                   |
| Maintainer   | Bryan O'Sullivan &lt;bos@serpentine.com&gt;            |
| Stability    | experimental                                           |
| Portability  | portable                                               |
| Safe Haskell | None                                                   |
| Language     | Haskell2010                                            |

Data.Aeson

Contents

-   [How to use this library](#g:1)
    -   [Writing instances by hand](#g:2)
    -   [Working with the AST](#g:3)
    -   [Decoding to a Haskell value](#g:4)
    -   [Decoding a mixed-type object](#g:5)
-   [Encoding and decoding](#g:6)
    -   [Direct encoding](#g:7)
    -   [Variants for strict bytestrings](#g:8)
-   [Core JSON types](#g:9)
-   [Convenience types](#g:10)
-   [Type conversion](#g:11)
    -   [Generic JSON classes and options](#g:12)
-   [Inspecting `Value`s](#g:13)
-   [Constructors and accessors](#g:14)
-   [Parsing](#g:15)

Description

Types and functions for working efficiently with JSON data.

(A note on naming: in Greek mythology, Aeson was the father of Jason.)

Synopsis

-   [decode](#v:decode) :: [FromJSON](Data-Aeson.html#t:FromJSON) a =&gt; [ByteString](../bytestring-0.10.6.0/Data-ByteString-Lazy.html#t:ByteString) -&gt; [Maybe](../base-4.8.2.0/Data-Maybe.html#t:Maybe) a
-   [decode'](#v:decode-39-) :: [FromJSON](Data-Aeson.html#t:FromJSON) a =&gt; [ByteString](../bytestring-0.10.6.0/Data-ByteString-Lazy.html#t:ByteString) -&gt; [Maybe](../base-4.8.2.0/Data-Maybe.html#t:Maybe) a
-   [eitherDecode](#v:eitherDecode) :: [FromJSON](Data-Aeson.html#t:FromJSON) a =&gt; [ByteString](../bytestring-0.10.6.0/Data-ByteString-Lazy.html#t:ByteString) -&gt; [Either](../base-4.8.2.0/Data-Either.html#t:Either) [String](../base-4.8.2.0/Data-String.html#t:String) a
-   [eitherDecode'](#v:eitherDecode-39-) :: [FromJSON](Data-Aeson.html#t:FromJSON) a =&gt; [ByteString](../bytestring-0.10.6.0/Data-ByteString-Lazy.html#t:ByteString) -&gt; [Either](../base-4.8.2.0/Data-Either.html#t:Either) [String](../base-4.8.2.0/Data-String.html#t:String) a
-   [encode](#v:encode) :: [ToJSON](Data-Aeson.html#t:ToJSON) a =&gt; a -&gt; [ByteString](../bytestring-0.10.6.0/Data-ByteString-Lazy.html#t:ByteString)
-   [decodeStrict](#v:decodeStrict) :: [FromJSON](Data-Aeson.html#t:FromJSON) a =&gt; [ByteString](../bytestring-0.10.6.0/Data-ByteString.html#t:ByteString) -&gt; [Maybe](../base-4.8.2.0/Data-Maybe.html#t:Maybe) a
-   [decodeStrict'](#v:decodeStrict-39-) :: [FromJSON](Data-Aeson.html#t:FromJSON) a =&gt; [ByteString](../bytestring-0.10.6.0/Data-ByteString.html#t:ByteString) -&gt; [Maybe](../base-4.8.2.0/Data-Maybe.html#t:Maybe) a
-   [eitherDecodeStrict](#v:eitherDecodeStrict) :: [FromJSON](Data-Aeson.html#t:FromJSON) a =&gt; [ByteString](../bytestring-0.10.6.0/Data-ByteString.html#t:ByteString) -&gt; [Either](../base-4.8.2.0/Data-Either.html#t:Either) [String](../base-4.8.2.0/Data-String.html#t:String) a
-   [eitherDecodeStrict'](#v:eitherDecodeStrict-39-) :: [FromJSON](Data-Aeson.html#t:FromJSON) a =&gt; [ByteString](../bytestring-0.10.6.0/Data-ByteString.html#t:ByteString) -&gt; [Either](../base-4.8.2.0/Data-Either.html#t:Either) [String](../base-4.8.2.0/Data-String.html#t:String) a
-   <span class="keyword">data</span> [Value](#t:Value)
    -   = [Object](#v:Object) ![Object](Data-Aeson.html#t:Object)
    -   | [Array](#v:Array) ![Array](Data-Aeson.html#t:Array)
    -   | [String](#v:String) ![Text](../text-1.2.2.1/Data-Text.html#t:Text)
    -   | [Number](#v:Number) ![Scientific](../scientific-0.3.4.9/Data-Scientific.html#t:Scientific)
    -   | [Bool](#v:Bool) ![Bool](../base-4.8.2.0/Data-Bool.html#t:Bool)
    -   | [Null](#v:Null)
-   <span class="keyword">data</span> [Encoding](#t:Encoding)
-   [fromEncoding](#v:fromEncoding) :: [Encoding](Data-Aeson.html#t:Encoding) -&gt; [Builder](../bytestring-0.10.6.0/Data-ByteString-Builder.html#t:Builder)
-   <span class="keyword">type</span> [Array](#t:Array) = [Vector](../vector-0.11.0.0/Data-Vector.html#t:Vector) [Value](Data-Aeson.html#t:Value)
-   <span class="keyword">type</span> [Object](#t:Object) = [HashMap](../unordered-containers-0.2.7.1/Data-HashMap-Strict.html#t:HashMap) [Text](../text-1.2.2.1/Data-Text.html#t:Text) [Value](Data-Aeson.html#t:Value)
-   <span class="keyword">newtype</span> [DotNetTime](#t:DotNetTime) = [DotNetTime](#v:DotNetTime) {
    -   [fromDotNetTime](#v:fromDotNetTime) :: [UTCTime](../time-1.5.0.1/Data-Time-Clock.html#t:UTCTime)

    }
-   <span class="keyword">class</span> [FromJSON](#t:FromJSON) a <span class="keyword">where</span>
    -   [parseJSON](#v:parseJSON) :: [Value](Data-Aeson.html#t:Value) -&gt; [Parser](Data-Aeson-Types.html#t:Parser) a
-   <span class="keyword">data</span> [Result](#t:Result) a
    -   = [Error](#v:Error) [String](../base-4.8.2.0/Data-String.html#t:String)
    -   | [Success](#v:Success) a
-   [fromJSON](#v:fromJSON) :: [FromJSON](Data-Aeson.html#t:FromJSON) a =&gt; [Value](Data-Aeson.html#t:Value) -&gt; [Result](Data-Aeson.html#t:Result) a
-   <span class="keyword">class</span> [ToJSON](#t:ToJSON) a <span class="keyword">where</span>
    -   [toJSON](#v:toJSON) :: a -&gt; [Value](Data-Aeson.html#t:Value)
    -   [toEncoding](#v:toEncoding) :: a -&gt; [Encoding](Data-Aeson.html#t:Encoding)
-   <span class="keyword">class</span> [KeyValue](#t:KeyValue) kv <span class="keyword">where</span>
    -   [(.=)](#v:.-61-) :: [ToJSON](Data-Aeson.html#t:ToJSON) v =&gt; [Text](../text-1.2.2.1/Data-Text.html#t:Text) -&gt; v -&gt; kv
-   <span class="keyword">class</span> [GFromJSON](#t:GFromJSON) f <span class="keyword">where</span>
    -   [gParseJSON](#v:gParseJSON) :: [Options](Data-Aeson-Types.html#t:Options) -&gt; [Value](Data-Aeson.html#t:Value) -&gt; [Parser](Data-Aeson-Types.html#t:Parser) (f a)
-   <span class="keyword">class</span> [GToJSON](#t:GToJSON) f <span class="keyword">where</span>
    -   [gToJSON](#v:gToJSON) :: [Options](Data-Aeson-Types.html#t:Options) -&gt; f a -&gt; [Value](Data-Aeson.html#t:Value)
-   <span class="keyword">class</span> [GToEncoding](#t:GToEncoding) f <span class="keyword">where</span>
    -   [gToEncoding](#v:gToEncoding) :: [Options](Data-Aeson-Types.html#t:Options) -&gt; f a -&gt; [Encoding](Data-Aeson.html#t:Encoding)
-   [genericToJSON](#v:genericToJSON) :: ([Generic](../base-4.8.2.0/GHC-Generics.html#t:Generic) a, [GToJSON](Data-Aeson.html#t:GToJSON) ([Rep](../base-4.8.2.0/GHC-Generics.html#t:Rep) a)) =&gt; [Options](Data-Aeson-Types.html#t:Options) -&gt; a -&gt; [Value](Data-Aeson.html#t:Value)
-   [genericToEncoding](#v:genericToEncoding) :: ([Generic](../base-4.8.2.0/GHC-Generics.html#t:Generic) a, [GToEncoding](Data-Aeson.html#t:GToEncoding) ([Rep](../base-4.8.2.0/GHC-Generics.html#t:Rep) a)) =&gt; [Options](Data-Aeson-Types.html#t:Options) -&gt; a -&gt; [Encoding](Data-Aeson.html#t:Encoding)
-   [genericParseJSON](#v:genericParseJSON) :: ([Generic](../base-4.8.2.0/GHC-Generics.html#t:Generic) a, [GFromJSON](Data-Aeson.html#t:GFromJSON) ([Rep](../base-4.8.2.0/GHC-Generics.html#t:Rep) a)) =&gt; [Options](Data-Aeson-Types.html#t:Options) -&gt; [Value](Data-Aeson.html#t:Value) -&gt; [Parser](Data-Aeson-Types.html#t:Parser) a
-   [defaultOptions](#v:defaultOptions) :: [Options](Data-Aeson-Types.html#t:Options)
-   [withObject](#v:withObject) :: [String](../base-4.8.2.0/Data-String.html#t:String) -&gt; ([Object](Data-Aeson.html#t:Object) -&gt; [Parser](Data-Aeson-Types.html#t:Parser) a) -&gt; [Value](Data-Aeson.html#t:Value) -&gt; [Parser](Data-Aeson-Types.html#t:Parser) a
-   [withText](#v:withText) :: [String](../base-4.8.2.0/Data-String.html#t:String) -&gt; ([Text](../text-1.2.2.1/Data-Text.html#t:Text) -&gt; [Parser](Data-Aeson-Types.html#t:Parser) a) -&gt; [Value](Data-Aeson.html#t:Value) -&gt; [Parser](Data-Aeson-Types.html#t:Parser) a
-   [withArray](#v:withArray) :: [String](../base-4.8.2.0/Data-String.html#t:String) -&gt; ([Array](Data-Aeson.html#t:Array) -&gt; [Parser](Data-Aeson-Types.html#t:Parser) a) -&gt; [Value](Data-Aeson.html#t:Value) -&gt; [Parser](Data-Aeson-Types.html#t:Parser) a
-   [withNumber](#v:withNumber) :: [String](../base-4.8.2.0/Data-String.html#t:String) -&gt; ([Number](../attoparsec-0.13.0.2/Data-Attoparsec-Number.html#t:Number) -&gt; [Parser](Data-Aeson-Types.html#t:Parser) a) -&gt; [Value](Data-Aeson.html#t:Value) -&gt; [Parser](Data-Aeson-Types.html#t:Parser) a
-   [withScientific](#v:withScientific) :: [String](../base-4.8.2.0/Data-String.html#t:String) -&gt; ([Scientific](../scientific-0.3.4.9/Data-Scientific.html#t:Scientific) -&gt; [Parser](Data-Aeson-Types.html#t:Parser) a) -&gt; [Value](Data-Aeson.html#t:Value) -&gt; [Parser](Data-Aeson-Types.html#t:Parser) a
-   [withBool](#v:withBool) :: [String](../base-4.8.2.0/Data-String.html#t:String) -&gt; ([Bool](../base-4.8.2.0/Data-Bool.html#t:Bool) -&gt; [Parser](Data-Aeson-Types.html#t:Parser) a) -&gt; [Value](Data-Aeson.html#t:Value) -&gt; [Parser](Data-Aeson-Types.html#t:Parser) a
-   <span class="keyword">data</span> [Series](#t:Series)
-   [pairs](#v:pairs) :: [Series](Data-Aeson.html#t:Series) -&gt; [Encoding](Data-Aeson.html#t:Encoding)
-   [foldable](#v:foldable) :: ([Foldable](../base-4.8.2.0/Data-Foldable.html#t:Foldable) t, [ToJSON](Data-Aeson.html#t:ToJSON) a) =&gt; t a -&gt; [Encoding](Data-Aeson.html#t:Encoding)
-   [(.:)](#v:.:) :: [FromJSON](Data-Aeson.html#t:FromJSON) a =&gt; [Object](Data-Aeson.html#t:Object) -&gt; [Text](../text-1.2.2.1/Data-Text.html#t:Text) -&gt; [Parser](Data-Aeson-Types.html#t:Parser) a
-   [(.:?)](#v:.:-63-) :: [FromJSON](Data-Aeson.html#t:FromJSON) a =&gt; [Object](Data-Aeson.html#t:Object) -&gt; [Text](../text-1.2.2.1/Data-Text.html#t:Text) -&gt; [Parser](Data-Aeson-Types.html#t:Parser) ([Maybe](../base-4.8.2.0/Data-Maybe.html#t:Maybe) a)
-   [(.:!)](#v:.:-33-) :: [FromJSON](Data-Aeson.html#t:FromJSON) a =&gt; [Object](Data-Aeson.html#t:Object) -&gt; [Text](../text-1.2.2.1/Data-Text.html#t:Text) -&gt; [Parser](Data-Aeson-Types.html#t:Parser) ([Maybe](../base-4.8.2.0/Data-Maybe.html#t:Maybe) a)
-   [(.!=)](#v:.-33--61-) :: [Parser](Data-Aeson-Types.html#t:Parser) ([Maybe](../base-4.8.2.0/Data-Maybe.html#t:Maybe) a) -&gt; a -&gt; [Parser](Data-Aeson-Types.html#t:Parser) a
-   [object](#v:object) :: \[[Pair](Data-Aeson-Types.html#t:Pair)\] -&gt; [Value](Data-Aeson.html#t:Value)
-   [json](#v:json) :: [Parser](../attoparsec-0.13.0.2/Data-Attoparsec-ByteString.html#t:Parser) [Value](Data-Aeson.html#t:Value)
-   [json'](#v:json-39-) :: [Parser](../attoparsec-0.13.0.2/Data-Attoparsec-ByteString.html#t:Parser) [Value](Data-Aeson.html#t:Value)

How to use this library
=======================

This section contains basic information on the different ways to work with data using this library. These range from simple but inflexible, to complex but flexible.

The most common way to use the library is to define a data type, corresponding to some JSON data you want to work with, and then write either a `FromJSON` instance, a to `ToJSON` instance, or both for that type.

For example, given this JSON data:

``` haskell
{ "name": "Joe", "age": 12 }
```

we create a matching data type:

``` haskell
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics

data Person = Person {
      name :: Text
    , age  :: Int
    } deriving (Generic, Show)
```

The `LANGUAGE` pragma and `Generic` instance let us write empty `FromJSON` and `ToJSON` instances for which the compiler will generate sensible default implementations.

``` haskell
instance ToJSON Person where
    -- No need to provide a toJSON implementation.

    -- For efficiency, we write a simple toEncoding implementation, as
    -- the default version uses toJSON.
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Person
    -- No need to provide a parseJSON implementation.
```

We can now encode a value like so:

``` haskell
>>> encode (Person {name = "Joe", age = 12})
"{\"name\":\"Joe\",\"age\":12}"
```

Writing instances by hand
-------------------------

When necessary, we can write `ToJSON` and `FromJSON` instances by hand. This is valuable when the JSON-on-the-wire and Haskell data are different or otherwise need some more carefully managed translation. Let's revisit our JSON data:

``` haskell
{ "name": "Joe", "age": 12 }
```

We once again create a matching data type, without bothering to add a `Generic` instance this time:

``` haskell
data Person = Person {
      name :: Text
    , age  :: Int
    } deriving Show
```

To decode data, we need to define a `FromJSON` instance:

``` haskell
{-# LANGUAGE OverloadedStrings #-}

instance FromJSON Person where
    parseJSON (Object v) = Person <$>
                           v .: "name" <*>
                           v .: "age"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = empty
```

We can now parse the JSON data like so:

``` haskell
>>> decode "{\"name\":\"Joe\",\"age\":12}" :: Maybe Person
Just (Person {name = "Joe", age = 12})
```

To encode data, we need to define a `ToJSON` instance. Let's begin with an instance written entirely by hand.

``` haskell
instance ToJSON Person where
    -- this generates a Value
    toJSON (Person name age) =
        object ["name" .= name, "age" .= age]

    -- this encodes directly to a bytestring Builder
    toEncoding (Person name age) =
        pairs ("name" .= name <> "age" .= age)
```

We can now encode a value like so:

``` haskell
>>> encode (Person {name = "Joe", age = 12})
"{\"name\":\"Joe\",\"age\":12}"
```

There are predefined `FromJSON` and `ToJSON` instances for many types. Here's an example using lists and `Int`s:

``` haskell
>>> decode "[1,2,3]" :: Maybe [Int]
Just [1,2,3]
```

And here's an example using the `Map` type to get a map of `Int`s.

``` haskell
>>> decode "{\"foo\":1,\"bar\":2}" :: Maybe (Map String Int)
Just (fromList [("bar",2),("foo",1)])
```

Working with the AST
--------------------

Sometimes you want to work with JSON data directly, without first converting it to a custom data type. This can be useful if you want to e.g. convert JSON data to YAML data, without knowing what the contents of the original JSON data was. The `Value` type, which is an instance of `FromJSON`, is used to represent an arbitrary JSON AST (abstract syntax tree). Example usage:

``` haskell
>>> decode "{\"foo\": 123}" :: Maybe Value
Just (Object (fromList [("foo",Number 123)]))
```

``` haskell
>>> decode "{\"foo\": [\"abc\",\"def\"]}" :: Maybe Value
Just (Object (fromList [("foo",Array (fromList [String "abc",String "def"]))]))
```

Once you have a `Value` you can write functions to traverse it and make arbitrary transformations.

Decoding to a Haskell value
---------------------------

We can decode to any instance of `FromJSON`:

``` haskell
λ> decode "[1,2,3]" :: Maybe [Int]
Just [1,2,3]
```

Alternatively, there are instances for standard data types, so you can use them directly. For example, use the `Map` type to get a map of `Int`s.

``` haskell
λ> import Data.Map
λ> decode "{\"foo\":1,\"bar\":2}" :: Maybe (Map String Int)
Just (fromList [("bar",2),("foo",1)])
```

Decoding a mixed-type object
----------------------------

The above approach with maps of course will not work for mixed-type objects that don't follow a strict schema, but there are a couple of approaches available for these.

The `Object` type contains JSON objects:

``` haskell
λ> decode "{\"name\":\"Dave\",\"age\":2}" :: Maybe Object
Just (fromList) [("name",String "Dave"),("age",Number 2)]
```

You can extract values from it with a parser using `parse`, `parseEither` or, in this example, `parseMaybe`:

``` haskell
λ> do result <- decode "{\"name\":\"Dave\",\"age\":2}"
      flip parseMaybe result $ \obj -> do
        age <- obj .: "age"
        name <- obj .: "name"
        return (name ++ ": " ++ show (age*2))

Just "Dave: 4"
```

Considering that any type that implements `FromJSON` can be used here, this is quite a powerful way to parse JSON. See the documentation in `FromJSON` for how to implement this class for your own data types.

The downside is that you have to write the parser yourself; the upside is that you have complete control over the way the JSON is parsed.

Encoding and decoding
=====================

Decoding is a two-step process.

-   When decoding a value, the process is reversed: the bytes are converted to a `Value`, then the `FromJSON` class is used to convert to the desired type.

There are two ways to encode a value.

-   Convert to a `Value` using `toJSON`, then possibly further encode. This was the only method available in aeson 0.9 and earlier.
-   Directly encode (to what will become a `ByteString`) using `toEncoding`. This is much more efficient (about 3x faster, and less memory intensive besides), but is only available in aeson 0.10 and newer.

For convenience, the `encode` and `decode` functions combine both steps.

Direct encoding
---------------

In older versions of this library, encoding a Haskell value involved converting to an intermediate `Value`, then encoding that.

A "direct" encoder converts straight from a source Haskell value to a `ByteString` without constructing an intermediate `Value`. This approach is faster than `toJSON`, and allocates less memory. The `toEncoding` method makes it possible to implement direct encoding with low memory overhead.

To complicate matters, the default implementation of `toEncoding` uses `toJSON`. Why? The `toEncoding` method was added to this library much more recently than `toJSON`. Using `toJSON` ensures that packages written against older versions of this library will compile and produce correct output, but they will not see any speedup from direct encoding.

To write a minimal implementation of direct encoding, your type must implement GHC's `Generic` class, and your code should look like this:

``` haskell
    toEncoding = genericToEncoding defaultOptions
```

What if you have more elaborate encoding needs? For example, perhaps you need to change the names of object keys, omit parts of a value.

To encode to a JSON "object", use the `pairs` function.

``` haskell
    toEncoding (Person name age) =
        pairs ("name" .= name <> "age" .= age)
```

Any container type that implements `Foldable` can be encoded to a JSON "array" using `foldable`.

``` haskell
> import Data.Sequence as Seq
> encode (Seq.fromList [1,2,3])
"[1,2,3]"
```

<a href="" class="def">decode</a> :: [FromJSON](Data-Aeson.html#t:FromJSON) a =&gt; [ByteString](../bytestring-0.10.6.0/Data-ByteString-Lazy.html#t:ByteString) -&gt; [Maybe](../base-4.8.2.0/Data-Maybe.html#t:Maybe) a <a href="src/Data-Aeson.html#decode" class="link">Source</a>

Efficiently deserialize a JSON value from a lazy `ByteString`. If this fails due to incomplete or invalid input, `Nothing` is returned.

The input must consist solely of a JSON document, with no trailing data except for whitespace.

This function parses immediately, but defers conversion. See `json` for details.

<a href="" class="def">decode'</a> :: [FromJSON](Data-Aeson.html#t:FromJSON) a =&gt; [ByteString](../bytestring-0.10.6.0/Data-ByteString-Lazy.html#t:ByteString) -&gt; [Maybe](../base-4.8.2.0/Data-Maybe.html#t:Maybe) a <a href="src/Data-Aeson.html#decode%27" class="link">Source</a>

Efficiently deserialize a JSON value from a lazy `ByteString`. If this fails due to incomplete or invalid input, `Nothing` is returned.

The input must consist solely of a JSON document, with no trailing data except for whitespace.

This function parses and performs conversion immediately. See `json'` for details.

<a href="" class="def">eitherDecode</a> :: [FromJSON](Data-Aeson.html#t:FromJSON) a =&gt; [ByteString](../bytestring-0.10.6.0/Data-ByteString-Lazy.html#t:ByteString) -&gt; [Either](../base-4.8.2.0/Data-Either.html#t:Either) [String](../base-4.8.2.0/Data-String.html#t:String) a <a href="src/Data-Aeson.html#eitherDecode" class="link">Source</a>

Like `decode` but returns an error message when decoding fails.

<a href="" class="def">eitherDecode'</a> :: [FromJSON](Data-Aeson.html#t:FromJSON) a =&gt; [ByteString](../bytestring-0.10.6.0/Data-ByteString-Lazy.html#t:ByteString) -&gt; [Either](../base-4.8.2.0/Data-Either.html#t:Either) [String](../base-4.8.2.0/Data-String.html#t:String) a <a href="src/Data-Aeson.html#eitherDecode%27" class="link">Source</a>

Like `decode'` but returns an error message when decoding fails.

<a href="" class="def">encode</a> :: [ToJSON](Data-Aeson.html#t:ToJSON) a =&gt; a -&gt; [ByteString](../bytestring-0.10.6.0/Data-ByteString-Lazy.html#t:ByteString) <a href="src/Data-Aeson-Encode-Functions.html#encode" class="link">Source</a>

Efficiently serialize a JSON value as a lazy `ByteString`.

This is implemented in terms of the `ToJSON` class's `toEncoding` method.

Variants for strict bytestrings
-------------------------------

<a href="" class="def">decodeStrict</a> :: [FromJSON](Data-Aeson.html#t:FromJSON) a =&gt; [ByteString](../bytestring-0.10.6.0/Data-ByteString.html#t:ByteString) -&gt; [Maybe](../base-4.8.2.0/Data-Maybe.html#t:Maybe) a <a href="src/Data-Aeson.html#decodeStrict" class="link">Source</a>

Efficiently deserialize a JSON value from a strict `ByteString`. If this fails due to incomplete or invalid input, `Nothing` is returned.

The input must consist solely of a JSON document, with no trailing data except for whitespace.

This function parses immediately, but defers conversion. See `json` for details.

<a href="" class="def">decodeStrict'</a> :: [FromJSON](Data-Aeson.html#t:FromJSON) a =&gt; [ByteString](../bytestring-0.10.6.0/Data-ByteString.html#t:ByteString) -&gt; [Maybe](../base-4.8.2.0/Data-Maybe.html#t:Maybe) a <a href="src/Data-Aeson.html#decodeStrict%27" class="link">Source</a>

Efficiently deserialize a JSON value from a lazy `ByteString`. If this fails due to incomplete or invalid input, `Nothing` is returned.

The input must consist solely of a JSON document, with no trailing data except for whitespace.

This function parses and performs conversion immediately. See `json'` for details.

<a href="" class="def">eitherDecodeStrict</a> :: [FromJSON](Data-Aeson.html#t:FromJSON) a =&gt; [ByteString](../bytestring-0.10.6.0/Data-ByteString.html#t:ByteString) -&gt; [Either](../base-4.8.2.0/Data-Either.html#t:Either) [String](../base-4.8.2.0/Data-String.html#t:String) a <a href="src/Data-Aeson.html#eitherDecodeStrict" class="link">Source</a>

Like `decodeStrict` but returns an error message when decoding fails.

<a href="" class="def">eitherDecodeStrict'</a> :: [FromJSON](Data-Aeson.html#t:FromJSON) a =&gt; [ByteString](../bytestring-0.10.6.0/Data-ByteString.html#t:ByteString) -&gt; [Either](../base-4.8.2.0/Data-Either.html#t:Either) [String](../base-4.8.2.0/Data-String.html#t:String) a <a href="src/Data-Aeson.html#eitherDecodeStrict%27" class="link">Source</a>

Like `decodeStrict'` but returns an error message when decoding fails.

Core JSON types
===============

<span class="keyword">data</span> <a href="" class="def">Value</a> <a href="src/Data-Aeson-Types-Internal.html#Value" class="link">Source</a>

A JSON value represented as a Haskell value.

Constructors

|                                                                                                          |     |
|----------------------------------------------------------------------------------------------------------|-----|
| <a href="" class="def">Object</a> ![Object](Data-Aeson.html#t:Object)                                    |     |
| <a href="" class="def">Array</a> ![Array](Data-Aeson.html#t:Array)                                       |     |
| <a href="" class="def">String</a> ![Text](../text-1.2.2.1/Data-Text.html#t:Text)                         |     |
| <a href="" class="def">Number</a> ![Scientific](../scientific-0.3.4.9/Data-Scientific.html#t:Scientific) |     |
| <a href="" class="def">Bool</a> ![Bool](../base-4.8.2.0/Data-Bool.html#t:Bool)                           |     |
| <a href="" class="def">Null</a>                                                                          |     |

Instances

|                                                                                                                                                                                                                           |     |
|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-----|
| <span class="inst-left">[Eq](../base-4.8.2.0/Data-Eq.html#t:Eq) [Value](Data-Aeson.html#t:Value)</span> <a href="src/Data-Aeson-Types-Internal.html#line-339" class="link">Source</a>                                     |     |
| <span class="inst-left">[Data](../base-4.8.2.0/Data-Data.html#t:Data) [Value](Data-Aeson.html#t:Value)</span> <a href="src/Data-Aeson-Types-Internal.html#line-339" class="link">Source</a>                               |     |
| <span class="inst-left">[Read](../base-4.8.2.0/Text-Read.html#t:Read) [Value](Data-Aeson.html#t:Value)</span> <a href="src/Data-Aeson-Types-Internal.html#line-339" class="link">Source</a>                               |     |
| <span class="inst-left">[Show](../base-4.8.2.0/Text-Show.html#t:Show) [Value](Data-Aeson.html#t:Value)</span> <a href="src/Data-Aeson-Types-Internal.html#line-339" class="link">Source</a>                               |     |
| <span class="inst-left">[IsString](../base-4.8.2.0/Data-String.html#t:IsString) [Value](Data-Aeson.html#t:Value)</span> <a href="src/Data-Aeson-Types-Internal.html#line-408" class="link">Source</a>                     |     |
| <span class="inst-left">[NFData](../deepseq-1.4.1.1/Control-DeepSeq.html#t:NFData) [Value](Data-Aeson.html#t:Value)</span> <a href="src/Data-Aeson-Types-Internal.html#line-400" class="link">Source</a>                  |     |
| <span class="inst-left">[Hashable](../hashable-1.2.4.0/Data-Hashable.html#t:Hashable) [Value](Data-Aeson.html#t:Value)</span> <a href="src/Data-Aeson-Types-Internal.html#line-428" class="link">Source</a>               |     |
| <span class="inst-left">[Lift](../template-haskell-2.10.0.0/Language-Haskell-TH-Syntax.html#t:Lift) [Value](Data-Aeson.html#t:Value)</span> <a href="src/Data-Aeson-Types-Internal.html#line-432" class="link">Source</a> |     |

<span class="keyword">data</span> <a href="" class="def">Encoding</a> <a href="src/Data-Aeson-Types-Internal.html#Encoding" class="link">Source</a>

An encoding of a JSON value.

Instances

|                                                                                                                                                                                                                       |     |
|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-----|
| <span class="inst-left">[Eq](../base-4.8.2.0/Data-Eq.html#t:Eq) [Encoding](Data-Aeson.html#t:Encoding)</span> <a href="src/Data-Aeson-Types-Internal.html#line-357" class="link">Source</a>                           |     |
| <span class="inst-left">[Ord](../base-4.8.2.0/Data-Ord.html#t:Ord) [Encoding](Data-Aeson.html#t:Encoding)</span> <a href="src/Data-Aeson-Types-Internal.html#line-360" class="link">Source</a>                        |     |
| <span class="inst-left">[Show](../base-4.8.2.0/Text-Show.html#t:Show) [Encoding](Data-Aeson.html#t:Encoding)</span> <a href="src/Data-Aeson-Types-Internal.html#line-354" class="link">Source</a>                     |     |
| <span class="inst-left">[Monoid](../base-4.8.2.0/Data-Monoid.html#t:Monoid) [Encoding](Data-Aeson.html#t:Encoding)</span> <a href="src/Data-Aeson-Types-Internal.html#line-345" class="link">Source</a>               |     |
| <span class="inst-left">[Semigroup](../semigroups-0.18.1/Data-Semigroup.html#t:Semigroup) [Encoding](Data-Aeson.html#t:Encoding)</span> <a href="src/Data-Aeson-Types-Internal.html#line-345" class="link">Source</a> |     |

<a href="" class="def">fromEncoding</a> :: [Encoding](Data-Aeson.html#t:Encoding) -&gt; [Builder](../bytestring-0.10.6.0/Data-ByteString-Builder.html#t:Builder) <a href="src/Data-Aeson-Types-Internal.html#fromEncoding" class="link">Source</a>

Acquire the underlying bytestring builder.

<span class="keyword">type</span> <a href="" class="def">Array</a> = [Vector](../vector-0.11.0.0/Data-Vector.html#t:Vector) [Value](Data-Aeson.html#t:Value) <a href="src/Data-Aeson-Types-Internal.html#Array" class="link">Source</a>

A JSON "array" (sequence).

<span class="keyword">type</span> <a href="" class="def">Object</a> = [HashMap](../unordered-containers-0.2.7.1/Data-HashMap-Strict.html#t:HashMap) [Text](../text-1.2.2.1/Data-Text.html#t:Text) [Value](Data-Aeson.html#t:Value) <a href="src/Data-Aeson-Types-Internal.html#Object" class="link">Source</a>

A JSON "object" (key/value map).

Convenience types
=================

<span class="keyword">newtype</span> <a href="" class="def">DotNetTime</a> <a href="src/Data-Aeson-Types-Internal.html#DotNetTime" class="link">Source</a>

A newtype wrapper for `UTCTime` that uses the same non-standard serialization format as Microsoft .NET, whose [System.DateTime](https://msdn.microsoft.com/en-us/library/system.datetime(v=vs.110).aspx) type is by default serialized to JSON as in the following example:

``` haskell
/Date(1302547608878)/
```

The number represents milliseconds since the Unix epoch.

Constructors

<a href="" class="def">DotNetTime</a>
 
Fields

<a href="" class="def">fromDotNetTime</a> :: [UTCTime](../time-1.5.0.1/Data-Time-Clock.html#t:UTCTime)  
Acquire the underlying value.

Instances

|                                                                                                                                                                                                                          |     |
|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-----|
| <span class="inst-left">[Eq](../base-4.8.2.0/Data-Eq.html#t:Eq) [DotNetTime](Data-Aeson.html#t:DotNetTime)</span> <a href="src/Data-Aeson-Types-Internal.html#line-398" class="link">Source</a>                          |     |
| <span class="inst-left">[Ord](../base-4.8.2.0/Data-Ord.html#t:Ord) [DotNetTime](Data-Aeson.html#t:DotNetTime)</span> <a href="src/Data-Aeson-Types-Internal.html#line-398" class="link">Source</a>                       |     |
| <span class="inst-left">[Read](../base-4.8.2.0/Text-Read.html#t:Read) [DotNetTime](Data-Aeson.html#t:DotNetTime)</span> <a href="src/Data-Aeson-Types-Internal.html#line-398" class="link">Source</a>                    |     |
| <span class="inst-left">[Show](../base-4.8.2.0/Text-Show.html#t:Show) [DotNetTime](Data-Aeson.html#t:DotNetTime)</span> <a href="src/Data-Aeson-Types-Internal.html#line-398" class="link">Source</a>                    |     |
| <span class="inst-left">[FormatTime](../time-1.5.0.1/Data-Time-Format.html#t:FormatTime) [DotNetTime](Data-Aeson.html#t:DotNetTime)</span> <a href="src/Data-Aeson-Types-Internal.html#line-398" class="link">Source</a> |     |

Type conversion
===============

<span class="keyword">class</span> <a href="" class="def">FromJSON</a> a <span class="keyword">where</span> <a href="src/Data-Aeson-Types-Class.html#FromJSON" class="link">Source</a>

A type that can be converted from JSON, with the possibility of failure.

In many cases, you can get the compiler to generate parsing code for you (see below). To begin, let's cover writing an instance by hand.

There are various reasons a conversion could fail. For example, an `Object` could be missing a required key, an `Array` could be of the wrong size, or a value could be of an incompatible type.

The basic ways to signal a failed conversion are as follows:

-   `empty` and `mzero` work, but are terse and uninformative
-   `fail` yields a custom error message
-   `typeMismatch` produces an informative message for cases when the value encountered is not of the expected type

An example type and instance:

``` haskell
-- Allow ourselves to write Text literals.
{-# LANGUAGE OverloadedStrings #-}

data Coord = Coord { x :: Double, y :: Double }

instance FromJSON Coord where
  parseJSON (Object v) = Coord    <$>
                         v .: "x" <*>
                         v .: "y"

  -- We do not expect a non-Object value here.
  -- We could use mzero to fail, but typeMismatch
  -- gives a much more informative error message.
  parseJSON invalid    = typeMismatch "Coord" invalid
```

Instead of manually writing your `FromJSON` instance, there are two options to do it automatically:

-   [Data.Aeson.TH](Data-Aeson-TH.html) provides Template Haskell functions which will derive an instance at compile time. The generated instance is optimized for your type so will probably be more efficient than the following two options:
-   The compiler can provide a default generic implementation for `parseJSON`.

To use the second, simply add a `deriving Generic` clause to your datatype and declare a `FromJSON` instance for your datatype without giving a definition for `parseJSON`.

For example, the previous example can be simplified to just:

``` haskell
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics

data Coord = Coord { x :: Double, y :: Double } deriving Generic

instance FromJSON Coord
```

If `DefaultSignatures` doesn't give exactly the results you want, you can customize the generic decoding with only a tiny amount of effort, using `genericParseJSON` with your preferred `Options`:

``` haskell
instance FromJSON Coord where
    parseJSON = genericParseJSON defaultOptions
```

Minimal complete definition

Nothing

Methods

<a href="" class="def">parseJSON</a> :: [Value](Data-Aeson.html#t:Value) -&gt; [Parser](Data-Aeson-Types.html#t:Parser) a <a href="src/Data-Aeson-Types-Class.html#parseJSON" class="link">Source</a>

<span class="keyword">data</span> <a href="" class="def">Result</a> a <a href="src/Data-Aeson-Types-Internal.html#Result" class="link">Source</a>

The result of running a `Parser`.

Constructors

|                                                                                      |     |
|--------------------------------------------------------------------------------------|-----|
| <a href="" class="def">Error</a> [String](../base-4.8.2.0/Data-String.html#t:String) |     |
| <a href="" class="def">Success</a> a                                                 |     |

Instances

|                                                                                                                                                                                                                                                                                   |     |
|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-----|
| <span class="inst-left">[Monad](../base-4.8.2.0/Control-Monad.html#t:Monad) [Result](Data-Aeson.html#t:Result)</span> <a href="src/Data-Aeson-Types-Internal.html#line-157" class="link">Source</a>                                                                               |     |
| <span class="inst-left">[Functor](../base-4.8.2.0/Data-Functor.html#t:Functor) [Result](Data-Aeson.html#t:Result)</span> <a href="src/Data-Aeson-Types-Internal.html#line-137" class="link">Source</a>                                                                            |     |
| <span class="inst-left">[Applicative](../base-4.8.2.0/Control-Applicative.html#t:Applicative) [Result](Data-Aeson.html#t:Result)</span> <a href="src/Data-Aeson-Types-Internal.html#line-178" class="link">Source</a>                                                             |     |
| <span class="inst-left">[Foldable](../base-4.8.2.0/Data-Foldable.html#t:Foldable) [Result](Data-Aeson.html#t:Result)</span> <a href="src/Data-Aeson-Types-Internal.html#line-239" class="link">Source</a>                                                                         |     |
| <span class="inst-left">[Traversable](../base-4.8.2.0/Data-Traversable.html#t:Traversable) [Result](Data-Aeson.html#t:Result)</span> <a href="src/Data-Aeson-Types-Internal.html#line-253" class="link">Source</a>                                                                |     |
| <span class="inst-left">[Alternative](../base-4.8.2.0/Control-Applicative.html#t:Alternative) [Result](Data-Aeson.html#t:Result)</span> <a href="src/Data-Aeson-Types-Internal.html#line-204" class="link">Source</a>                                                             |     |
| <span class="inst-left">[MonadPlus](../base-4.8.2.0/Control-Monad.html#t:MonadPlus) [Result](Data-Aeson.html#t:Result)</span> <a href="src/Data-Aeson-Types-Internal.html#line-191" class="link">Source</a>                                                                       |     |
| <span class="inst-left">[MonadFail](../fail-4.9.0.0/Control-Monad-Fail.html#t:MonadFail) [Result](Data-Aeson.html#t:Result)</span> <a href="src/Data-Aeson-Types-Internal.html#line-168" class="link">Source</a>                                                                  |     |
| <span class="inst-left">[Eq](../base-4.8.2.0/Data-Eq.html#t:Eq) a =&gt; [Eq](../base-4.8.2.0/Data-Eq.html#t:Eq) ([Result](Data-Aeson.html#t:Result) a)</span> <a href="src/Data-Aeson-Types-Internal.html#line-118" class="link">Source</a>                                       |     |
| <span class="inst-left">[Show](../base-4.8.2.0/Text-Show.html#t:Show) a =&gt; [Show](../base-4.8.2.0/Text-Show.html#t:Show) ([Result](Data-Aeson.html#t:Result) a)</span> <a href="src/Data-Aeson-Types-Internal.html#line-118" class="link">Source</a>                           |     |
| <span class="inst-left">[Monoid](../base-4.8.2.0/Data-Monoid.html#t:Monoid) ([Result](Data-Aeson.html#t:Result) a)</span> <a href="src/Data-Aeson-Types-Internal.html#line-224" class="link">Source</a>                                                                           |     |
| <span class="inst-left">[NFData](../deepseq-1.4.1.1/Control-DeepSeq.html#t:NFData) a =&gt; [NFData](../deepseq-1.4.1.1/Control-DeepSeq.html#t:NFData) ([Result](Data-Aeson.html#t:Result) a)</span> <a href="src/Data-Aeson-Types-Internal.html#line-128" class="link">Source</a> |     |
| <span class="inst-left">[Semigroup](../semigroups-0.18.1/Data-Semigroup.html#t:Semigroup) ([Result](Data-Aeson.html#t:Result) a)</span> <a href="src/Data-Aeson-Types-Internal.html#line-220" class="link">Source</a>                                                             |     |

<a href="" class="def">fromJSON</a> :: [FromJSON](Data-Aeson.html#t:FromJSON) a =&gt; [Value](Data-Aeson.html#t:Value) -&gt; [Result](Data-Aeson.html#t:Result) a <a href="src/Data-Aeson-Types-Instances.html#fromJSON" class="link">Source</a>

Convert a value from JSON, failing if the types do not match.

<span class="keyword">class</span> <a href="" class="def">ToJSON</a> a <span class="keyword">where</span> <a href="src/Data-Aeson-Types-Class.html#ToJSON" class="link">Source</a>

A type that can be converted to JSON.

An example type and instance:

``` haskell
-- Allow ourselves to write Text literals.
{-# LANGUAGE OverloadedStrings #-}

data Coord = Coord { x :: Double, y :: Double }

instance ToJSON Coord where
  toJSON (Coord x y) = object ["x" .= x, "y" .= y]

  toEncoding (Coord x y) = pairs ("x" .= x <> "y" .= y)
```

Instead of manually writing your `ToJSON` instance, there are two options to do it automatically:

-   [Data.Aeson.TH](Data-Aeson-TH.html) provides Template Haskell functions which will derive an instance at compile time. The generated instance is optimized for your type so will probably be more efficient than the following two options:
-   The compiler can provide a default generic implementation for `toJSON`.

To use the second, simply add a `deriving Generic` clause to your datatype and declare a `ToJSON` instance for your datatype without giving definitions for `toJSON` or `toEncoding`.

For example, the previous example can be simplified to a more minimal instance:

``` haskell
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics

data Coord = Coord { x :: Double, y :: Double } deriving Generic

instance ToJSON Coord where
    toEncoding = genericToEncoding defaultOptions
```

Why do we provide an implementation for `toEncoding` here? The `toEncoding` function is a relatively new addition to this class. To allow users of older versions of this library to upgrade without having to edit all of their instances or encounter surprising incompatibilities, the default implementation of `toEncoding` uses `toJSON`. This produces correct results, but since it performs an intermediate conversion to a `Value`, it will be less efficient than directly emitting an `Encoding`. Our one-liner definition of `toEncoding` above bypasses the intermediate `Value`.

If `DefaultSignatures` doesn't give exactly the results you want, you can customize the generic encoding with only a tiny amount of effort, using `genericToJSON` and `genericToEncoding` with your preferred `Options`:

``` haskell
instance ToJSON Coord where
    toJSON     = genericToJSON defaultOptions
    toEncoding = genericToEncoding defaultOptions
```

Minimal complete definition

Nothing

Methods

<a href="" class="def">toJSON</a> :: a -&gt; [Value](Data-Aeson.html#t:Value) <a href="src/Data-Aeson-Types-Class.html#toJSON" class="link">Source</a>

Convert a Haskell value to a JSON-friendly intermediate type.

<a href="" class="def">toEncoding</a> :: a -&gt; [Encoding](Data-Aeson.html#t:Encoding) <a href="src/Data-Aeson-Types-Class.html#toEncoding" class="link">Source</a>

Encode a Haskell value as JSON.

The default implementation of this method creates an intermediate `Value` using `toJSON`. This provides source-level compatibility for people upgrading from older versions of this library, but obviously offers no performance advantage.

To benefit from direct encoding, you *must* provide an implementation for this method. The easiest way to do so is by having your types implement `Generic` using the `DeriveGeneric` extension, and then have GHC generate a method body as follows.

``` haskell
instance ToJSON Coord where
    toEncoding = genericToEncoding defaultOptions
```

<span class="keyword">class</span> <a href="" class="def">KeyValue</a> kv <span class="keyword">where</span> <a href="src/Data-Aeson-Types-Class.html#KeyValue" class="link">Source</a>

A key-value pair for encoding a JSON object.

Methods

<a href="" class="def">(.=)</a> :: [ToJSON](Data-Aeson.html#t:ToJSON) v =&gt; [Text](../text-1.2.2.1/Data-Text.html#t:Text) -&gt; v -&gt; kv <span class="fixity">infixr 8</span><span class="rightedge"></span> <a href="src/Data-Aeson-Types-Class.html#.%3D" class="link">Source</a>

Generic JSON classes and options
--------------------------------

<span class="keyword">class</span> <a href="" class="def">GFromJSON</a> f <span class="keyword">where</span> <a href="src/Data-Aeson-Types-Class.html#GFromJSON" class="link">Source</a>

Class of generic representation types (`Rep`) that can be converted from JSON.

Methods

<a href="" class="def">gParseJSON</a> :: [Options](Data-Aeson-Types.html#t:Options) -&gt; [Value](Data-Aeson.html#t:Value) -&gt; [Parser](Data-Aeson-Types.html#t:Parser) (f a) <a href="src/Data-Aeson-Types-Class.html#gParseJSON" class="link">Source</a>

This method (applied to `defaultOptions`) is used as the default generic implementation of `parseJSON`.

<span class="keyword">class</span> <a href="" class="def">GToJSON</a> f <span class="keyword">where</span> <a href="src/Data-Aeson-Types-Class.html#GToJSON" class="link">Source</a>

Class of generic representation types (`Rep`) that can be converted to JSON.

Methods

<a href="" class="def">gToJSON</a> :: [Options](Data-Aeson-Types.html#t:Options) -&gt; f a -&gt; [Value](Data-Aeson.html#t:Value) <a href="src/Data-Aeson-Types-Class.html#gToJSON" class="link">Source</a>

This method (applied to `defaultOptions`) is used as the default generic implementation of `toJSON`.

<span class="keyword">class</span> <a href="" class="def">GToEncoding</a> f <span class="keyword">where</span> <a href="src/Data-Aeson-Types-Class.html#GToEncoding" class="link">Source</a>

Class of generic representation types (`Rep`) that can be converted to a JSON `Encoding`.

Methods

<a href="" class="def">gToEncoding</a> :: [Options](Data-Aeson-Types.html#t:Options) -&gt; f a -&gt; [Encoding](Data-Aeson.html#t:Encoding) <a href="src/Data-Aeson-Types-Class.html#gToEncoding" class="link">Source</a>

This method (applied to `defaultOptions`) can be used as the default generic implementation of `toEncoding`.

<a href="" class="def">genericToJSON</a> :: ([Generic](../base-4.8.2.0/GHC-Generics.html#t:Generic) a, [GToJSON](Data-Aeson.html#t:GToJSON) ([Rep](../base-4.8.2.0/GHC-Generics.html#t:Rep) a)) =&gt; [Options](Data-Aeson-Types.html#t:Options) -&gt; a -&gt; [Value](Data-Aeson.html#t:Value) <a href="src/Data-Aeson-Types-Class.html#genericToJSON" class="link">Source</a>

A configurable generic JSON creator. This function applied to `defaultOptions` is used as the default for `toJSON` when the type is an instance of `Generic`.

<a href="" class="def">genericToEncoding</a> :: ([Generic](../base-4.8.2.0/GHC-Generics.html#t:Generic) a, [GToEncoding](Data-Aeson.html#t:GToEncoding) ([Rep](../base-4.8.2.0/GHC-Generics.html#t:Rep) a)) =&gt; [Options](Data-Aeson-Types.html#t:Options) -&gt; a -&gt; [Encoding](Data-Aeson.html#t:Encoding) <a href="src/Data-Aeson-Types-Class.html#genericToEncoding" class="link">Source</a>

A configurable generic JSON encoder. This function applied to `defaultOptions` is used as the default for `toEncoding` when the type is an instance of `Generic`.

<a href="" class="def">genericParseJSON</a> :: ([Generic](../base-4.8.2.0/GHC-Generics.html#t:Generic) a, [GFromJSON](Data-Aeson.html#t:GFromJSON) ([Rep](../base-4.8.2.0/GHC-Generics.html#t:Rep) a)) =&gt; [Options](Data-Aeson-Types.html#t:Options) -&gt; [Value](Data-Aeson.html#t:Value) -&gt; [Parser](Data-Aeson-Types.html#t:Parser) a <a href="src/Data-Aeson-Types-Class.html#genericParseJSON" class="link">Source</a>

A configurable generic JSON decoder. This function applied to `defaultOptions` is used as the default for `parseJSON` when the type is an instance of `Generic`.

<a href="" class="def">defaultOptions</a> :: [Options](Data-Aeson-Types.html#t:Options) <a href="src/Data-Aeson-Types-Internal.html#defaultOptions" class="link">Source</a>

Default encoding `Options`:

``` haskell
Options
{ fieldLabelModifier      = id
, constructorTagModifier  = id
, allNullaryToStringTag   = True
, omitNothingFields       = False
, sumEncoding             = defaultTaggedObject
, unwrapUnaryRecords      = False
}
```

Inspecting `Value`s
===================

<a href="" class="def">withObject</a> :: [String](../base-4.8.2.0/Data-String.html#t:String) -&gt; ([Object](Data-Aeson.html#t:Object) -&gt; [Parser](Data-Aeson-Types.html#t:Parser) a) -&gt; [Value](Data-Aeson.html#t:Value) -&gt; [Parser](Data-Aeson-Types.html#t:Parser) a <a href="src/Data-Aeson-Types-Instances.html#withObject" class="link">Source</a>

`withObject expected f value` applies `f` to the `Object` when `value` is an `Object` and fails using `typeMismatch` expected otherwise.

<a href="" class="def">withText</a> :: [String](../base-4.8.2.0/Data-String.html#t:String) -&gt; ([Text](../text-1.2.2.1/Data-Text.html#t:Text) -&gt; [Parser](Data-Aeson-Types.html#t:Parser) a) -&gt; [Value](Data-Aeson.html#t:Value) -&gt; [Parser](Data-Aeson-Types.html#t:Parser) a <a href="src/Data-Aeson-Types-Instances.html#withText" class="link">Source</a>

`withText expected f value` applies `f` to the `Text` when `value` is a `String` and fails using `typeMismatch` expected otherwise.

<a href="" class="def">withArray</a> :: [String](../base-4.8.2.0/Data-String.html#t:String) -&gt; ([Array](Data-Aeson.html#t:Array) -&gt; [Parser](Data-Aeson-Types.html#t:Parser) a) -&gt; [Value](Data-Aeson.html#t:Value) -&gt; [Parser](Data-Aeson-Types.html#t:Parser) a <a href="src/Data-Aeson-Types-Instances.html#withArray" class="link">Source</a>

`withArray expected f value` applies `f` to the `Array` when `value` is an `Array` and fails using `typeMismatch` expected otherwise.

<a href="" class="def">withNumber</a> :: [String](../base-4.8.2.0/Data-String.html#t:String) -&gt; ([Number](../attoparsec-0.13.0.2/Data-Attoparsec-Number.html#t:Number) -&gt; [Parser](Data-Aeson-Types.html#t:Parser) a) -&gt; [Value](Data-Aeson.html#t:Value) -&gt; [Parser](Data-Aeson-Types.html#t:Parser) a <a href="src/Data-Aeson-Types-Instances.html#withNumber" class="link">Source</a>

Deprecated: Use withScientific instead

`withNumber expected f value` applies `f` to the `Number` when `value` is a `Number`. and fails using `typeMismatch` expected otherwise.

<a href="" class="def">withScientific</a> :: [String](../base-4.8.2.0/Data-String.html#t:String) -&gt; ([Scientific](../scientific-0.3.4.9/Data-Scientific.html#t:Scientific) -&gt; [Parser](Data-Aeson-Types.html#t:Parser) a) -&gt; [Value](Data-Aeson.html#t:Value) -&gt; [Parser](Data-Aeson-Types.html#t:Parser) a <a href="src/Data-Aeson-Types-Instances.html#withScientific" class="link">Source</a>

`withScientific expected f value` applies `f` to the `Scientific` number when `value` is a `Number`. and fails using `typeMismatch` expected otherwise.

<a href="" class="def">withBool</a> :: [String](../base-4.8.2.0/Data-String.html#t:String) -&gt; ([Bool](../base-4.8.2.0/Data-Bool.html#t:Bool) -&gt; [Parser](Data-Aeson-Types.html#t:Parser) a) -&gt; [Value](Data-Aeson.html#t:Value) -&gt; [Parser](Data-Aeson-Types.html#t:Parser) a <a href="src/Data-Aeson-Types-Instances.html#withBool" class="link">Source</a>

`withBool expected f value` applies `f` to the `Bool` when `value` is a `Bool` and fails using `typeMismatch` expected otherwise.

Constructors and accessors
==========================

<span class="keyword">data</span> <a href="" class="def">Series</a> <a href="src/Data-Aeson-Types-Internal.html#Series" class="link">Source</a>

A series of values that, when encoded, should be separated by commas. Since 0.11.0.0, the `.=` operator is overloaded to create either `(Text, Value)` or `Series`. You can use Series when encoding directly to a bytestring builder as in the following example:

``` haskell
toEncoding (Person name age) = pairs ("name" .= name <> "age" .= age)
```

Instances

|                                                                                                                                                                                                                   |     |
|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-----|
| <span class="inst-left">[Monoid](../base-4.8.2.0/Data-Monoid.html#t:Monoid) [Series](Data-Aeson.html#t:Series)</span> <a href="src/Data-Aeson-Types-Internal.html#line-383" class="link">Source</a>               |     |
| <span class="inst-left">[Semigroup](../semigroups-0.18.1/Data-Semigroup.html#t:Semigroup) [Series](Data-Aeson.html#t:Series)</span> <a href="src/Data-Aeson-Types-Internal.html#line-375" class="link">Source</a> |     |

<a href="" class="def">pairs</a> :: [Series](Data-Aeson.html#t:Series) -&gt; [Encoding](Data-Aeson.html#t:Encoding) <a href="src/Data-Aeson-Encode-Functions.html#pairs" class="link">Source</a>

Encode a series of key/value pairs, separated by commas.

<a href="" class="def">foldable</a> :: ([Foldable](../base-4.8.2.0/Data-Foldable.html#t:Foldable) t, [ToJSON](Data-Aeson.html#t:ToJSON) a) =&gt; t a -&gt; [Encoding](Data-Aeson.html#t:Encoding) <a href="src/Data-Aeson-Encode-Functions.html#foldable" class="link">Source</a>

Encode a `Foldable` as a JSON array.

<a href="" class="def">(.:)</a> :: [FromJSON](Data-Aeson.html#t:FromJSON) a =&gt; [Object](Data-Aeson.html#t:Object) -&gt; [Text](../text-1.2.2.1/Data-Text.html#t:Text) -&gt; [Parser](Data-Aeson-Types.html#t:Parser) a <a href="src/Data-Aeson-Types-Instances.html#.%3A" class="link">Source</a>

Retrieve the value associated with the given key of an `Object`. The result is `empty` if the key is not present or the value cannot be converted to the desired type.

This accessor is appropriate if the key and value *must* be present in an object for it to be valid. If the key and value are optional, use `.:?` instead.

<a href="" class="def">(.:?)</a> :: [FromJSON](Data-Aeson.html#t:FromJSON) a =&gt; [Object](Data-Aeson.html#t:Object) -&gt; [Text](../text-1.2.2.1/Data-Text.html#t:Text) -&gt; [Parser](Data-Aeson-Types.html#t:Parser) ([Maybe](../base-4.8.2.0/Data-Maybe.html#t:Maybe) a) <a href="src/Data-Aeson-Types-Instances.html#.%3A%3F" class="link">Source</a>

Retrieve the value associated with the given key of an `Object`. The result is `Nothing` if the key is not present, or `empty` if the value cannot be converted to the desired type.

This accessor is most useful if the key and value can be absent from an object without affecting its validity. If the key and value are mandatory, use `.:` instead.

<a href="" class="def">(.:!)</a> :: [FromJSON](Data-Aeson.html#t:FromJSON) a =&gt; [Object](Data-Aeson.html#t:Object) -&gt; [Text](../text-1.2.2.1/Data-Text.html#t:Text) -&gt; [Parser](Data-Aeson-Types.html#t:Parser) ([Maybe](../base-4.8.2.0/Data-Maybe.html#t:Maybe) a) <a href="src/Data-Aeson-Types-Instances.html#.%3A%21" class="link">Source</a>

Like `.:?`, but the resulting parser will fail, if the key is present but is `Null`.

<a href="" class="def">(.!=)</a> :: [Parser](Data-Aeson-Types.html#t:Parser) ([Maybe](../base-4.8.2.0/Data-Maybe.html#t:Maybe) a) -&gt; a -&gt; [Parser](Data-Aeson-Types.html#t:Parser) a <a href="src/Data-Aeson-Types-Instances.html#.%21%3D" class="link">Source</a>

Helper for use in combination with `.:?` to provide default values for optional JSON object fields.

This combinator is most useful if the key and value can be absent from an object without affecting its validity and we know a default value to assign in that case. If the key and value are mandatory, use `.:` instead.

Example usage:

``` haskell
 v1 <- o .:? "opt_field_with_dfl" .!= "default_val"
 v2 <- o .:  "mandatory_field"
 v3 <- o .:? "opt_field2"
```

<a href="" class="def">object</a> :: \[[Pair](Data-Aeson-Types.html#t:Pair)\] -&gt; [Value](Data-Aeson.html#t:Value) <a href="src/Data-Aeson-Types-Internal.html#object" class="link">Source</a>

Create a `Value` from a list of name/value `Pair`s. If duplicate keys arise, earlier keys and their associated values win.

Parsing
=======

<a href="" class="def">json</a> :: [Parser](../attoparsec-0.13.0.2/Data-Attoparsec-ByteString.html#t:Parser) [Value](Data-Aeson.html#t:Value) <a href="src/Data-Aeson-Parser-Internal.html#json" class="link">Source</a>

Parse a top-level JSON value.

The conversion of a parsed value to a Haskell value is deferred until the Haskell value is needed. This may improve performance if only a subset of the results of conversions are needed, but at a cost in thunk allocation.

This function is an alias for `value`. In aeson 0.8 and earlier, it parsed only object or array types, in conformance with the now-obsolete RFC 4627.

<a href="" class="def">json'</a> :: [Parser](../attoparsec-0.13.0.2/Data-Attoparsec-ByteString.html#t:Parser) [Value](Data-Aeson.html#t:Value) <a href="src/Data-Aeson-Parser-Internal.html#json%27" class="link">Source</a>

Parse a top-level JSON value.

This is a strict version of `json` which avoids building up thunks during parsing; it performs all conversions immediately. Prefer this version if most of the JSON data needs to be accessed.

This function is an alias for `value'`. In aeson 0.8 and earlier, it parsed only object or array types, in conformance with the now-obsolete RFC 4627.

Produced by [Haddock](http://www.haskell.org/haddock/) version 2.16.1


