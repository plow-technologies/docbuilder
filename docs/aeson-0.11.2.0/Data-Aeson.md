<div id="package-header">

-   [Source](src/Data-Aeson.html)
-   [Contents](index.html)
-   [Index](doc-index.html)

aeson-0.11.2.0: Fast JSON parsing and encoding

</div>

<div id="content">

<div id="module-header">

  -------------- ---------------------------------------------------------
  Copyright      \(c) 2011-2016 Bryan O'Sullivan (c) 2011 MailRank, Inc.
  License        BSD3
  Maintainer     Bryan O'Sullivan &lt;bos@serpentine.com&gt;
  Stability      experimental
  Portability    portable
  Safe Haskell   None
  Language       Haskell2010
  -------------- ---------------------------------------------------------

Data.Aeson

</div>

<div id="table-of-contents">

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

</div>

<div id="description">

Description

<div class="doc">

Types and functions for working efficiently with JSON data.

(A note on naming: in Greek mythology, Aeson was the father of Jason.)

</div>

</div>

<div id="synopsis">

Synopsis

-   [decode](#v:decode) :: [FromJSON](Data-Aeson.html#t:FromJSON) a
    =&gt;
    [ByteString](../bytestring-0.10.6.0/Data-ByteString-Lazy.html#t:ByteString)
    -&gt; [Maybe](../base-4.8.2.0/Data-Maybe.html#t:Maybe) a
-   [decode'](#v:decode-39-) :: [FromJSON](Data-Aeson.html#t:FromJSON) a
    =&gt;
    [ByteString](../bytestring-0.10.6.0/Data-ByteString-Lazy.html#t:ByteString)
    -&gt; [Maybe](../base-4.8.2.0/Data-Maybe.html#t:Maybe) a
-   [eitherDecode](#v:eitherDecode) ::
    [FromJSON](Data-Aeson.html#t:FromJSON) a =&gt;
    [ByteString](../bytestring-0.10.6.0/Data-ByteString-Lazy.html#t:ByteString)
    -&gt; [Either](../base-4.8.2.0/Data-Either.html#t:Either)
    [String](../base-4.8.2.0/Data-String.html#t:String) a
-   [eitherDecode'](#v:eitherDecode-39-) ::
    [FromJSON](Data-Aeson.html#t:FromJSON) a =&gt;
    [ByteString](../bytestring-0.10.6.0/Data-ByteString-Lazy.html#t:ByteString)
    -&gt; [Either](../base-4.8.2.0/Data-Either.html#t:Either)
    [String](../base-4.8.2.0/Data-String.html#t:String) a
-   [encode](#v:encode) :: [ToJSON](Data-Aeson.html#t:ToJSON) a =&gt; a
    -&gt;
    [ByteString](../bytestring-0.10.6.0/Data-ByteString-Lazy.html#t:ByteString)
-   [decodeStrict](#v:decodeStrict) ::
    [FromJSON](Data-Aeson.html#t:FromJSON) a =&gt;
    [ByteString](../bytestring-0.10.6.0/Data-ByteString.html#t:ByteString)
    -&gt; [Maybe](../base-4.8.2.0/Data-Maybe.html#t:Maybe) a
-   [decodeStrict'](#v:decodeStrict-39-) ::
    [FromJSON](Data-Aeson.html#t:FromJSON) a =&gt;
    [ByteString](../bytestring-0.10.6.0/Data-ByteString.html#t:ByteString)
    -&gt; [Maybe](../base-4.8.2.0/Data-Maybe.html#t:Maybe) a
-   [eitherDecodeStrict](#v:eitherDecodeStrict) ::
    [FromJSON](Data-Aeson.html#t:FromJSON) a =&gt;
    [ByteString](../bytestring-0.10.6.0/Data-ByteString.html#t:ByteString)
    -&gt; [Either](../base-4.8.2.0/Data-Either.html#t:Either)
    [String](../base-4.8.2.0/Data-String.html#t:String) a
-   [eitherDecodeStrict'](#v:eitherDecodeStrict-39-) ::
    [FromJSON](Data-Aeson.html#t:FromJSON) a =&gt;
    [ByteString](../bytestring-0.10.6.0/Data-ByteString.html#t:ByteString)
    -&gt; [Either](../base-4.8.2.0/Data-Either.html#t:Either)
    [String](../base-4.8.2.0/Data-String.html#t:String) a
-   <span class="keyword">data</span> [Value](#t:Value)
    -   = [Object](#v:Object) ![Object](Data-Aeson.html#t:Object)
    -   | [Array](#v:Array) ![Array](Data-Aeson.html#t:Array)
    -   | [String](#v:String)
        ![Text](../text-1.2.2.1/Data-Text.html#t:Text)
    -   | [Number](#v:Number)
        ![Scientific](../scientific-0.3.4.9/Data-Scientific.html#t:Scientific)
    -   | [Bool](#v:Bool) ![Bool](../base-4.8.2.0/Data-Bool.html#t:Bool)
    -   | [Null](#v:Null)
-   <span class="keyword">data</span> [Encoding](#t:Encoding)
-   [fromEncoding](#v:fromEncoding) ::
    [Encoding](Data-Aeson.html#t:Encoding) -&gt;
    [Builder](../bytestring-0.10.6.0/Data-ByteString-Builder.html#t:Builder)
-   <span class="keyword">type</span> [Array](#t:Array) =
    [Vector](../vector-0.11.0.0/Data-Vector.html#t:Vector)
    [Value](Data-Aeson.html#t:Value)
-   <span class="keyword">type</span> [Object](#t:Object) =
    [HashMap](../unordered-containers-0.2.7.1/Data-HashMap-Strict.html#t:HashMap)
    [Text](../text-1.2.2.1/Data-Text.html#t:Text)
    [Value](Data-Aeson.html#t:Value)
-   <span class="keyword">newtype</span> [DotNetTime](#t:DotNetTime) =
    [DotNetTime](#v:DotNetTime) {
    -   [fromDotNetTime](#v:fromDotNetTime) ::
        [UTCTime](../time-1.5.0.1/Data-Time-Clock.html#t:UTCTime)

    }
-   <span class="keyword">class</span> [FromJSON](#t:FromJSON) a <span
    class="keyword">where</span>
    -   [parseJSON](#v:parseJSON) :: [Value](Data-Aeson.html#t:Value)
        -&gt; [Parser](Data-Aeson-Types.html#t:Parser) a
-   <span class="keyword">data</span> [Result](#t:Result) a
    -   = [Error](#v:Error)
        [String](../base-4.8.2.0/Data-String.html#t:String)
    -   | [Success](#v:Success) a
-   [fromJSON](#v:fromJSON) :: [FromJSON](Data-Aeson.html#t:FromJSON) a
    =&gt; [Value](Data-Aeson.html#t:Value) -&gt;
    [Result](Data-Aeson.html#t:Result) a
-   <span class="keyword">class</span> [ToJSON](#t:ToJSON) a <span
    class="keyword">where</span>
    -   [toJSON](#v:toJSON) :: a -&gt; [Value](Data-Aeson.html#t:Value)
    -   [toEncoding](#v:toEncoding) :: a -&gt;
        [Encoding](Data-Aeson.html#t:Encoding)
-   <span class="keyword">class</span> [KeyValue](#t:KeyValue) kv <span
    class="keyword">where</span>
    -   [(.=)](#v:.-61-) :: [ToJSON](Data-Aeson.html#t:ToJSON) v =&gt;
        [Text](../text-1.2.2.1/Data-Text.html#t:Text) -&gt; v -&gt; kv
-   <span class="keyword">class</span> [GFromJSON](#t:GFromJSON) f <span
    class="keyword">where</span>
    -   [gParseJSON](#v:gParseJSON) ::
        [Options](Data-Aeson-Types.html#t:Options) -&gt;
        [Value](Data-Aeson.html#t:Value) -&gt;
        [Parser](Data-Aeson-Types.html#t:Parser) (f a)
-   <span class="keyword">class</span> [GToJSON](#t:GToJSON) f <span
    class="keyword">where</span>
    -   [gToJSON](#v:gToJSON) ::
        [Options](Data-Aeson-Types.html#t:Options) -&gt; f a -&gt;
        [Value](Data-Aeson.html#t:Value)
-   <span class="keyword">class</span> [GToEncoding](#t:GToEncoding) f
    <span class="keyword">where</span>
    -   [gToEncoding](#v:gToEncoding) ::
        [Options](Data-Aeson-Types.html#t:Options) -&gt; f a -&gt;
        [Encoding](Data-Aeson.html#t:Encoding)
-   [genericToJSON](#v:genericToJSON) ::
    ([Generic](../base-4.8.2.0/GHC-Generics.html#t:Generic) a,
    [GToJSON](Data-Aeson.html#t:GToJSON)
    ([Rep](../base-4.8.2.0/GHC-Generics.html#t:Rep) a)) =&gt;
    [Options](Data-Aeson-Types.html#t:Options) -&gt; a -&gt;
    [Value](Data-Aeson.html#t:Value)
-   [genericToEncoding](#v:genericToEncoding) ::
    ([Generic](../base-4.8.2.0/GHC-Generics.html#t:Generic) a,
    [GToEncoding](Data-Aeson.html#t:GToEncoding)
    ([Rep](../base-4.8.2.0/GHC-Generics.html#t:Rep) a)) =&gt;
    [Options](Data-Aeson-Types.html#t:Options) -&gt; a -&gt;
    [Encoding](Data-Aeson.html#t:Encoding)
-   [genericParseJSON](#v:genericParseJSON) ::
    ([Generic](../base-4.8.2.0/GHC-Generics.html#t:Generic) a,
    [GFromJSON](Data-Aeson.html#t:GFromJSON)
    ([Rep](../base-4.8.2.0/GHC-Generics.html#t:Rep) a)) =&gt;
    [Options](Data-Aeson-Types.html#t:Options) -&gt;
    [Value](Data-Aeson.html#t:Value) -&gt;
    [Parser](Data-Aeson-Types.html#t:Parser) a
-   [defaultOptions](#v:defaultOptions) ::
    [Options](Data-Aeson-Types.html#t:Options)
-   [withObject](#v:withObject) ::
    [String](../base-4.8.2.0/Data-String.html#t:String) -&gt;
    ([Object](Data-Aeson.html#t:Object) -&gt;
    [Parser](Data-Aeson-Types.html#t:Parser) a) -&gt;
    [Value](Data-Aeson.html#t:Value) -&gt;
    [Parser](Data-Aeson-Types.html#t:Parser) a
-   [withText](#v:withText) ::
    [String](../base-4.8.2.0/Data-String.html#t:String) -&gt;
    ([Text](../text-1.2.2.1/Data-Text.html#t:Text) -&gt;
    [Parser](Data-Aeson-Types.html#t:Parser) a) -&gt;
    [Value](Data-Aeson.html#t:Value) -&gt;
    [Parser](Data-Aeson-Types.html#t:Parser) a
-   [withArray](#v:withArray) ::
    [String](../base-4.8.2.0/Data-String.html#t:String) -&gt;
    ([Array](Data-Aeson.html#t:Array) -&gt;
    [Parser](Data-Aeson-Types.html#t:Parser) a) -&gt;
    [Value](Data-Aeson.html#t:Value) -&gt;
    [Parser](Data-Aeson-Types.html#t:Parser) a
-   [withNumber](#v:withNumber) ::
    [String](../base-4.8.2.0/Data-String.html#t:String) -&gt;
    ([Number](../attoparsec-0.13.0.2/Data-Attoparsec-Number.html#t:Number)
    -&gt; [Parser](Data-Aeson-Types.html#t:Parser) a) -&gt;
    [Value](Data-Aeson.html#t:Value) -&gt;
    [Parser](Data-Aeson-Types.html#t:Parser) a
-   [withScientific](#v:withScientific) ::
    [String](../base-4.8.2.0/Data-String.html#t:String) -&gt;
    ([Scientific](../scientific-0.3.4.9/Data-Scientific.html#t:Scientific)
    -&gt; [Parser](Data-Aeson-Types.html#t:Parser) a) -&gt;
    [Value](Data-Aeson.html#t:Value) -&gt;
    [Parser](Data-Aeson-Types.html#t:Parser) a
-   [withBool](#v:withBool) ::
    [String](../base-4.8.2.0/Data-String.html#t:String) -&gt;
    ([Bool](../base-4.8.2.0/Data-Bool.html#t:Bool) -&gt;
    [Parser](Data-Aeson-Types.html#t:Parser) a) -&gt;
    [Value](Data-Aeson.html#t:Value) -&gt;
    [Parser](Data-Aeson-Types.html#t:Parser) a
-   <span class="keyword">data</span> [Series](#t:Series)
-   [pairs](#v:pairs) :: [Series](Data-Aeson.html#t:Series) -&gt;
    [Encoding](Data-Aeson.html#t:Encoding)
-   [foldable](#v:foldable) ::
    ([Foldable](../base-4.8.2.0/Data-Foldable.html#t:Foldable) t,
    [ToJSON](Data-Aeson.html#t:ToJSON) a) =&gt; t a -&gt;
    [Encoding](Data-Aeson.html#t:Encoding)
-   [(.:)](#v:.:) :: [FromJSON](Data-Aeson.html#t:FromJSON) a =&gt;
    [Object](Data-Aeson.html#t:Object) -&gt;
    [Text](../text-1.2.2.1/Data-Text.html#t:Text) -&gt;
    [Parser](Data-Aeson-Types.html#t:Parser) a
-   [(.:?)](#v:.:-63-) :: [FromJSON](Data-Aeson.html#t:FromJSON) a =&gt;
    [Object](Data-Aeson.html#t:Object) -&gt;
    [Text](../text-1.2.2.1/Data-Text.html#t:Text) -&gt;
    [Parser](Data-Aeson-Types.html#t:Parser)
    ([Maybe](../base-4.8.2.0/Data-Maybe.html#t:Maybe) a)
-   [(.:!)](#v:.:-33-) :: [FromJSON](Data-Aeson.html#t:FromJSON) a =&gt;
    [Object](Data-Aeson.html#t:Object) -&gt;
    [Text](../text-1.2.2.1/Data-Text.html#t:Text) -&gt;
    [Parser](Data-Aeson-Types.html#t:Parser)
    ([Maybe](../base-4.8.2.0/Data-Maybe.html#t:Maybe) a)
-   [(.!=)](#v:.-33--61-) :: [Parser](Data-Aeson-Types.html#t:Parser)
    ([Maybe](../base-4.8.2.0/Data-Maybe.html#t:Maybe) a) -&gt; a -&gt;
    [Parser](Data-Aeson-Types.html#t:Parser) a
-   [object](#v:object) :: \[[Pair](Data-Aeson-Types.html#t:Pair)\]
    -&gt; [Value](Data-Aeson.html#t:Value)
-   [json](#v:json) ::
    [Parser](../attoparsec-0.13.0.2/Data-Attoparsec-ByteString.html#t:Parser)
    [Value](Data-Aeson.html#t:Value)
-   [json'](#v:json-39-) ::
    [Parser](../attoparsec-0.13.0.2/Data-Attoparsec-ByteString.html#t:Parser)
    [Value](Data-Aeson.html#t:Value)

</div>

<div id="interface">

How to use this library {#g:1}
=======================

<div class="doc">

This section contains basic information on the different ways to work
with data using this library. These range from simple but inflexible, to
complex but flexible.

The most common way to use the library is to define a data type,
corresponding to some JSON data you want to work with, and then write
either a `FromJSON` instance, a to `ToJSON` instance, or both for that
type.

For example, given this JSON data:

    { "name": "Joe", "age": 12 }

we create a matching data type:

    {-# LANGUAGE DeriveGeneric #-}

    import GHC.Generics

    data Person = Person {
          name :: Text
        , age  :: Int
        } deriving (Generic, Show)

The `LANGUAGE` pragma and `Generic` instance let us write empty
`FromJSON` and `ToJSON` instances for which the compiler will generate
sensible default implementations.

    instance ToJSON Person where
        -- No need to provide a toJSON implementation.

        -- For efficiency, we write a simple toEncoding implementation, as
        -- the default version uses toJSON.
        toEncoding = genericToEncoding defaultOptions

    instance FromJSON Person
        -- No need to provide a parseJSON implementation.

We can now encode a value like so:

    >>> encode (Person {name = "Joe", age = 12})
    "{\"name\":\"Joe\",\"age\":12}"

</div>

Writing instances by hand {#g:2}
-------------------------

<div class="doc">

When necessary, we can write `ToJSON` and `FromJSON` instances by hand.
This is valuable when the JSON-on-the-wire and Haskell data are
different or otherwise need some more carefully managed translation.
Let's revisit our JSON data:

    { "name": "Joe", "age": 12 }

We once again create a matching data type, without bothering to add a
`Generic` instance this time:

    data Person = Person {
          name :: Text
        , age  :: Int
        } deriving Show

To decode data, we need to define a `FromJSON` instance:

    {-# LANGUAGE OverloadedStrings #-}

    instance FromJSON Person where
        parseJSON (Object v) = Person <$>
                               v .: "name" <*>
                               v .: "age"
        -- A non-Object value is of the wrong type, so fail.
        parseJSON _          = empty

We can now parse the JSON data like so:

    >>> decode "{\"name\":\"Joe\",\"age\":12}" :: Maybe Person
    Just (Person {name = "Joe", age = 12})

To encode data, we need to define a `ToJSON` instance. Let's begin with
an instance written entirely by hand.

    instance ToJSON Person where
        -- this generates a Value
        toJSON (Person name age) =
            object ["name" .= name, "age" .= age]

        -- this encodes directly to a bytestring Builder
        toEncoding (Person name age) =
            pairs ("name" .= name <> "age" .= age)

We can now encode a value like so:

    >>> encode (Person {name = "Joe", age = 12})
    "{\"name\":\"Joe\",\"age\":12}"

There are predefined `FromJSON` and `ToJSON` instances for many types.
Here's an example using lists and `Int`s:

    >>> decode "[1,2,3]" :: Maybe [Int]
    Just [1,2,3]

And here's an example using the `Map` type to get a map of `Int`s.

    >>> decode "{\"foo\":1,\"bar\":2}" :: Maybe (Map String Int)
    Just (fromList [("bar",2),("foo",1)])

</div>

Working with the AST {#g:3}
--------------------

<div class="doc">

Sometimes you want to work with JSON data directly, without first
converting it to a custom data type. This can be useful if you want to
e.g. convert JSON data to YAML data, without knowing what the contents
of the original JSON data was. The `Value` type, which is an instance of
`FromJSON`, is used to represent an arbitrary JSON AST (abstract syntax
tree). Example usage:

    >>> decode "{\"foo\": 123}" :: Maybe Value
    Just (Object (fromList [("foo",Number 123)]))

    >>> decode "{\"foo\": [\"abc\",\"def\"]}" :: Maybe Value
    Just (Object (fromList [("foo",Array (fromList [String "abc",String "def"]))]))

Once you have a `Value` you can write functions to traverse it and make
arbitrary transformations.

</div>

Decoding to a Haskell value {#g:4}
---------------------------

<div class="doc">

We can decode to any instance of `FromJSON`:

    λ> decode "[1,2,3]" :: Maybe [Int]
    Just [1,2,3]

Alternatively, there are instances for standard data types, so you can
use them directly. For example, use the `Map` type to get a map of
`Int`s.

    λ> import Data.Map
    λ> decode "{\"foo\":1,\"bar\":2}" :: Maybe (Map String Int)
    Just (fromList [("bar",2),("foo",1)])

</div>

Decoding a mixed-type object {#g:5}
----------------------------

<div class="doc">

The above approach with maps of course will not work for mixed-type
objects that don't follow a strict schema, but there are a couple of
approaches available for these.

The `Object` type contains JSON objects:

    λ> decode "{\"name\":\"Dave\",\"age\":2}" :: Maybe Object
    Just (fromList) [("name",String "Dave"),("age",Number 2)]

You can extract values from it with a parser using `parse`,
`parseEither` or, in this example, `parseMaybe`:

    λ> do result <- decode "{\"name\":\"Dave\",\"age\":2}"
          flip parseMaybe result $ \obj -> do
            age <- obj .: "age"
            name <- obj .: "name"
            return (name ++ ": " ++ show (age*2))

    Just "Dave: 4"

Considering that any type that implements `FromJSON` can be used here,
this is quite a powerful way to parse JSON. See the documentation in
`FromJSON` for how to implement this class for your own data types.

The downside is that you have to write the parser yourself; the upside
is that you have complete control over the way the JSON is parsed.

</div>

Encoding and decoding {#g:6}
=====================

<div class="doc">

Decoding is a two-step process.

-   When decoding a value, the process is reversed: the bytes are
    converted to a `Value`, then the `FromJSON` class is used to convert
    to the desired type.

There are two ways to encode a value.

-   Convert to a `Value` using `toJSON`, then possibly further encode.
    This was the only method available in aeson 0.9 and earlier.
-   Directly encode (to what will become a `ByteString`) using
    `toEncoding`. This is much more efficient (about 3x faster, and less
    memory intensive besides), but is only available in aeson 0.10
    and newer.

For convenience, the `encode` and `decode` functions combine both steps.

</div>

Direct encoding {#g:7}
---------------

<div class="doc">

In older versions of this library, encoding a Haskell value involved
converting to an intermediate `Value`, then encoding that.

A "direct" encoder converts straight from a source Haskell value to a
`ByteString` without constructing an intermediate `Value`. This approach
is faster than `toJSON`, and allocates less memory. The `toEncoding`
method makes it possible to implement direct encoding with low memory
overhead.

To complicate matters, the default implementation of `toEncoding` uses
`toJSON`. Why? The `toEncoding` method was added to this library much
more recently than `toJSON`. Using `toJSON` ensures that packages
written against older versions of this library will compile and produce
correct output, but they will not see any speedup from direct encoding.

To write a minimal implementation of direct encoding, your type must
implement GHC's `Generic` class, and your code should look like this:

        toEncoding = genericToEncoding defaultOptions

What if you have more elaborate encoding needs? For example, perhaps you
need to change the names of object keys, omit parts of a value.

To encode to a JSON "object", use the `pairs` function.

        toEncoding (Person name age) =
            pairs ("name" .= name <> "age" .= age)

Any container type that implements `Foldable` can be encoded to a JSON
"array" using `foldable`.

    > import Data.Sequence as Seq
    > encode (Seq.fromList [1,2,3])
    "[1,2,3]"

</div>

<div class="top">

[decode](){.def} :: [FromJSON](Data-Aeson.html#t:FromJSON) a =&gt;
[ByteString](../bytestring-0.10.6.0/Data-ByteString-Lazy.html#t:ByteString)
-&gt; [Maybe](../base-4.8.2.0/Data-Maybe.html#t:Maybe) a
[Source](src/Data-Aeson.html#decode){.link}

<div class="doc">

Efficiently deserialize a JSON value from a lazy `ByteString`. If this
fails due to incomplete or invalid input, `Nothing` is returned.

The input must consist solely of a JSON document, with no trailing data
except for whitespace.

This function parses immediately, but defers conversion. See `json` for
details.

</div>

</div>

<div class="top">

[decode'](){.def} :: [FromJSON](Data-Aeson.html#t:FromJSON) a =&gt;
[ByteString](../bytestring-0.10.6.0/Data-ByteString-Lazy.html#t:ByteString)
-&gt; [Maybe](../base-4.8.2.0/Data-Maybe.html#t:Maybe) a
[Source](src/Data-Aeson.html#decode%27){.link}

<div class="doc">

Efficiently deserialize a JSON value from a lazy `ByteString`. If this
fails due to incomplete or invalid input, `Nothing` is returned.

The input must consist solely of a JSON document, with no trailing data
except for whitespace.

This function parses and performs conversion immediately. See `json'`
for details.

</div>

</div>

<div class="top">

[eitherDecode](){.def} :: [FromJSON](Data-Aeson.html#t:FromJSON) a =&gt;
[ByteString](../bytestring-0.10.6.0/Data-ByteString-Lazy.html#t:ByteString)
-&gt; [Either](../base-4.8.2.0/Data-Either.html#t:Either)
[String](../base-4.8.2.0/Data-String.html#t:String) a
[Source](src/Data-Aeson.html#eitherDecode){.link}

<div class="doc">

Like `decode` but returns an error message when decoding fails.

</div>

</div>

<div class="top">

[eitherDecode'](){.def} :: [FromJSON](Data-Aeson.html#t:FromJSON) a
=&gt;
[ByteString](../bytestring-0.10.6.0/Data-ByteString-Lazy.html#t:ByteString)
-&gt; [Either](../base-4.8.2.0/Data-Either.html#t:Either)
[String](../base-4.8.2.0/Data-String.html#t:String) a
[Source](src/Data-Aeson.html#eitherDecode%27){.link}

<div class="doc">

Like `decode'` but returns an error message when decoding fails.

</div>

</div>

<div class="top">

[encode](){.def} :: [ToJSON](Data-Aeson.html#t:ToJSON) a =&gt; a -&gt;
[ByteString](../bytestring-0.10.6.0/Data-ByteString-Lazy.html#t:ByteString)
[Source](src/Data-Aeson-Encode-Functions.html#encode){.link}

<div class="doc">

Efficiently serialize a JSON value as a lazy `ByteString`.

This is implemented in terms of the `ToJSON` class's `toEncoding`
method.

</div>

</div>

Variants for strict bytestrings {#g:8}
-------------------------------

<div class="top">

[decodeStrict](){.def} :: [FromJSON](Data-Aeson.html#t:FromJSON) a =&gt;
[ByteString](../bytestring-0.10.6.0/Data-ByteString.html#t:ByteString)
-&gt; [Maybe](../base-4.8.2.0/Data-Maybe.html#t:Maybe) a
[Source](src/Data-Aeson.html#decodeStrict){.link}

<div class="doc">

Efficiently deserialize a JSON value from a strict `ByteString`. If this
fails due to incomplete or invalid input, `Nothing` is returned.

The input must consist solely of a JSON document, with no trailing data
except for whitespace.

This function parses immediately, but defers conversion. See `json` for
details.

</div>

</div>

<div class="top">

[decodeStrict'](){.def} :: [FromJSON](Data-Aeson.html#t:FromJSON) a
=&gt;
[ByteString](../bytestring-0.10.6.0/Data-ByteString.html#t:ByteString)
-&gt; [Maybe](../base-4.8.2.0/Data-Maybe.html#t:Maybe) a
[Source](src/Data-Aeson.html#decodeStrict%27){.link}

<div class="doc">

Efficiently deserialize a JSON value from a lazy `ByteString`. If this
fails due to incomplete or invalid input, `Nothing` is returned.

The input must consist solely of a JSON document, with no trailing data
except for whitespace.

This function parses and performs conversion immediately. See `json'`
for details.

</div>

</div>

<div class="top">

[eitherDecodeStrict](){.def} :: [FromJSON](Data-Aeson.html#t:FromJSON) a
=&gt;
[ByteString](../bytestring-0.10.6.0/Data-ByteString.html#t:ByteString)
-&gt; [Either](../base-4.8.2.0/Data-Either.html#t:Either)
[String](../base-4.8.2.0/Data-String.html#t:String) a
[Source](src/Data-Aeson.html#eitherDecodeStrict){.link}

<div class="doc">

Like `decodeStrict` but returns an error message when decoding fails.

</div>

</div>

<div class="top">

[eitherDecodeStrict'](){.def} :: [FromJSON](Data-Aeson.html#t:FromJSON)
a =&gt;
[ByteString](../bytestring-0.10.6.0/Data-ByteString.html#t:ByteString)
-&gt; [Either](../base-4.8.2.0/Data-Either.html#t:Either)
[String](../base-4.8.2.0/Data-String.html#t:String) a
[Source](src/Data-Aeson.html#eitherDecodeStrict%27){.link}

<div class="doc">

Like `decodeStrict'` but returns an error message when decoding fails.

</div>

</div>

Core JSON types {#g:9}
===============

<div class="top">

<span class="keyword">data</span> [Value](){.def}
[Source](src/Data-Aeson-Types-Internal.html#Value){.link}

<div class="doc">

A JSON value represented as a Haskell value.

</div>

<div class="subs constructors">

Constructors

  ----------------------------------------------------------------------------------------- ---
  [Object](){.def} ![Object](Data-Aeson.html#t:Object)                                       
  [Array](){.def} ![Array](Data-Aeson.html#t:Array)                                          
  [String](){.def} ![Text](../text-1.2.2.1/Data-Text.html#t:Text)                            
  [Number](){.def} ![Scientific](../scientific-0.3.4.9/Data-Scientific.html#t:Scientific)    
  [Bool](){.def} ![Bool](../base-4.8.2.0/Data-Bool.html#t:Bool)                              
  [Null](){.def}                                                                             
  ----------------------------------------------------------------------------------------- ---

</div>

<div class="subs instances">

Instances

<div id="section.i:Value" class="show">

  ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- ---
  <span class="inst-left">[Eq](../base-4.8.2.0/Data-Eq.html#t:Eq) [Value](Data-Aeson.html#t:Value)</span> [Source](src/Data-Aeson-Types-Internal.html#line-339){.link}                                        
  <span class="inst-left">[Data](../base-4.8.2.0/Data-Data.html#t:Data) [Value](Data-Aeson.html#t:Value)</span> [Source](src/Data-Aeson-Types-Internal.html#line-339){.link}                                  
  <span class="inst-left">[Read](../base-4.8.2.0/Text-Read.html#t:Read) [Value](Data-Aeson.html#t:Value)</span> [Source](src/Data-Aeson-Types-Internal.html#line-339){.link}                                  
  <span class="inst-left">[Show](../base-4.8.2.0/Text-Show.html#t:Show) [Value](Data-Aeson.html#t:Value)</span> [Source](src/Data-Aeson-Types-Internal.html#line-339){.link}                                  
  <span class="inst-left">[IsString](../base-4.8.2.0/Data-String.html#t:IsString) [Value](Data-Aeson.html#t:Value)</span> [Source](src/Data-Aeson-Types-Internal.html#line-408){.link}                        
  <span class="inst-left">[NFData](../deepseq-1.4.1.1/Control-DeepSeq.html#t:NFData) [Value](Data-Aeson.html#t:Value)</span> [Source](src/Data-Aeson-Types-Internal.html#line-400){.link}                     
  <span class="inst-left">[Hashable](../hashable-1.2.4.0/Data-Hashable.html#t:Hashable) [Value](Data-Aeson.html#t:Value)</span> [Source](src/Data-Aeson-Types-Internal.html#line-428){.link}                  
  <span class="inst-left">[Lift](../template-haskell-2.10.0.0/Language-Haskell-TH-Syntax.html#t:Lift) [Value](Data-Aeson.html#t:Value)</span> [Source](src/Data-Aeson-Types-Internal.html#line-432){.link}    
  ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- ---

</div>

</div>

</div>

<div class="top">

<span class="keyword">data</span> [Encoding](){.def}
[Source](src/Data-Aeson-Types-Internal.html#Encoding){.link}

<div class="doc">

An encoding of a JSON value.

</div>

<div class="subs instances">

Instances

<div id="section.i:Encoding" class="show">

  ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ ---
  <span class="inst-left">[Eq](../base-4.8.2.0/Data-Eq.html#t:Eq) [Encoding](Data-Aeson.html#t:Encoding)</span> [Source](src/Data-Aeson-Types-Internal.html#line-357){.link}                              
  <span class="inst-left">[Ord](../base-4.8.2.0/Data-Ord.html#t:Ord) [Encoding](Data-Aeson.html#t:Encoding)</span> [Source](src/Data-Aeson-Types-Internal.html#line-360){.link}                           
  <span class="inst-left">[Show](../base-4.8.2.0/Text-Show.html#t:Show) [Encoding](Data-Aeson.html#t:Encoding)</span> [Source](src/Data-Aeson-Types-Internal.html#line-354){.link}                        
  <span class="inst-left">[Monoid](../base-4.8.2.0/Data-Monoid.html#t:Monoid) [Encoding](Data-Aeson.html#t:Encoding)</span> [Source](src/Data-Aeson-Types-Internal.html#line-345){.link}                  
  <span class="inst-left">[Semigroup](../semigroups-0.18.1/Data-Semigroup.html#t:Semigroup) [Encoding](Data-Aeson.html#t:Encoding)</span> [Source](src/Data-Aeson-Types-Internal.html#line-345){.link}    
  ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ ---

</div>

</div>

</div>

<div class="top">

[fromEncoding](){.def} :: [Encoding](Data-Aeson.html#t:Encoding) -&gt;
[Builder](../bytestring-0.10.6.0/Data-ByteString-Builder.html#t:Builder)
[Source](src/Data-Aeson-Types-Internal.html#fromEncoding){.link}

<div class="doc">

Acquire the underlying bytestring builder.

</div>

</div>

<div class="top">

<span class="keyword">type</span> [Array](){.def} =
[Vector](../vector-0.11.0.0/Data-Vector.html#t:Vector)
[Value](Data-Aeson.html#t:Value)
[Source](src/Data-Aeson-Types-Internal.html#Array){.link}

<div class="doc">

A JSON "array" (sequence).

</div>

</div>

<div class="top">

<span class="keyword">type</span> [Object](){.def} =
[HashMap](../unordered-containers-0.2.7.1/Data-HashMap-Strict.html#t:HashMap)
[Text](../text-1.2.2.1/Data-Text.html#t:Text)
[Value](Data-Aeson.html#t:Value)
[Source](src/Data-Aeson-Types-Internal.html#Object){.link}

<div class="doc">

A JSON "object" (key/value map).

</div>

</div>

Convenience types {#g:10}
=================

<div class="top">

<span class="keyword">newtype</span> [DotNetTime](){.def}
[Source](src/Data-Aeson-Types-Internal.html#DotNetTime){.link}

<div class="doc">

A newtype wrapper for `UTCTime` that uses the same non-standard
serialization format as Microsoft .NET, whose
[System.DateTime](https://msdn.microsoft.com/en-us/library/system.datetime(v=vs.110).aspx)
type is by default serialized to JSON as in the following example:

    /Date(1302547608878)/

The number represents milliseconds since the Unix epoch.

</div>

<div class="subs constructors">

Constructors

[DotNetTime](){.def}
 
<div class="subs fields">

Fields

[fromDotNetTime](){.def} :: [UTCTime](../time-1.5.0.1/Data-Time-Clock.html#t:UTCTime)

:   Acquire the underlying value.

<div class="clear">

</div>

</div>

</div>

<div class="subs instances">

Instances

<div id="section.i:DotNetTime" class="show">

  --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- ---
  <span class="inst-left">[Eq](../base-4.8.2.0/Data-Eq.html#t:Eq) [DotNetTime](Data-Aeson.html#t:DotNetTime)</span> [Source](src/Data-Aeson-Types-Internal.html#line-398){.link}                             
  <span class="inst-left">[Ord](../base-4.8.2.0/Data-Ord.html#t:Ord) [DotNetTime](Data-Aeson.html#t:DotNetTime)</span> [Source](src/Data-Aeson-Types-Internal.html#line-398){.link}                          
  <span class="inst-left">[Read](../base-4.8.2.0/Text-Read.html#t:Read) [DotNetTime](Data-Aeson.html#t:DotNetTime)</span> [Source](src/Data-Aeson-Types-Internal.html#line-398){.link}                       
  <span class="inst-left">[Show](../base-4.8.2.0/Text-Show.html#t:Show) [DotNetTime](Data-Aeson.html#t:DotNetTime)</span> [Source](src/Data-Aeson-Types-Internal.html#line-398){.link}                       
  <span class="inst-left">[FormatTime](../time-1.5.0.1/Data-Time-Format.html#t:FormatTime) [DotNetTime](Data-Aeson.html#t:DotNetTime)</span> [Source](src/Data-Aeson-Types-Internal.html#line-398){.link}    
  --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- ---

</div>

</div>

</div>

Type conversion {#g:11}
===============

<div class="top">

<span class="keyword">class</span> [FromJSON](){.def} a <span
class="keyword">where</span>
[Source](src/Data-Aeson-Types-Class.html#FromJSON){.link}

<div class="doc">

A type that can be converted from JSON, with the possibility of failure.

In many cases, you can get the compiler to generate parsing code for you
(see below). To begin, let's cover writing an instance by hand.

There are various reasons a conversion could fail. For example, an
`Object` could be missing a required key, an `Array` could be of the
wrong size, or a value could be of an incompatible type.

The basic ways to signal a failed conversion are as follows:

-   `empty` and `mzero` work, but are terse and uninformative
-   `fail` yields a custom error message
-   `typeMismatch` produces an informative message for cases when the
    value encountered is not of the expected type

An example type and instance:

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

Instead of manually writing your `FromJSON` instance, there are two
options to do it automatically:

-   [Data.Aeson.TH](Data-Aeson-TH.html) provides Template Haskell
    functions which will derive an instance at compile time. The
    generated instance is optimized for your type so will probably be
    more efficient than the following two options:
-   The compiler can provide a default generic implementation for
    `parseJSON`.

To use the second, simply add a `deriving Generic` clause to your
datatype and declare a `FromJSON` instance for your datatype without
giving a definition for `parseJSON`.

For example, the previous example can be simplified to just:

    {-# LANGUAGE DeriveGeneric #-}

    import GHC.Generics

    data Coord = Coord { x :: Double, y :: Double } deriving Generic

    instance FromJSON Coord

If `DefaultSignatures` doesn't give exactly the results you want, you
can customize the generic decoding with only a tiny amount of effort,
using `genericParseJSON` with your preferred `Options`:

    instance FromJSON Coord where
        parseJSON = genericParseJSON defaultOptions

</div>

<div class="subs minimal">

Minimal complete definition

Nothing

</div>

<div class="subs methods">

Methods

[parseJSON](){.def} :: [Value](Data-Aeson.html#t:Value) -&gt;
[Parser](Data-Aeson-Types.html#t:Parser) a
[Source](src/Data-Aeson-Types-Class.html#parseJSON){.link}

</div>

</div>

<div class="top">

<span class="keyword">data</span> [Result](){.def} a
[Source](src/Data-Aeson-Types-Internal.html#Result){.link}

<div class="doc">

The result of running a `Parser`.

</div>

<div class="subs constructors">

Constructors

  --------------------------------------------------------------------- ---
  [Error](){.def} [String](../base-4.8.2.0/Data-String.html#t:String)    
  [Success](){.def} a                                                    
  --------------------------------------------------------------------- ---

</div>

<div class="subs instances">

Instances

<div id="section.i:Result" class="show">

  ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ ---
  <span class="inst-left">[Monad](../base-4.8.2.0/Control-Monad.html#t:Monad) [Result](Data-Aeson.html#t:Result)</span> [Source](src/Data-Aeson-Types-Internal.html#line-157){.link}                                                                                  
  <span class="inst-left">[Functor](../base-4.8.2.0/Data-Functor.html#t:Functor) [Result](Data-Aeson.html#t:Result)</span> [Source](src/Data-Aeson-Types-Internal.html#line-137){.link}                                                                               
  <span class="inst-left">[Applicative](../base-4.8.2.0/Control-Applicative.html#t:Applicative) [Result](Data-Aeson.html#t:Result)</span> [Source](src/Data-Aeson-Types-Internal.html#line-178){.link}                                                                
  <span class="inst-left">[Foldable](../base-4.8.2.0/Data-Foldable.html#t:Foldable) [Result](Data-Aeson.html#t:Result)</span> [Source](src/Data-Aeson-Types-Internal.html#line-239){.link}                                                                            
  <span class="inst-left">[Traversable](../base-4.8.2.0/Data-Traversable.html#t:Traversable) [Result](Data-Aeson.html#t:Result)</span> [Source](src/Data-Aeson-Types-Internal.html#line-253){.link}                                                                   
  <span class="inst-left">[Alternative](../base-4.8.2.0/Control-Applicative.html#t:Alternative) [Result](Data-Aeson.html#t:Result)</span> [Source](src/Data-Aeson-Types-Internal.html#line-204){.link}                                                                
  <span class="inst-left">[MonadPlus](../base-4.8.2.0/Control-Monad.html#t:MonadPlus) [Result](Data-Aeson.html#t:Result)</span> [Source](src/Data-Aeson-Types-Internal.html#line-191){.link}                                                                          
  <span class="inst-left">[MonadFail](../fail-4.9.0.0/Control-Monad-Fail.html#t:MonadFail) [Result](Data-Aeson.html#t:Result)</span> [Source](src/Data-Aeson-Types-Internal.html#line-168){.link}                                                                     
  <span class="inst-left">[Eq](../base-4.8.2.0/Data-Eq.html#t:Eq) a =&gt; [Eq](../base-4.8.2.0/Data-Eq.html#t:Eq) ([Result](Data-Aeson.html#t:Result) a)</span> [Source](src/Data-Aeson-Types-Internal.html#line-118){.link}                                          
  <span class="inst-left">[Show](../base-4.8.2.0/Text-Show.html#t:Show) a =&gt; [Show](../base-4.8.2.0/Text-Show.html#t:Show) ([Result](Data-Aeson.html#t:Result) a)</span> [Source](src/Data-Aeson-Types-Internal.html#line-118){.link}                              
  <span class="inst-left">[Monoid](../base-4.8.2.0/Data-Monoid.html#t:Monoid) ([Result](Data-Aeson.html#t:Result) a)</span> [Source](src/Data-Aeson-Types-Internal.html#line-224){.link}                                                                              
  <span class="inst-left">[NFData](../deepseq-1.4.1.1/Control-DeepSeq.html#t:NFData) a =&gt; [NFData](../deepseq-1.4.1.1/Control-DeepSeq.html#t:NFData) ([Result](Data-Aeson.html#t:Result) a)</span> [Source](src/Data-Aeson-Types-Internal.html#line-128){.link}    
  <span class="inst-left">[Semigroup](../semigroups-0.18.1/Data-Semigroup.html#t:Semigroup) ([Result](Data-Aeson.html#t:Result) a)</span> [Source](src/Data-Aeson-Types-Internal.html#line-220){.link}                                                                
  ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ ---

</div>

</div>

</div>

<div class="top">

[fromJSON](){.def} :: [FromJSON](Data-Aeson.html#t:FromJSON) a =&gt;
[Value](Data-Aeson.html#t:Value) -&gt;
[Result](Data-Aeson.html#t:Result) a
[Source](src/Data-Aeson-Types-Instances.html#fromJSON){.link}

<div class="doc">

Convert a value from JSON, failing if the types do not match.

</div>

</div>

<div class="top">

<span class="keyword">class</span> [ToJSON](){.def} a <span
class="keyword">where</span>
[Source](src/Data-Aeson-Types-Class.html#ToJSON){.link}

<div class="doc">

A type that can be converted to JSON.

An example type and instance:

    -- Allow ourselves to write Text literals.
    {-# LANGUAGE OverloadedStrings #-}

    data Coord = Coord { x :: Double, y :: Double }

    instance ToJSON Coord where
      toJSON (Coord x y) = object ["x" .= x, "y" .= y]

      toEncoding (Coord x y) = pairs ("x" .= x <> "y" .= y)

Instead of manually writing your `ToJSON` instance, there are two
options to do it automatically:

-   [Data.Aeson.TH](Data-Aeson-TH.html) provides Template Haskell
    functions which will derive an instance at compile time. The
    generated instance is optimized for your type so will probably be
    more efficient than the following two options:
-   The compiler can provide a default generic implementation for
    `toJSON`.

To use the second, simply add a `deriving Generic` clause to your
datatype and declare a `ToJSON` instance for your datatype without
giving definitions for `toJSON` or `toEncoding`.

For example, the previous example can be simplified to a more minimal
instance:

    {-# LANGUAGE DeriveGeneric #-}

    import GHC.Generics

    data Coord = Coord { x :: Double, y :: Double } deriving Generic

    instance ToJSON Coord where
        toEncoding = genericToEncoding defaultOptions

Why do we provide an implementation for `toEncoding` here? The
`toEncoding` function is a relatively new addition to this class. To
allow users of older versions of this library to upgrade without having
to edit all of their instances or encounter surprising
incompatibilities, the default implementation of `toEncoding` uses
`toJSON`. This produces correct results, but since it performs an
intermediate conversion to a `Value`, it will be less efficient than
directly emitting an `Encoding`. Our one-liner definition of
`toEncoding` above bypasses the intermediate `Value`.

If `DefaultSignatures` doesn't give exactly the results you want, you
can customize the generic encoding with only a tiny amount of effort,
using `genericToJSON` and `genericToEncoding` with your preferred
`Options`:

    instance ToJSON Coord where
        toJSON     = genericToJSON defaultOptions
        toEncoding = genericToEncoding defaultOptions

</div>

<div class="subs minimal">

Minimal complete definition

Nothing

</div>

<div class="subs methods">

Methods

[toJSON](){.def} :: a -&gt; [Value](Data-Aeson.html#t:Value)
[Source](src/Data-Aeson-Types-Class.html#toJSON){.link}

<div class="doc">

Convert a Haskell value to a JSON-friendly intermediate type.

</div>

[toEncoding](){.def} :: a -&gt; [Encoding](Data-Aeson.html#t:Encoding)
[Source](src/Data-Aeson-Types-Class.html#toEncoding){.link}

<div class="doc">

Encode a Haskell value as JSON.

The default implementation of this method creates an intermediate
`Value` using `toJSON`. This provides source-level compatibility for
people upgrading from older versions of this library, but obviously
offers no performance advantage.

To benefit from direct encoding, you *must* provide an implementation
for this method. The easiest way to do so is by having your types
implement `Generic` using the `DeriveGeneric` extension, and then have
GHC generate a method body as follows.

    instance ToJSON Coord where
        toEncoding = genericToEncoding defaultOptions

</div>

</div>

</div>

<div class="top">

<span class="keyword">class</span> [KeyValue](){.def} kv <span
class="keyword">where</span>
[Source](src/Data-Aeson-Types-Class.html#KeyValue){.link}

<div class="doc">

A key-value pair for encoding a JSON object.

</div>

<div class="subs methods">

Methods

[(.=)](){.def} :: [ToJSON](Data-Aeson.html#t:ToJSON) v =&gt;
[Text](../text-1.2.2.1/Data-Text.html#t:Text) -&gt; v -&gt; kv <span
class="fixity">infixr 8</span><span class="rightedge"></span>
[Source](src/Data-Aeson-Types-Class.html#.%3D){.link}

</div>

</div>

Generic JSON classes and options {#g:12}
--------------------------------

<div class="top">

<span class="keyword">class</span> [GFromJSON](){.def} f <span
class="keyword">where</span>
[Source](src/Data-Aeson-Types-Class.html#GFromJSON){.link}

<div class="doc">

Class of generic representation types (`Rep`) that can be converted from
JSON.

</div>

<div class="subs methods">

Methods

[gParseJSON](){.def} :: [Options](Data-Aeson-Types.html#t:Options) -&gt;
[Value](Data-Aeson.html#t:Value) -&gt;
[Parser](Data-Aeson-Types.html#t:Parser) (f a)
[Source](src/Data-Aeson-Types-Class.html#gParseJSON){.link}

<div class="doc">

This method (applied to `defaultOptions`) is used as the default generic
implementation of `parseJSON`.

</div>

</div>

</div>

<div class="top">

<span class="keyword">class</span> [GToJSON](){.def} f <span
class="keyword">where</span>
[Source](src/Data-Aeson-Types-Class.html#GToJSON){.link}

<div class="doc">

Class of generic representation types (`Rep`) that can be converted to
JSON.

</div>

<div class="subs methods">

Methods

[gToJSON](){.def} :: [Options](Data-Aeson-Types.html#t:Options) -&gt; f
a -&gt; [Value](Data-Aeson.html#t:Value)
[Source](src/Data-Aeson-Types-Class.html#gToJSON){.link}

<div class="doc">

This method (applied to `defaultOptions`) is used as the default generic
implementation of `toJSON`.

</div>

</div>

</div>

<div class="top">

<span class="keyword">class</span> [GToEncoding](){.def} f <span
class="keyword">where</span>
[Source](src/Data-Aeson-Types-Class.html#GToEncoding){.link}

<div class="doc">

Class of generic representation types (`Rep`) that can be converted to a
JSON `Encoding`.

</div>

<div class="subs methods">

Methods

[gToEncoding](){.def} :: [Options](Data-Aeson-Types.html#t:Options)
-&gt; f a -&gt; [Encoding](Data-Aeson.html#t:Encoding)
[Source](src/Data-Aeson-Types-Class.html#gToEncoding){.link}

<div class="doc">

This method (applied to `defaultOptions`) can be used as the default
generic implementation of `toEncoding`.

</div>

</div>

</div>

<div class="top">

[genericToJSON](){.def} ::
([Generic](../base-4.8.2.0/GHC-Generics.html#t:Generic) a,
[GToJSON](Data-Aeson.html#t:GToJSON)
([Rep](../base-4.8.2.0/GHC-Generics.html#t:Rep) a)) =&gt;
[Options](Data-Aeson-Types.html#t:Options) -&gt; a -&gt;
[Value](Data-Aeson.html#t:Value)
[Source](src/Data-Aeson-Types-Class.html#genericToJSON){.link}

<div class="doc">

A configurable generic JSON creator. This function applied to
`defaultOptions` is used as the default for `toJSON` when the type is an
instance of `Generic`.

</div>

</div>

<div class="top">

[genericToEncoding](){.def} ::
([Generic](../base-4.8.2.0/GHC-Generics.html#t:Generic) a,
[GToEncoding](Data-Aeson.html#t:GToEncoding)
([Rep](../base-4.8.2.0/GHC-Generics.html#t:Rep) a)) =&gt;
[Options](Data-Aeson-Types.html#t:Options) -&gt; a -&gt;
[Encoding](Data-Aeson.html#t:Encoding)
[Source](src/Data-Aeson-Types-Class.html#genericToEncoding){.link}

<div class="doc">

A configurable generic JSON encoder. This function applied to
`defaultOptions` is used as the default for `toEncoding` when the type
is an instance of `Generic`.

</div>

</div>

<div class="top">

[genericParseJSON](){.def} ::
([Generic](../base-4.8.2.0/GHC-Generics.html#t:Generic) a,
[GFromJSON](Data-Aeson.html#t:GFromJSON)
([Rep](../base-4.8.2.0/GHC-Generics.html#t:Rep) a)) =&gt;
[Options](Data-Aeson-Types.html#t:Options) -&gt;
[Value](Data-Aeson.html#t:Value) -&gt;
[Parser](Data-Aeson-Types.html#t:Parser) a
[Source](src/Data-Aeson-Types-Class.html#genericParseJSON){.link}

<div class="doc">

A configurable generic JSON decoder. This function applied to
`defaultOptions` is used as the default for `parseJSON` when the type is
an instance of `Generic`.

</div>

</div>

<div class="top">

[defaultOptions](){.def} :: [Options](Data-Aeson-Types.html#t:Options)
[Source](src/Data-Aeson-Types-Internal.html#defaultOptions){.link}

<div class="doc">

Default encoding `Options`:

    Options
    { fieldLabelModifier      = id
    , constructorTagModifier  = id
    , allNullaryToStringTag   = True
    , omitNothingFields       = False
    , sumEncoding             = defaultTaggedObject
    , unwrapUnaryRecords      = False
    }

</div>

</div>

Inspecting `Value`s {#g:13}
===================

<div class="top">

[withObject](){.def} ::
[String](../base-4.8.2.0/Data-String.html#t:String) -&gt;
([Object](Data-Aeson.html#t:Object) -&gt;
[Parser](Data-Aeson-Types.html#t:Parser) a) -&gt;
[Value](Data-Aeson.html#t:Value) -&gt;
[Parser](Data-Aeson-Types.html#t:Parser) a
[Source](src/Data-Aeson-Types-Instances.html#withObject){.link}

<div class="doc">

`withObject expected f value` applies `f` to the `Object` when `value`
is an `Object` and fails using `typeMismatch` expected otherwise.

</div>

</div>

<div class="top">

[withText](){.def} ::
[String](../base-4.8.2.0/Data-String.html#t:String) -&gt;
([Text](../text-1.2.2.1/Data-Text.html#t:Text) -&gt;
[Parser](Data-Aeson-Types.html#t:Parser) a) -&gt;
[Value](Data-Aeson.html#t:Value) -&gt;
[Parser](Data-Aeson-Types.html#t:Parser) a
[Source](src/Data-Aeson-Types-Instances.html#withText){.link}

<div class="doc">

`withText expected f value` applies `f` to the `Text` when `value` is a
`String` and fails using `typeMismatch` expected otherwise.

</div>

</div>

<div class="top">

[withArray](){.def} ::
[String](../base-4.8.2.0/Data-String.html#t:String) -&gt;
([Array](Data-Aeson.html#t:Array) -&gt;
[Parser](Data-Aeson-Types.html#t:Parser) a) -&gt;
[Value](Data-Aeson.html#t:Value) -&gt;
[Parser](Data-Aeson-Types.html#t:Parser) a
[Source](src/Data-Aeson-Types-Instances.html#withArray){.link}

<div class="doc">

`withArray expected f value` applies `f` to the `Array` when `value` is
an `Array` and fails using `typeMismatch` expected otherwise.

</div>

</div>

<div class="top">

[withNumber](){.def} ::
[String](../base-4.8.2.0/Data-String.html#t:String) -&gt;
([Number](../attoparsec-0.13.0.2/Data-Attoparsec-Number.html#t:Number)
-&gt; [Parser](Data-Aeson-Types.html#t:Parser) a) -&gt;
[Value](Data-Aeson.html#t:Value) -&gt;
[Parser](Data-Aeson-Types.html#t:Parser) a
[Source](src/Data-Aeson-Types-Instances.html#withNumber){.link}

<div class="doc">

<div class="warning">

Deprecated: Use withScientific instead

</div>

`withNumber expected f value` applies `f` to the `Number` when `value`
is a `Number`. and fails using `typeMismatch` expected otherwise.

</div>

</div>

<div class="top">

[withScientific](){.def} ::
[String](../base-4.8.2.0/Data-String.html#t:String) -&gt;
([Scientific](../scientific-0.3.4.9/Data-Scientific.html#t:Scientific)
-&gt; [Parser](Data-Aeson-Types.html#t:Parser) a) -&gt;
[Value](Data-Aeson.html#t:Value) -&gt;
[Parser](Data-Aeson-Types.html#t:Parser) a
[Source](src/Data-Aeson-Types-Instances.html#withScientific){.link}

<div class="doc">

`withScientific expected f value` applies `f` to the `Scientific` number
when `value` is a `Number`. and fails using `typeMismatch` expected
otherwise.

</div>

</div>

<div class="top">

[withBool](){.def} ::
[String](../base-4.8.2.0/Data-String.html#t:String) -&gt;
([Bool](../base-4.8.2.0/Data-Bool.html#t:Bool) -&gt;
[Parser](Data-Aeson-Types.html#t:Parser) a) -&gt;
[Value](Data-Aeson.html#t:Value) -&gt;
[Parser](Data-Aeson-Types.html#t:Parser) a
[Source](src/Data-Aeson-Types-Instances.html#withBool){.link}

<div class="doc">

`withBool expected f value` applies `f` to the `Bool` when `value` is a
`Bool` and fails using `typeMismatch` expected otherwise.

</div>

</div>

Constructors and accessors {#g:14}
==========================

<div class="top">

<span class="keyword">data</span> [Series](){.def}
[Source](src/Data-Aeson-Types-Internal.html#Series){.link}

<div class="doc">

A series of values that, when encoded, should be separated by commas.
Since 0.11.0.0, the `.=` operator is overloaded to create either
`(Text, Value)` or `Series`. You can use Series when encoding directly
to a bytestring builder as in the following example:

    toEncoding (Person name age) = pairs ("name" .= name <> "age" .= age)

</div>

<div class="subs instances">

Instances

<div id="section.i:Series" class="show">

  -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- ---
  <span class="inst-left">[Monoid](../base-4.8.2.0/Data-Monoid.html#t:Monoid) [Series](Data-Aeson.html#t:Series)</span> [Source](src/Data-Aeson-Types-Internal.html#line-383){.link}                  
  <span class="inst-left">[Semigroup](../semigroups-0.18.1/Data-Semigroup.html#t:Semigroup) [Series](Data-Aeson.html#t:Series)</span> [Source](src/Data-Aeson-Types-Internal.html#line-375){.link}    
  -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- ---

</div>

</div>

</div>

<div class="top">

[pairs](){.def} :: [Series](Data-Aeson.html#t:Series) -&gt;
[Encoding](Data-Aeson.html#t:Encoding)
[Source](src/Data-Aeson-Encode-Functions.html#pairs){.link}

<div class="doc">

Encode a series of key/value pairs, separated by commas.

</div>

</div>

<div class="top">

[foldable](){.def} ::
([Foldable](../base-4.8.2.0/Data-Foldable.html#t:Foldable) t,
[ToJSON](Data-Aeson.html#t:ToJSON) a) =&gt; t a -&gt;
[Encoding](Data-Aeson.html#t:Encoding)
[Source](src/Data-Aeson-Encode-Functions.html#foldable){.link}

<div class="doc">

Encode a `Foldable` as a JSON array.

</div>

</div>

<div class="top">

[(.:)](){.def} :: [FromJSON](Data-Aeson.html#t:FromJSON) a =&gt;
[Object](Data-Aeson.html#t:Object) -&gt;
[Text](../text-1.2.2.1/Data-Text.html#t:Text) -&gt;
[Parser](Data-Aeson-Types.html#t:Parser) a
[Source](src/Data-Aeson-Types-Instances.html#.%3A){.link}

<div class="doc">

Retrieve the value associated with the given key of an `Object`. The
result is `empty` if the key is not present or the value cannot be
converted to the desired type.

This accessor is appropriate if the key and value *must* be present in
an object for it to be valid. If the key and value are optional, use
`.:?` instead.

</div>

</div>

<div class="top">

[(.:?)](){.def} :: [FromJSON](Data-Aeson.html#t:FromJSON) a =&gt;
[Object](Data-Aeson.html#t:Object) -&gt;
[Text](../text-1.2.2.1/Data-Text.html#t:Text) -&gt;
[Parser](Data-Aeson-Types.html#t:Parser)
([Maybe](../base-4.8.2.0/Data-Maybe.html#t:Maybe) a)
[Source](src/Data-Aeson-Types-Instances.html#.%3A%3F){.link}

<div class="doc">

Retrieve the value associated with the given key of an `Object`. The
result is `Nothing` if the key is not present, or `empty` if the value
cannot be converted to the desired type.

This accessor is most useful if the key and value can be absent from an
object without affecting its validity. If the key and value are
mandatory, use `.:` instead.

</div>

</div>

<div class="top">

[(.:!)](){.def} :: [FromJSON](Data-Aeson.html#t:FromJSON) a =&gt;
[Object](Data-Aeson.html#t:Object) -&gt;
[Text](../text-1.2.2.1/Data-Text.html#t:Text) -&gt;
[Parser](Data-Aeson-Types.html#t:Parser)
([Maybe](../base-4.8.2.0/Data-Maybe.html#t:Maybe) a)
[Source](src/Data-Aeson-Types-Instances.html#.%3A%21){.link}

<div class="doc">

Like `.:?`, but the resulting parser will fail, if the key is present
but is `Null`.

</div>

</div>

<div class="top">

[(.!=)](){.def} :: [Parser](Data-Aeson-Types.html#t:Parser)
([Maybe](../base-4.8.2.0/Data-Maybe.html#t:Maybe) a) -&gt; a -&gt;
[Parser](Data-Aeson-Types.html#t:Parser) a
[Source](src/Data-Aeson-Types-Instances.html#.%21%3D){.link}

<div class="doc">

Helper for use in combination with `.:?` to provide default values for
optional JSON object fields.

This combinator is most useful if the key and value can be absent from
an object without affecting its validity and we know a default value to
assign in that case. If the key and value are mandatory, use `.:`
instead.

Example usage:

     v1 <- o .:? "opt_field_with_dfl" .!= "default_val"
     v2 <- o .:  "mandatory_field"
     v3 <- o .:? "opt_field2"

</div>

</div>

<div class="top">

[object](){.def} :: \[[Pair](Data-Aeson-Types.html#t:Pair)\] -&gt;
[Value](Data-Aeson.html#t:Value)
[Source](src/Data-Aeson-Types-Internal.html#object){.link}

<div class="doc">

Create a `Value` from a list of name/value `Pair`s. If duplicate keys
arise, earlier keys and their associated values win.

</div>

</div>

Parsing {#g:15}
=======

<div class="top">

[json](){.def} ::
[Parser](../attoparsec-0.13.0.2/Data-Attoparsec-ByteString.html#t:Parser)
[Value](Data-Aeson.html#t:Value)
[Source](src/Data-Aeson-Parser-Internal.html#json){.link}

<div class="doc">

Parse a top-level JSON value.

The conversion of a parsed value to a Haskell value is deferred until
the Haskell value is needed. This may improve performance if only a
subset of the results of conversions are needed, but at a cost in thunk
allocation.

This function is an alias for `value`. In aeson 0.8 and earlier, it
parsed only object or array types, in conformance with the now-obsolete
RFC 4627.

</div>

</div>

<div class="top">

[json'](){.def} ::
[Parser](../attoparsec-0.13.0.2/Data-Attoparsec-ByteString.html#t:Parser)
[Value](Data-Aeson.html#t:Value)
[Source](src/Data-Aeson-Parser-Internal.html#json%27){.link}

<div class="doc">

Parse a top-level JSON value.

This is a strict version of `json` which avoids building up thunks
during parsing; it performs all conversions immediately. Prefer this
version if most of the JSON data needs to be accessed.

This function is an alias for `value'`. In aeson 0.8 and earlier, it
parsed only object or array types, in conformance with the now-obsolete
RFC 4627.

</div>

</div>

</div>

</div>

<div id="footer">

Produced by [Haddock](http://www.haskell.org/haddock/) version 2.16.1

</div>
