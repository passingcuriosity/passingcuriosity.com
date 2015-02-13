---
title: Multiple JSON encodings in Haskell
strapline: Working with multiple JSON encodings of the same data type
tags: haskell, functional programming, work, patterns
locations: Sydney, New South Wales
excerpt: 
  Here's a nice "pattern" to work with multiple JSON encodings of the same set
  of types in a Haskell program.
---

I'm currently working on a small RESTful API to control a system with
a command-line interface. The command produces JSON output but it's not really
ideal to expose in an API. This post describes the approach I took to
supporting two different JSON encodings for the same set of data types -- one
for communicating with API clients and another for communicating with the
upstream system.

I'll start with some data types to represent the data my API manages. In this
post I'll use the example of a painting robot. The robot can carry several
colours of paint but can only paint with one "active" colour at a time. Here
are some data types to represent these details:

````haskell
newtype ColourName = ColourName { unColourName :: Text }
  deriving (Eq, Show)

data Colour = Colour
    { colorName :: ColourName
    , colourRGB :: (Word8, Word8, Word8)
    }
  deriving (Eq, Show)

newtype RobotName = RobotName { unRobotName :: Text }
  deriving (Eq, Show)

data Robot = Robot
    { robotName :: RobotName
    , robotActiveColour :: ColourName
    , robotAvailableColours :: [Colour]
    }
````

JSON for the API clients
========================

The JSON encoding of `Robot` that I'd like to provide to API clients is pretty
straightforward:

````json
{ "name" : "Rosie the robot"
, "activeColour" : "red"
, "availableColours" :
    { "red"   : { "R": 255, "G":   0, "B":   0}
    , "green" : { "R":   0, "G": 255, "B":   0}
    , "blue"  : { "R":   0, "G":   0, "B": 255}
    }
}
````

The Haskell code to parse this JSON using [aeson][] is straightforward too
(though please note that the instances derived for the `newtype` are only safe
to use *within* a larger JSON structure as they result in bare JSON strings,
not objects or arrays):

[aeson]: https://hackage.haskell.org/package/aeson

````haskell
deriving instance FromJSON ColourName
deriving instance ToJSON ColourName

instance FromJSON [Colour] where
    parseJSON (Object v) = mapM (uncurry colour) $ HashMap.toList v
      where
        colour name (Object o) = Colour
                <$> parseJSON (String name)
                <*> ((,,) <$> o .: "R" <*> o .: "G" <*> o .: "B")
        colour _ _ = fail "Colour must be a JSON object"
    parseJSON _ = fail "Colours must be a JSON object"

deriving instance FromJSON RobotName
deriving instance ToJSON RobotName

instance FromJSON Robot where
    parseJSON (Object v) = Robot
        <$> v .: "name"
        <*> v .: "activeColour"
        <*> v .: "availableColours"
    parseJSON _ = fail "Robot must be a JSON object"
````

To talk to the upstream system I'll use the [process][] library to execute
a command which produces JSON on its standard output. A simple function to
invoke a command, parse the JSON, and return the value (or an error) keeps the
boilerplate contained:

[process]: https://hackage.haskell.org/package/process

````haskell
shellOutJSON
    :: (MonadError String m, MonadIO m, FromJSON a)
    => String
    -> [String]
    -> m a
shellOutJSON cmd args = do
    -- Execute the command.
    (exit_code, out, _err) <- liftIO $ readProcessWithExitCode cmd args ""

    -- Check it succeeded.
    output <- case exit_code of
        ExitSuccess -> return $ BS.pack out
        ExitFailure err -> throwError $
            "Could not execute command: error " <> show err

    -- Decode the JSON.
    case eitherDecode output of
        Left e -> throwError $ "Error decoding JSON: " <> e
        Right v -> return v
````

It's important to note that the *call site* is responsible for fixing the type
`a` of value to be parsed from the JSON. This means that `shellOutJSON` will
happily attempt to parse the JSON into *any* type you ask it to (so long as it
has a `FromJSON` instance), whether or not you should expect the command to
produce such JSON. The obvious potential problem -- a caller asking for data in
the wrong format -- occurred twice in a dozen lines of code in my current
project.

JSON for the upstream system
============================

The second JSON encoding is the one used to communicate with the command-line
application. The main difference from the API encoding is that it represents
the active colour by adding a `status` property to each colours; exactly one of
them is `active` and the rest are `available`. Rosie the robot is looks like
this:

````json
{ "name" : "Rosie the robot"
, "colours" :
    { "red"   : { "R": 255, "G":   0, "B":   0, "status": "active"}
    , "green" : { "R":   0, "G": 255, "B":   0, "status": "available"}
    , "blue"  : { "R":   0, "G":   0, "B": 255, "status": "available"}
    }
}
````

This is structure is great if you are using the data to output a nice table for
a human to read but not so great in an API.

This additional format could be implemented with new data types to represent
robots and colours and a few conversion functions (probably using the excellent
[lens][] package) to represent the weirdly formatted versions of our types. Or
I could keep the same data types but create a `newtype` wrapper around each of
them with new `FromJSON` instances implementing the new format.

[lens]: https://hackage.haskell.org/package/lens

Instead I'll add a "wrapper" type with which to distinguish a normal `Robot`
from one which should be formatted for the upstream system.

````haskell
data Upstream a = Upstream { unwrapUpstream :: a }
````

This new type doesn't "do" anything, it just tags the value it wraps and lets
me distinguish a `Robot` from an `Upstream Robot` which should be formatted for
the API and the upstream system respectively. (This is not strictly true: it
does take up memory and does cost an additional pointer dereference to
traverse). With the new `Upstream` type I can write a second `FromJSON`
instance each of my types.

If there is no special upstream format for a type the new instance can just
call the existing instance and stuff the result in an `Upstream` wrapper:

````haskell
instance FromJSON (Upstream [Colour]) where
    parseJSON j = Upstream <$> parseJSON j
````

When the upstream encoding and the API encoding do differ, I write a `FromJSON`
instance in exactly the same way I normally would (making sure to use the
`Upstream` version of any other `FromJSON` instances I use):

````haskell
instance FromJSON (Upstream Robot) where
    parseJSON (Object v) = Upstream <$>
        (Robot
            <$>  v .: "name"
            <*> (v .: "colours" >>= activeColours >>= exactlyOne)
            <*> (unwrapUpstream <$> v .: "colours"))
      where
        -- Parse a JSON object of colours into a list of 'ColourName's which
        -- have @status == "active".
        activeColours :: Value -> Parser [ColourName]
        activeColours (Object o) = (fmap fst . filter snd) <$>
            mapM (uncurry colour) (HM.toList o)
        activeColours _ = fail "Colours must be a JSON object."

        -- Given a name and a JSON value, parse a pair containing the name and
        -- whether the colour has @status == "active"@.
        colour :: Text -> Value -> Parser (ColourName, Bool)
        colour name (Object o) = (,)
            <$> parseJSON (String name)
            <*> ((String "active" ==) <$> (o .: "status"))
        colour _ _ = fail "Colour must be a JSON object."

    parseJSON _ = fail "Robot must be a JSON object"

-- | Parser to check that a list contains exactly one value.
exactlyOne :: [a] -> Parser a
exactlyOne [] = fail "Missing value"
exactlyOne [a] = pure a
exactlyOne _ = fail "More than one value"
````

With all these instances written I can update `shellOutJSON` to use the
`Upstream` instances when it interacts with the command-line program. Two small
changes -- adding `Upstream` to the `FromJSON` constraint and the "success"
pattern match -- are enough to ensure that *all* communication with the
upstream system uses the `Upstream` JSON encoding:

````haskell
shellOutJSON
    :: (MonadError String m, MonadIO m, FromJSON (Upstream a))
    => [String]
    -> m a
shellOutJSON cmd = do
    -- Execute the command.
    (exit_code, out, _err) <- liftIO $ readProcessWithExitCode cmd [] ""

    -- Check it succeeded.
    output <- case exit_code of
        ExitSuccess -> return $ BS.pack out
        ExitFailure err -> throwError $
            "Could not execute command: errno = " <> show err

    -- Decode the JSON.
    case eitherDecode output of
        Left e -> throwError $ 
            "Error decoding JSON: " <> e
        Right (Upstream v) -> return v
````

Now any call to `shellOutJSON` will automatically parse using the correct JSON
encoding and any existing code using `shellOutJSON` doesn't have to change.
Even better, any call which needs a type without an `Upstream` instance of
`FromJSON` will result in a type error at run time:

````
lib/Server.hs:115:5:
    Could not deduce (FromJSON (Upstream Colour))
      arising from a use of ‘shellOutJSON’
    from the context (MonadError String m, MonadIO m)
      bound by the type signature for
                 getColour :: (MonadError String m, MonadIO m) =>
                                      ColourName -> m Colour
      at lib/Server.hs:(109,8)-(112,18)
    In a stmt of a 'do' block: shellOutJSON cmd ["colour", "list", colour_name]
    In the expression:
      shellOutJSON cmd ["colour", "list", color_name]
    In an equation for ‘getColour’:
        getColour name
          = do { let colour_name = T.unpack $ unColourName name
                 shellOutJSON cmd ["colour", ....] }
````

The second line of the error tells you exactly what's missing: the compiler
can't find a `FromJSON` instance for `Upstream Colour`.

Conclusion
==========

By using a "wrapper" type like `Upstream a` I reduced the amount of code I need
to write and maintain (in particular, there's no converting back and forth
between `Colour` and `WeirdlyFormattedColour` data types). The values of my
various types are clearly still related and `Upstream` is completely agnostic
to the type being wrapped -- an `Upstream Robot` is just a `Robot` inside an
`Upstream` and neither the `Robot` not the `Upstream` cares about the other
part at all.

Making the wrapper parametric like this (as opposed to, for example, creating
a different `newtype` wrapper around each of the particular types) makes i
possible to write code which -- like the modified `shellOutJSON` -- doesn't
care about the *what* is being wrapped, just that it *is* wrapped.

Adding and removing the `Upstream` wrapper at the system boundary minimises the
amount code which can incorrectly use the wrong representation and, in
particular, makes it impossible for these bugs to happen in the many places
I use `shellOutJSON`. This forces me to define wrapped `FromJSON` instances for
*all* the types, even the ones that use the same JSON representation, but this
is a price I'm willing to pay for an interface that makes a class of errors
impossible.

Using this approach in my current project made the code shorter, simpler (in
terms of number of data types and functions defined), fixed two "wrong format"
bugs, and made it impossible to reintroduce them.
