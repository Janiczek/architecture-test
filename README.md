# ArchitectureTest

[<img src="https://github.com/Janiczek/elm-architecture-test/raw/master/doc/example_run.gif" width="592" height="398">](https://asciinema.org/a/6n3ax5a5um1fy60q6q4taisoi)

**tl;dr:** Fuzz-test your `update` function with random `List Msg`s!

(Click on the diagram below for full size!)

[![High-level diagram](https://github.com/Janiczek/elm-architecture-test/raw/master/doc/diagram_thumbnail.jpg)](https://github.com/Janiczek/elm-architecture-test/raw/master/doc/diagram.jpg)

To do this, you create a `Msg` fuzzer (and possibly a `Model` fuzzer) and plug it in test functions exposed here!

- [Here](https://github.com/Janiczek/elm-architecture-test/tree/master/examples) are example Models with test suites showing how to write these kinds of tests.
- Richard Feldman has a simpler library [`rtfeldman/test-update`](http://package.elm-lang.org/packages/rtfeldman/test-update/latest) with the same purpose - give it a look!
- A talk about this topic will be presented at [Elm Europe 2017](http://elmeurope.org/).
- For more discussion, see [this issue](https://github.com/elm-community/elm-test/issues/154) of elm-test.

----

## FAQ

### Can Elm enumerate over your Msg type automatically?

No, it cannot (as of version 0.18). You will have to specify your Msg fuzzers by hand:

```elm
cancel : Fuzzer Msg
cancel =
    Fuzz.constant Cancel

addCoins : Fuzzer Msg
addCoins =
    Fuzz.intRange 0 Random.maxInt
        |> Fuzz.map AddCoins

-- some other Msg fuzzers... and then, combine them:

allMsgs : Fuzzer Msg
allMsgs =
    Fuzz.oneOf
        [ cancel
        , addCoins
        , buy
        , takeProduct
        ]
```

### How can I focus my Msgs (prefer some of them more than others?)

Right, so [`Fuzz.oneOf`](http://package.elm-lang.org/packages/elm-community/elm-test/4.1.0/Fuzz#oneOf) gives all the options the same probability of being chosen. It internally uses [`Fuzz.frequency`](http://package.elm-lang.org/packages/elm-community/elm-test/4.1.0/Fuzz#frequency), which you can use too and specify your own probabilities!

```elm
preferAdding : Fuzzer Msg
preferAdding =
    Fuzz.frequency
        [ (1, cancel)
        , (10, addCoins)
        , (1, buy)
        , (1, takeProduct)
        ]
```
