# ArchitectureTest

**tl;dr:** Fuzz-test your `update` function with random `List Msg`s!

(Click on the diagram below for full size!)

[![High-level diagram](https://github.com/Janiczek/elm-architecture-test/raw/master/doc/diagram_thumbnail.jpg)](https://github.com/Janiczek/elm-architecture-test/raw/master/doc/diagram.jpg)

To do this, you create a `Msg` fuzzer (and possibly a `Model` fuzzer) and plug it in test functions exposed here!

- [Here](https://github.com/Janiczek/elm-architecture-test/tree/master/examples) are example Models with test suites showing how to write these kinds of tests.
- Richard Feldman has a simpler library [`rtfeldman/test-update`](http://package.elm-lang.org/packages/rtfeldman/test-update/latest) with the same purpose - give it a look!
- A talk about this topic will be presented at [Elm Europe 2017](http://elmeurope.org/).
- For more discussion, see [this issue](https://github.com/elm-community/elm-test/issues/154) of elm-test.
