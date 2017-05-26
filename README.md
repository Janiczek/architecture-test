# ArchitectureTest

**tl;dr:** Fuzz-test your `update` function with random `List Msg`s!

![High-level diagram](https://github.com/Janiczek/elm-architecture-test/raw/master/doc/diagram.jpg)

To do this, you create a `Msg` fuzzer (and possibly a `Model` fuzzer) and just plug it in!

- [Here's](https://github.com/Janiczek/elm-architecture-test/tree/master/examples/vending-machine) an example project with [tests](https://github.com/Janiczek/elm-architecture-test/tree/master/examples/vending-machine/Tests/VendingMachine.elm).
- A talk about this topic will be presented at [Elm Europe 2017](http://elmeurope.org/).
- For more discussion, see [this issue](https://github.com/elm-community/elm-test/issues/154) of elm-test.
