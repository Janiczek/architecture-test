module ArchitectureTest
    exposing
        ( invariantTest
        , msgTest
        , msgTestWithPrecondition
        , oneOfMsgs
        )

{-| A library for **fuzz testing TEA models** by simulating user
interactions (using fuzzed lists of Msgs).

This means:

  - start with a model (can be fuzzed, see `TestedModel` in Types module)
  - generate random Msgs (ie. "what the user would do")
  - apply them to the model
  - test a property of the model (ie. "Cancel Msg sets currentCoins to 0")

**You get the nice property of fuzz tests that this kind of testing
will show you the minimal Msg sequence to provoke a bug.**

For all the code examples in docs below, these definitions are
going to be used:

    type alias Model =
        { currentCoins : Int
        , productPrice : Int
        , isProductVended : Bool
        }

    type Msg
        = AddCoins Int
        | Cancel
        | Buy
        | TakeProduct

    init : Model
    update : Msg -> Model -> Model
    -- both not defined here

    app : TestedApp Model Msg
    app =
        { model = ConstantModel init
        , update = BeginnerUpdate update
        , msgFuzzer = msg
        }

    msg : Fuzzer Msg
    msg =
        ArchitectureTest.oneOfMsgs
            [ addCoins
            , cancel
            , buy
            , takeProduct
            ]

    addCoins : Fuzzer Msg
    addCoins =
        Fuzz.int
            |> Fuzz.map AddCoins

    cancel : Fuzzer Msg
    cancel =
        Fuzz.constant Cancel

    buy : Fuzzer Msg
    buy =
        Fuzz.constant Buy

    takeProduct : Fuzzer Msg
    takeProduct =
        Fuzz.constant TakeProduct


# Tests

@docs msgTest, msgTestWithPrecondition, invariantTest

# Helpers

@docs oneOfMsgs

-}

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import ArchitectureTest.Internal exposing (..)
import ArchitectureTest.Types exposing (..)
import Test exposing (Test)


{- TODO would a `Fuzzer (List msg)` escape hatch be worth having
   here? (ie. smart Msg list building based on previously generated
   values, instead of "dumb" Fuzz.list)
-}
{- TODO what about running the expectations after every Msg, like in
   https://github.com/rtfeldman/test-update/blob/master/src/Test/Update.elm ?
-}


{-| Tests that a condition holds for a randomly generated Model
after that specific Msg is applied.

    cancelReturnsMoney : Test
    cancelReturnsMoney =
        msgTest
            "Cancelling returns all input money"
            app
            cancelFuzzer
        <|
            \_ _ _ _ finalModel ->
                finalModel.currentCoins
                    |> Expect.equal 0

The test function's arguments are:

    init model -> random Msgs -> model before tested Msg -> tested Msg -> final model

-}
msgTest :
    String
    -> TestedApp model msg
    -> Fuzzer msg
    -> (model -> List msg -> model -> msg -> model -> Expectation)
    -> Test
msgTest description app specificMsgFuzzer testFn =
    Test.fuzz3
        (testedModelToFuzzer app.model)
        (Fuzz.list app.msgFuzzer)
        specificMsgFuzzer
        description
    <|
        \initModel msgs msg ->
            let
                update =
                    transformUpdate app.update

                modelAfterMsgs =
                    List.foldl update initModel msgs

                finalModel =
                    update msg modelAfterMsgs
            in
                customFailure
                    (testFn initModel msgs modelAfterMsgs msg finalModel)
                    (failureStringCommon initModel msgs modelAfterMsgs msg finalModel)


{-| Similar to msgTest, but only gets run when a precondition holds.

    buyingAbovePriceVendsProduct : Test
    buyingAbovePriceVendsProduct =
        msgTestWithPrecondition
            "Buying above price vends the product"
            app
            buyFuzzer
            (\model -> model.currentCoins >= model.productPrice)
        <|
            \_ _ _ _ finalModel ->
                finalModel.isProductVended
                    |> Expect.true "Product should be vended after trying to buy with enough money"

The precondition acts on the "model before specific Msg" (see `msgTest` docs).

-}
msgTestWithPrecondition :
    String
    -> TestedApp model msg
    -> Fuzzer msg
    -> (model -> Bool)
    -> (model -> List msg -> model -> msg -> model -> Expectation)
    -> Test
msgTestWithPrecondition description app specificMsgFuzzer precondition testFn =
    Test.fuzz3
        (testedModelToFuzzer app.model)
        (Fuzz.list app.msgFuzzer)
        specificMsgFuzzer
        description
    <|
        \initModel msgs msg ->
            let
                update =
                    transformUpdate app.update

                modelAfterMsgs =
                    List.foldl update initModel msgs

                finalModel =
                    update msg modelAfterMsgs
            in
                if precondition modelAfterMsgs then
                    customFailure
                        (testFn initModel msgs modelAfterMsgs msg finalModel)
                        (failureStringCommon initModel msgs modelAfterMsgs msg finalModel)
                else
                    Expect.pass


{-| Tests that a property holds no matter what Msgs we applied.

    priceConstant : Test
    priceConstant =
        invariantTest
            "Price is constant"
            app
        <|
            \initModel _ finalModel ->
                finalModel.productPrice
                    |> Expect.equal initModel.productPrice

The test function's arguments are:

    init model -> random Msgs -> final model

-}
invariantTest :
    String
    -> TestedApp model msg
    -> (model -> List msg -> model -> Expectation)
    -> Test
invariantTest description app testFn =
    Test.fuzz2
        (testedModelToFuzzer app.model)
        (Fuzz.list app.msgFuzzer)
        description
    <|
        \initModel msgs ->
            let
                update =
                    transformUpdate app.update

                finalModel =
                    List.foldl update initModel msgs
            in
                customFailure
                    (testFn initModel msgs finalModel)
                    (failureStringInvariant initModel msgs finalModel)


{-| Fuzzer that chooses a Msg from a collection of Msg fuzzers.
-}
oneOfMsgs : List (Fuzzer msg) -> Fuzzer msg
oneOfMsgs list =
    list
        |> List.map (\x -> ( 1, x ))
        |> Fuzz.frequency
