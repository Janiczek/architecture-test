module ArchitectureTest
    exposing
        ( TestedApp
        , TestedModel(..)
        , TestedUpdate(..)
        , invariantTest
        , msgTest
        , msgTestWithPrecondition
        , orthogonalityTest
        )

{-| A library for **fuzz testing TEA models** by simulating user
interactions (using fuzzed lists of Msgs).

This means:

  - start with a model (can be fuzzed, see `TestedModel` below)
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
        , msgFuzzer = msgFuzzer
        }

    msgFuzzer : Fuzzer Msg
    msgFuzzer =
        Fuzz.frequency
            [ ( 1, addCoinsFuzzer )
            , ( 1, cancelFuzzer )
            , ( 1, buyFuzzer )
            , ( 1, takeProductFuzzer )
            ]

    addCoinsFuzzer : Fuzzer Msg
    addCoinsFuzzer =
        Fuzz.int
            |> Fuzz.map AddCoins

    cancelFuzzer : Fuzzer Msg
    cancelFuzzer =
        Fuzz.constant Cancel


    -- similarly for buyFuzzer and takeProductFuzzer


# App specifications

@docs TestedApp, TestedModel, TestedUpdate


# Tests

@docs msgTest, msgTestWithPrecondition, invariantTest, orthogonalityTest

-}

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Test exposing (Test)
import Tuple


{- TODO would a `Fuzzer (List msg)` escape hatch be worth having
   here? (ie. smart Msg list building based on previously generated
   values, instead of "dumb" Fuzz.list)
-}


{-| All of these "architecture tests" are going to have something
in common: Model, update function and Msgs.

Note that for some tests you can eg. make the Msg fuzzer prefer
certain Msgs more if you need to test them more extensively.

-}
type alias TestedApp model msg =
    { model : TestedModel model
    , update : TestedUpdate model msg
    , msgFuzzer : Fuzzer msg
    }


{-| The strategy for choosing an init model to which the Msgs
will be applied.
-}
type TestedModel model
    = ConstantModel model
    | FuzzedModel (Fuzzer model)
    | OneOfModels (List model)


{-| Main applications can be of two types: beginner (no Cmds)
and normal (Cmds present).

For custom `update` functions returning eg. triples etc.,
just use `BeginnerUpdate` with a function that returns just the model part of
the result:

    update : Msg -> Model -> {model : Model, cmd : Cmd Msg, outMsg : OutMsg}

    BeginnerUpdate (\msg model -> update msg model |> .model)

-}
type TestedUpdate model msg
    = BeginnerUpdate (msg -> model -> model)
    | NormalUpdate (msg -> model -> ( model, Cmd msg ))


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
            testFn initModel msgs modelAfterMsgs msg finalModel


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
            if precondition finalModel then
                testFn initModel msgs modelAfterMsgs msg finalModel
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
            testFn initModel msgs finalModel


{-| TODO: the current implementation won't work! If it gets wrong Msg
(predicate -> False) from the fuzzer, it just passes, instead it should try
to generate another Msg until it succeeds (predicate -> True), and then use that.

Tests that no other Msg than the one specified changes a given part
of the model.

The next example can be thought of in this way:

> If the Msg is not Cancel, the amount of current coins shouldn't decrease.

    onlyCancelRemovesCoins : Test
    onlyCancelRemovesCoins =
        orthogonalityTest
            "Only 'Cancel' removes coins"
            app
            (\msg -> msg /= Cancel)
        <|
            \_ _ modelBeforeMsg _ finalModel ->
                finalModel.currentCoins
                    |> Expect.atLeast modelBeforeMsg.currentCoins


    -- or with union type with data:

    isAddCoins : Msg -> Bool
    isAddCoins msg =
        case msg of
            AddCoins _ ->
                True

            _ ->
                False

    onlyAddCoinsAddsCoins : Test
    onlyAddCoinsAddsCoins =
        orthogonalityTest "Only 'AddCoins' adds coins"
            app
            (\msg -> not (isAddCoins msg))
        <|
            \_ _ modelBeforeMsg _ finalModel ->
                finalModel.currentCoins
                    |> Expect.atMost modelBeforeMsg.currentCoins

The Msg condition acts on the "specific Msg" (see `msgTest` docs).
Note that you don't specify its fuzzer - it gets created with
the same fuzzer as all the other Msgs.

-}
orthogonalityTest :
    String
    -> TestedApp model msg
    -> (msg -> Bool)
    -> (model -> List msg -> model -> msg -> model -> Expectation)
    -> Test
orthogonalityTest description app msgCondition testFn =
    Test.fuzz3
        (testedModelToFuzzer app.model)
        (Fuzz.list app.msgFuzzer)
        app.msgFuzzer
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
            if msgCondition msg then
                testFn initModel msgs modelAfterMsgs msg finalModel
            else
                Expect.pass



-- helpers


{-| Create a fuzzer from the Model specification.
-}
testedModelToFuzzer : TestedModel model -> Fuzzer model
testedModelToFuzzer testedModel =
    case testedModel of
        ConstantModel model ->
            Fuzz.constant model

        FuzzedModel modelFuzzer ->
            modelFuzzer

        OneOfModels modelList ->
            oneOf modelList


{-| Prepare the `update` function for use in the tests, ie. drop Cmds.
-}
transformUpdate : TestedUpdate model msg -> (msg -> model -> model)
transformUpdate testedUpdate =
    case testedUpdate of
        BeginnerUpdate update ->
            update

        NormalUpdate update ->
            -- ignore the Cmd
            \msg model -> update msg model |> Tuple.first


{-| Fuzzer that chooses a value from a collection of values.
-}
oneOf : List a -> Fuzzer a
oneOf list =
    -- Why isn't this in elm-test?
    list
        |> List.map (\x -> ( 1, Fuzz.constant x ))
        |> Fuzz.frequency
