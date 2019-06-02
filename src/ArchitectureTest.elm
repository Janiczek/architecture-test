module ArchitectureTest
    exposing
        ( TestedApp
        , TestedModel(..)
        , TestedUpdate(..)
        , invariantTest
        , msgTest
        , msgTestWithPrecondition
        )

{-| A library for **fuzz testing TEA models** by simulating user
interactions (using fuzzed lists of Msgs).

This means:

  - start with a model (can be fuzzed, see `TestedModel`)
  - generate random Msgs (ie. "what the user would do")
  - apply them to the model
  - test a property of the model (ie. "Cancel Msg sets currentCoins to 0")

**You get the nice property of fuzz tests that this kind of testing
will show you the minimal Msg sequence to provoke a bug.**

The `app` in doc examples below is:

    { model = ConstantModel model
    , update = UpdateWithoutCmds update
    , msgFuzzer =
        Fuzz.oneOf
            [ Fuzz.int 0 50 |> Fuzz.map AddCoins
            , Fuzz.constant Cancel
            , Fuzz.constant Buy
            , Fuzz.constant TakeProduct
            ]
    }

For a complete code example, see the examples/ directory of the repo.


# Tests

@docs msgTest, msgTestWithPrecondition, invariantTest


# Types

@docs TestedApp, TestedModel, TestedUpdate

-}

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Test exposing (Test)
import Test.Runner
import Test.Runner.Failure



{- TODO would a `Fuzzer (List msg)` escape hatch be worth having
   here? (ie. smart Msg list building based on previously generated
   values, instead of "dumb" Fuzz.list)
-}
{- TODO what about running the expectations after every Msg, like in
   https://github.com/rtfeldman/test-update/blob/master/src/Test/Update.elm ?
-}


{-| Tests that a condition holds for a randomly generated Model
after that specific Msg is applied.

The process is as follows:

1.  get an initial `Model` (based on `TestedApp`)

2.  stuff it and a list of random `Msg`s into `update` to get a random `Model`

3.  create a `Msg` we will test

4.  stuff it and the random `Model` into `update` to get the final `Model`

5.  run your test function on the three values (random Model, tested Msg, final Model)


    cancelReturnsMoney : Test
    cancelReturnsMoney =
        msgTest "Cancelling returns all input money"
            app
            (Fuzz.constant Cancel)
        <|
            \_ _ finalModel -> finalModel.currentCoins |> Expect.equal 0


The test function's arguments are:

    random Model (before the tested Msg) -> tested Msg -> final Model

-}
msgTest :
    String
    -> TestedApp model msg
    -> Fuzzer msg
    -> (model -> msg -> model -> Expectation)
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
                (testFn modelAfterMsgs msg finalModel)
                (failureStringCommon app modelAfterMsgs msg finalModel)


{-| Similar to msgTest, but only gets run when a precondition holds.

    buyingAbovePriceVendsProduct : Test
    buyingAbovePriceVendsProduct =
        msgTestWithPrecondition "Buying above price vends the product"
            app
            (Fuzz.constant Buy)
            (\model -> model.currentCoins >= model.productPrice)
        <|
            \_ _ finalModel ->
                finalModel.isProductVended
                    |> Expect.true "Product should be vended after trying to buy with enough money"

The precondition acts on the "model before specific Msg" (see `msgTest` docs).

-}
msgTestWithPrecondition :
    String
    -> TestedApp model msg
    -> Fuzzer msg
    -> (model -> Bool)
    -> (model -> msg -> model -> Expectation)
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
                    (testFn modelAfterMsgs msg finalModel)
                    (failureStringCommon app modelAfterMsgs msg finalModel)

            else
                Expect.pass


{-| Tests that a property holds no matter what Msgs we applied.

    priceConstant : Test
    priceConstant =
        invariantTest "Price is constant"
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
                (failureStringInvariant app initModel msgs finalModel)


{-| All of these "architecture tests" are going to have something
in common: Model, update function and Msgs.

Note that for some tests you can eg. make the Msg fuzzer prefer
certain Msgs more if you need to test them more extensively.

-}
type alias TestedApp model msg =
    { model : TestedModel model
    , update : TestedUpdate model msg
    , msgFuzzer : Fuzzer msg
    , msgToString : msg -> String
    , modelToString : model -> String
    }


{-| The strategy for choosing an init model to which the Msgs
will be applied.
-}
type TestedModel model
    = ConstantModel model
    | FuzzedModel (Fuzzer model)
    | OneOfModels (List model)


{-| Main applications can be of two types: those without Cmds
and normal (Cmds present).

For custom `update` functions returning eg. triples etc.,
just use `UpdateWithoutCmds` with a function that returns just the model part of
the result:

    update : Msg -> Model -> {model : Model, cmd : Cmd Msg, outMsg : OutMsg}

    UpdateWithoutCmds (\msg model -> update msg model |> .model)

-}
type TestedUpdate model msg
    = UpdateWithoutCmds (msg -> model -> model)
    | NormalUpdate (msg -> model -> ( model, Cmd msg ))


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
            oneOfValues modelList


{-| Prepare the `update` function for use in the tests, ie. drop Cmds.
-}
transformUpdate : TestedUpdate model msg -> (msg -> model -> model)
transformUpdate testedUpdate =
    case testedUpdate of
        UpdateWithoutCmds update ->
            update

        NormalUpdate update ->
            -- ignore the Cmd
            \msg model -> update msg model |> Tuple.first


{-| A nice custom failure message for a failing expectation.

Now if only there was a way to get rid of the "Given ..." :)

-}
customFailure : Expectation -> (String -> String) -> Expectation
customFailure expectation failureString =
    case Test.Runner.getFailureReason expectation of
        Nothing ->
            Expect.pass

        Just { description, reason } ->
            Test.Runner.Failure.format description reason
                |> failureString
                |> Expect.fail


{-| Failure message given when most of the tests fail.
-}
failureStringCommon : TestedApp model msg -> model -> msg -> model -> String -> String
failureStringCommon { modelToString, msgToString } modelAfterMsgs msg finalModel message =
    [ "Random Model:"
    , ""
    , "    " ++ modelToString modelAfterMsgs
    , ""
    , "Tested Msg (failed its contract):"
    , ""
    , "    " ++ msgToString msg
    , ""
    , "Resulting Model:"
    , ""
    , "    " ++ modelToString finalModel
    , ""
    , "Failure:"
    , ""
    , indentLines message
    ]
        |> String.join "\n"


{-| Failure message given when an invariant test fails.
-}
failureStringInvariant : TestedApp model msg -> model -> List msg -> model -> String -> String
failureStringInvariant { modelToString, msgToString } initModel msgs finalModel message =
    [ "Starting model:"
    , ""
    , "    " ++ modelToString initModel
    , ""
    , "Msgs applied to it (failed a contract):"
    , ""
    , "    [ " ++ (msgs |> List.map msgToString |> String.join ", ") ++ " ]"
    , ""
    , "Resulting model:"
    , ""
    , "    " ++ modelToString finalModel
    , ""
    , "Failure:"
    , ""
    , indentLines message
    ]
        |> String.join "\n"


{-| Fuzzer that chooses a value from a collection of values.
-}
oneOfValues : List a -> Fuzzer a
oneOfValues list =
    list
        |> List.map Fuzz.constant
        |> Fuzz.oneOf


indentLines : String -> String
indentLines message =
    message
        |> String.lines
        |> List.map (\line -> "    " ++ line)
        |> String.join "\n"
