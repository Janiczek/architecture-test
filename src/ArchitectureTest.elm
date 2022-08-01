module ArchitectureTest exposing
    ( TestedApp, withoutPreconditions
    , msgTest, invariantTest
    )

{-| A library for **fuzz testing TEA models** by simulating user
interactions (using fuzzed lists of Msgs).

This means:

  - start with a model (fuzzed, but you can use `Fuzz.constant` if you want to!)
  - generate random Msgs (ie. "what the user would do")
  - apply them to the model
  - test a property of the model (ie. "Cancel Msg sets currentCoins to 0")

**You get the nice property of fuzz tests that this kind of testing
will show you the minimal Msg sequence to provoke a bug.**

The `app` in doc examples below is:

    { update = update
    , getModel = identity
    , model = Fuzz.constant model
    , msgFuzzers =
        [ { precondition = \_ -> True
          , fuzzer = Fuzz.int 0 50 |> Fuzz.map AddCoins
          }
        , { precondition = \_ -> True
          , fuzzer = Fuzz.constant Cancel
          }
        , { precondition = \_ -> True
          , fuzzer = Fuzz.constant Buy
          }
        , { precondition = \_ -> True
          , fuzzer = Fuzz.constant TakeProduct
          }
        ]
    }

For a complete code example, see the examples/ directory of the repo.


# Config

@docs TestedApp, withoutPreconditions


# Tests

@docs msgTest, invariantTest

-}

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Test exposing (Test)
import Test.Runner
import Test.Runner.Failure.Extra


{-| All of these "architecture tests" are going to have something
in common: Model, update function and Msgs.

You can test both apps and packages in this way: for a `Dict` package you'd put
all the operations like `insert`, `delete`, ... into the `Msg` type and
the `Dict` type itself into the `Model`.

Note that for some tests you can eg. make the Msg fuzzer prefer
certain Msgs more if you need to test them more extensively.

You can use `Debug.toString` for `msgToString` and `modelToString` if you don't
want to provide your own custom functions. Tests can use the Debug module!

---

`update` functions can take many forms:

    updateWithoutCmds : Msg -> Model -> Model

    updateWithCmds : Msg -> Model -> ( Model, Cmd Msg )

    updateWithEffects : Msg -> Model -> ( Model, List Effect )

    updateWithOutMsg : Msg -> Model -> ( Model, Cmd msg, List OutMsg )

To encompass them, we use the `updateResult` type variable:

    update : msg -> model -> updateResult

So the `updateResult` will be some variant of `(model, Cmd msg)`.

You will get the result as one of the arguments to the test functions and will be
free to check any part of it. (This is where Effects really come in handy
compared to Cmds, as Cmds are opaque.)

If your `update` function returns Cmds but you only want to check the model, you
can wrap the `update` function in another one that drops the uninteresting parts:

    { update = \msg model -> update msg model |> Tuple.first
    , ...
    }

-}
type alias TestedApp model msg updateResult =
    { update : msg -> model -> updateResult
    , getModel : updateResult -> model
    , modelFuzzer : Fuzzer model
    , msgFuzzers :
        List
            { precondition : model -> Bool
            , fuzzer : Fuzzer msg
            }
    , msgToString : msg -> String
    , modelToString : model -> String
    , updateResultToString : updateResult -> String
    }


{-| Turn off precondition checking for fuzzed Msgs.

Preconditions help you guide the Msg generation process and start testing
interesting states faster - an optional optimization.

They do bring a risk though - of not testing certain Msgs that would be valid
during the normal application runtime. So you need to be precise about your
preconditions. `\_ -> True` will turn them off and `\_ -> False` will
effectively say "This Msg can't ever be emitted!"

-}
withoutPreconditions : List (Fuzzer msg) -> List { precondition : model -> Bool, fuzzer : Fuzzer msg }
withoutPreconditions fuzzers =
    fuzzers
        |> List.map
            (\fuzzer ->
                { precondition = \_ -> True
                , fuzzer = fuzzer
                }
            )


{-| Tests that a condition holds for a randomly generated Model
after that specific Msg is applied.

The process is as follows:

1.  get an initial `Model` (based on `TestedApp`)

2.  stuff it and a list of random `Msg`s into `update` to get a random `Model`

3.  create a `Msg` we will test

4.  stuff it and the random `Model` into `update` to get the final `Model`

5.  run your test function on the three values (random Model, tested Msg, final Model)

```
cancelReturnsMoney : Test
cancelReturnsMoney =
    msgTest "Cancelling returns all input money"
        app
        (Fuzz.constant Cancel)
    <|
        \_ _ updateResult ->
            finalModel.currentCoins
                |> Expect.equal 0
```

The test function's arguments are:

    random Model (before the tested Msg) -> tested Msg -> result of running update

-}
msgTest :
    String
    -> TestedApp model msg updateResult
    -> Fuzzer msg
    -> (model -> msg -> updateResult -> Expectation)
    -> Test
msgTest description app specificMsgFuzzer testFn =
    Test.fuzz (msgTestResultFuzzer 0 app specificMsgFuzzer testFn)
        description
    <|
        \result ->
            customFailure
                (testFn result.modelBeforeMsg result.msg result.updateResult)
                (failureStringCommon app result.modelBeforeMsg result.msg result.updateResult)


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

    init model -> random Msgs -> result of running update

-}
invariantTest :
    String
    -> TestedApp model msg updateResult
    -> (model -> List msg -> updateResult -> Expectation)
    -> Test
invariantTest description app testFn =
    Test.fuzz (invariantTestResultFuzzer 0 app testFn) description <|
        \result ->
            customFailure
                result.expectation
                (failureStringInvariant app result.initModel result.msgs result.updateResult)


minMsgListLength : Int
minMsgListLength =
    1


maxMsgListLength : Int
maxMsgListLength =
    32


continueProbability : Float
continueProbability =
    -- formula taken from elm-test Fuzz module, listOfLengthBetween
    1 - 1 / (1 + (toFloat minMsgListLength + toFloat maxMsgListLength / 2))


hasFailure : Expectation -> Bool
hasFailure exp =
    Test.Runner.getFailureReason exp /= Nothing


{-| When we generate Msgs, it's possible no precondition will hold and we won't
be able to generate a Msg. If that happens we try again from scratch.
If we reset too many times, we bail out with `Fuzz.invalid`.
-}
maxTries : Int
maxTries =
    15


type alias MsgTestResult model msg updateResult =
    { initModel : model
    , previousMsgs : List msg
    , modelBeforeMsg : model
    , msg : msg
    , updateResult : updateResult
    , expectation : Expectation
    }


type alias InvariantTestResult model msg updateResult =
    { initModel : model
    , msgs : List msg
    , updateResult : updateResult
    , expectation : Expectation
    }


msgTestResultFuzzer :
    Int
    -> TestedApp model msg updateResult
    -> Fuzzer msg
    -> (model -> msg -> updateResult -> Expectation)
    -> Fuzzer (MsgTestResult model msg updateResult)
msgTestResultFuzzer tries app specificMsgFuzzer testFn =
    if tries > maxTries then
        Fuzz.invalid <|
            "Can't generate Msgs given the current set of preconditions (tried "
                ++ String.fromInt maxTries
                ++ " times)."

    else
        let
            tryAgain : () -> Fuzzer (MsgTestResult model msg updateResult)
            tryAgain () =
                msgTestResultFuzzer (tries + 1) app specificMsgFuzzer testFn
        in
        app.modelFuzzer
            |> Fuzz.andThen
                (\initModel ->
                    let
                        addItem : model -> List msg -> Fuzzer (MsgTestResult model msg updateResult)
                        addItem currentModel msgsSoFar =
                            let
                                applicableMsgFuzzers : List (Fuzzer msg)
                                applicableMsgFuzzers =
                                    app.msgFuzzers
                                        |> List.filter (\r -> r.precondition currentModel)
                                        |> List.map .fuzzer
                            in
                            if List.isEmpty applicableMsgFuzzers then
                                tryAgain ()

                            else
                                applicableMsgFuzzers
                                    |> Fuzz.oneOf
                                    |> Fuzz.andThen
                                        (\msg ->
                                            let
                                                result : updateResult
                                                result =
                                                    app.update msg currentModel

                                                newModel : model
                                                newModel =
                                                    app.getModel result

                                                newMsgsSoFar : List msg
                                                newMsgsSoFar =
                                                    msg :: msgsSoFar
                                            in
                                            go newModel newMsgsSoFar
                                        )

                        end : model -> List msg -> Fuzzer (MsgTestResult model msg updateResult)
                        end currentModel msgsSoFar =
                            -- random walk done, let's generate the specific msg and finish
                            specificMsgFuzzer
                                |> Fuzz.map
                                    (\msg ->
                                        let
                                            result : updateResult
                                            result =
                                                app.update msg currentModel
                                        in
                                        { initModel = initModel
                                        , previousMsgs = msgsSoFar
                                        , modelBeforeMsg = currentModel
                                        , msg = msg
                                        , updateResult = result
                                        , expectation = testFn currentModel msg result
                                        }
                                    )

                        go : model -> List msg -> Fuzzer (MsgTestResult model msg updateResult)
                        go currentModel msgsSoFar =
                            let
                                length =
                                    List.length msgsSoFar
                            in
                            if length < minMsgListLength then
                                addItem currentModel msgsSoFar

                            else if length >= maxMsgListLength then
                                end currentModel msgsSoFar

                            else
                                Fuzz.weightedBool continueProbability
                                    |> Fuzz.andThen
                                        (\oneMorePlease ->
                                            if oneMorePlease then
                                                addItem currentModel msgsSoFar

                                            else
                                                end currentModel msgsSoFar
                                        )
                    in
                    go initModel []
                )


invariantTestResultFuzzer :
    Int
    -> TestedApp model msg updateResult
    -> (model -> List msg -> updateResult -> Expectation)
    -> Fuzzer (InvariantTestResult model msg updateResult)
invariantTestResultFuzzer tries app testFn =
    if tries > maxTries then
        Fuzz.invalid <|
            "Can't generate Msgs given the current set of preconditions (tried "
                ++ String.fromInt maxTries
                ++ " times)."

    else
        let
            tryAgain : () -> Fuzzer (InvariantTestResult model msg updateResult)
            tryAgain () =
                invariantTestResultFuzzer (tries + 1) app testFn
        in
        app.modelFuzzer
            |> Fuzz.andThen
                (\initModel ->
                    let
                        addItem : model -> List msg -> Fuzzer (InvariantTestResult model msg updateResult)
                        addItem currentModel msgsSoFar =
                            let
                                applicableMsgFuzzers : List (Fuzzer msg)
                                applicableMsgFuzzers =
                                    app.msgFuzzers
                                        |> List.filter (\r -> r.precondition currentModel)
                                        |> List.map .fuzzer
                            in
                            if List.isEmpty applicableMsgFuzzers then
                                tryAgain ()

                            else
                                applicableMsgFuzzers
                                    |> Fuzz.oneOf
                                    |> Fuzz.andThen
                                        (\msg ->
                                            let
                                                updateResult : updateResult
                                                updateResult =
                                                    app.update msg currentModel

                                                newModel : model
                                                newModel =
                                                    app.getModel updateResult

                                                newMsgsSoFar : List msg
                                                newMsgsSoFar =
                                                    msg :: msgsSoFar

                                                expectation : Expectation
                                                expectation =
                                                    testFn initModel newMsgsSoFar updateResult
                                            in
                                            if hasFailure expectation then
                                                Fuzz.constant
                                                    { initModel = initModel
                                                    , msgs = newMsgsSoFar
                                                    , updateResult = updateResult
                                                    , expectation = expectation
                                                    }

                                            else
                                                go newModel newMsgsSoFar (Just updateResult)
                                        )

                        end : Maybe updateResult -> List msg -> Fuzzer (InvariantTestResult model msg updateResult)
                        end lastUpdateResult msgsSoFar =
                            case lastUpdateResult of
                                Nothing ->
                                    Fuzz.invalid "Couldn't generate a valid Msg"

                                Just updateResult ->
                                    Fuzz.constant
                                        { initModel = initModel
                                        , msgs = msgsSoFar
                                        , updateResult = updateResult
                                        , expectation = Expect.pass
                                        }

                        go : model -> List msg -> Maybe updateResult -> Fuzzer (InvariantTestResult model msg updateResult)
                        go currentModel msgsSoFar lastUpdateResult =
                            let
                                length =
                                    List.length msgsSoFar
                            in
                            if length < minMsgListLength then
                                addItem currentModel msgsSoFar

                            else if length >= maxMsgListLength then
                                end lastUpdateResult msgsSoFar

                            else
                                Fuzz.weightedBool continueProbability
                                    |> Fuzz.andThen
                                        (\oneMorePlease ->
                                            if oneMorePlease then
                                                addItem currentModel msgsSoFar

                                            else
                                                end lastUpdateResult msgsSoFar
                                        )
                    in
                    go initModel [] Nothing
                )


{-| A nice custom failure message for a failing expectation.

Now if only there was a way to get rid of the "Given ..." :)

-}
customFailure : Expectation -> (String -> String) -> Expectation
customFailure expectation failureString =
    case Test.Runner.getFailureReason expectation of
        Nothing ->
            Expect.pass

        Just { description, reason } ->
            Test.Runner.Failure.Extra.format description reason
                |> failureString
                |> Expect.fail


{-| Failure message given when most of the tests fail.
-}
failureStringCommon :
    TestedApp model msg updateResult
    -> model
    -> msg
    -> updateResult
    -> String
    -> String
failureStringCommon app modelAfterMsgs msg updateResult message =
    [ "Random Model:"
    , ""
    , "    " ++ app.modelToString modelAfterMsgs
    , ""
    , "Tested Msg (failed its contract):"
    , ""
    , "    " ++ app.msgToString msg
    , ""
    , "Result of the final `update`:"
    , ""
    , "    " ++ app.updateResultToString updateResult
    , ""
    , "Failure:"
    , ""
    , indentLines message
    ]
        |> String.join "\n"


{-| Failure message given when an invariant test fails.
-}
failureStringInvariant :
    TestedApp model msg updateResult
    -> model
    -> List msg
    -> updateResult
    -> String
    -> String
failureStringInvariant app initModel msgs updateResult message =
    [ "Starting model:"
    , ""
    , "    " ++ app.modelToString initModel
    , ""
    , "Msgs applied to it (failed a contract):"
    , ""
    , "    [ " ++ (msgs |> List.map app.msgToString |> String.join ", ") ++ " ]"
    , ""
    , "Result of the final `update`:"
    , ""
    , "    " ++ app.updateResultToString updateResult
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
