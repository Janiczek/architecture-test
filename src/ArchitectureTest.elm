module ArchitectureTest
    exposing
        ( App
        , ArchitectureTest
        , msgTest
        , msgTestWithPrecondition
        , invariantTest
        , spec
        )

{-|
TODO
@docs App, ArchitectureTest
@docs msgTest, msgTestWithPrecondition, invariantTest
@docs spec
-}

import Fuzz exposing (Fuzzer)
import Test exposing (Test)
import Expect exposing (Expectation)


{-| TODO
-}
type ArchitectureTest msg model
    = MsgTest (MsgTestRecord msg model)
    | MsgTestWithPrecondition (MsgTestWithPreconditionRecord msg model)
    | InvariantTest (InvariantTestRecord msg model)


type alias MsgTestRecord msg model =
    { desc : String
    , fn : MsgTestFn msg model
    , msgFuzzer : Fuzzer msg
    }


type alias MsgTestWithPreconditionRecord msg model =
    { desc : String
    , fn : MsgTestFn msg model
    , precondition : MsgTestPrecondition model
    , msgFuzzer : Fuzzer msg
    }


type alias InvariantTestRecord msg model =
    { desc : String
    , fn : InvariantTestFn msg model
    }


type alias MsgTestPrecondition model =
    model -> Bool


type alias MsgTestFn msg model =
    List msg -> model -> msg -> model -> Expectation


type alias InvariantTestFn msg model =
    model -> List msg -> model -> Expectation


{-| TODO
-}
msgTest :
    String
    -> Fuzzer msg
    -> MsgTestFn msg model
    -> ArchitectureTest msg model
msgTest desc msgFuzzer fn =
    MsgTest
        { desc = desc
        , fn = fn
        , msgFuzzer = msgFuzzer
        }


{-| TODO
-}
msgTestWithPrecondition :
    String
    -> Fuzzer msg
    -> MsgTestPrecondition model
    -> MsgTestFn msg model
    -> ArchitectureTest msg model
msgTestWithPrecondition desc msgFuzzer precondition fn =
    MsgTestWithPrecondition
        { desc = desc
        , fn = fn
        , msgFuzzer = msgFuzzer
        , precondition = precondition
        }


{-| TODO
-}
invariantTest :
    String
    -> InvariantTestFn msg model
    -> ArchitectureTest msg model
invariantTest desc fn =
    InvariantTest
        { desc = desc
        , fn = fn
        }


{-| TODO
-}
type alias App msg model =
    { init : model
    , update : msg -> model -> model
    }


{-| TODO
-}
spec :
    String
    -> Fuzzer msg
    -> App msg model
    -> List (ArchitectureTest msg model)
    -> Test
spec name msgFuzzer app tests =
    let
        msgListFuzzer =
            Fuzz.list msgFuzzer
    in
        tests
            |> List.map (architectureTestToTest msgListFuzzer app)
            |> Test.describe name


architectureTestToTest : Fuzzer (List msg) -> App msg model -> ArchitectureTest msg model -> Test
architectureTestToTest msgListFuzzer app test =
    case test of
        MsgTest test ->
            msgTestToTest test msgListFuzzer app

        MsgTestWithPrecondition test ->
            msgTestWithPreconditionToTest test msgListFuzzer app

        InvariantTest test ->
            invariantTestToTest test msgListFuzzer app


msgTestToTest : MsgTestRecord msg model -> Fuzzer (List msg) -> App msg model -> Test
msgTestToTest test msgListFuzzer app =
    Test.fuzz2 msgListFuzzer test.msgFuzzer test.desc <|
        \msgs msg ->
            let
                modelJustBeforeMsg =
                    List.foldl app.update app.init msgs

                lastModel =
                    app.update msg modelJustBeforeMsg
            in
                test.fn msgs modelJustBeforeMsg msg lastModel


msgTestWithPreconditionToTest : MsgTestWithPreconditionRecord msg model -> Fuzzer (List msg) -> App msg model -> Test
msgTestWithPreconditionToTest test msgListFuzzer app =
    Test.fuzz2 msgListFuzzer test.msgFuzzer test.desc <|
        \msgs msg ->
            let
                modelJustBeforeMsg =
                    List.foldl app.update app.init msgs

                lastModel =
                    app.update msg modelJustBeforeMsg
            in
                if test.precondition modelJustBeforeMsg then
                    test.fn msgs modelJustBeforeMsg msg lastModel
                else
                    Expect.pass


invariantTestToTest : InvariantTestRecord msg model -> Fuzzer (List msg) -> App msg model -> Test
invariantTestToTest test msgListFuzzer app =
    Test.fuzz msgListFuzzer test.desc <|
        \msgs ->
            let
                lastModel =
                    List.foldl app.update app.init msgs
            in
                test.fn app.init msgs lastModel
