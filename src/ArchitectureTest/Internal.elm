module ArchitectureTest.Internal exposing (..)

import ArchitectureTest.Types exposing (..)
import Console
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Test.Runner


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
        BeginnerUpdate update ->
            update

        NormalUpdate update ->
            -- ignore the Cmd
            \msg model -> update msg model |> Tuple.first


{-| A nice custom failure message for a failing expectation.

Now if only there was a way to get rid of the "Given ..." :)

-}
customFailure : Expectation -> (String -> String) -> Expectation
customFailure expectation failureString =
    case Test.Runner.getFailure expectation of
        Nothing ->
            Expect.pass

        Just { message } ->
            message
                |> failureString
                |> Expect.fail


{-| Failure message given when most of the tests fail.
-}
failureStringCommon : model -> List msg -> model -> msg -> model -> String -> String
failureStringCommon initModel msgs modelAfterMsgs msg finalModel message =
    [ Console.dark <| "Starting model:"
    , ""
    , "    " ++ toString initModel
    , ""
    , Console.dark <| "Msgs applied to it:"
    , ""
    , "    " ++ toString msgs
    , ""
    , Console.dark <| "Model after the Msgs:"
    , ""
    , "    " ++ toString modelAfterMsgs
    , ""
    , Console.dark <| Console.bold <| "Tested Msg (failed its contract):"
    , ""
    , Console.bold <| Console.red <| "    " ++ toString msg
    , ""
    , Console.dark <| "Resulting model:"
    , ""
    , "    " ++ toString finalModel
    , ""
    , Console.dark <| Console.bold <| "Failure:"
    , ""
    , Console.bold <| Console.red <| "    " ++ message
    ]
        |> String.join "\n"


{-| Failure message given when an invariant test fails.
-}
failureStringInvariant : model -> List msg -> model -> String -> String
failureStringInvariant initModel msgs finalModel message =
    [ Console.dark <| "Starting model:"
    , ""
    , "    " ++ toString initModel
    , ""
    , Console.dark <| "Msgs applied to it (failed a contract):"
    , ""
    , "    " ++ toString msgs
    , ""
    , Console.dark <| "Resulting model:"
    , ""
    , "    " ++ toString finalModel
    , ""
    , Console.dark <| Console.bold <| "Failure:"
    , ""
    , Console.bold <| Console.red <| "    " ++ message
    ]
        |> String.join "\n"


{-| Fuzzer that chooses a value from a collection of values.
-}
oneOfValues : List a -> Fuzzer a
oneOfValues list =
    list
        |> List.map Fuzz.constant
        |> Fuzz.oneOf
