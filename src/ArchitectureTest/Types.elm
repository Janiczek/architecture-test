module ArchitectureTest.Types exposing (..)

{-| # App specifications

@docs TestedApp, TestedModel, TestedUpdate
-}

import Fuzz exposing (Fuzzer)


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
