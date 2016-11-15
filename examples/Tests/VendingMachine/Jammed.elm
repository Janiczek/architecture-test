module Tests.VendingMachine.Jammed exposing (tests)

import ArchitectureTest exposing (ArchitectureTest)
import Test exposing (Test)
import VendingMachine.Jammed as Jammed exposing (Model)
import VendingMachine exposing (Msg(..))
import Fuzz exposing (Fuzzer)


tests : Fuzzer Msg -> List (ArchitectureTest Msg Model) -> Test
tests msgFuzzer specTests =
    ArchitectureTest.spec "Jammed Vending Machine"
        msgFuzzer
        app
        specTests


app : ArchitectureTest.App Msg Model
app =
    { update = Jammed.update
    , init = Jammed.init
    }
