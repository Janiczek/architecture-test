module Tests.VendingMachine.Scrooge exposing (tests)

import ArchitectureTest exposing (ArchitectureTest)
import Test exposing (Test)
import VendingMachine.Scrooge as Scrooge exposing (Model)
import VendingMachine exposing (Msg(..))
import Fuzz exposing (Fuzzer)


tests : Fuzzer Msg -> List (ArchitectureTest Msg Model) -> Test
tests msgFuzzer specTests =
    ArchitectureTest.spec "Scrooge Vending Machine"
        msgFuzzer
        app
        specTests


app : ArchitectureTest.App Msg Model
app =
    { update = Scrooge.update
    , init = Scrooge.init
    }
