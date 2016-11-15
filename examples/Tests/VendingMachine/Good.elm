module Tests.VendingMachine.Good exposing (tests)

import ArchitectureTest exposing (ArchitectureTest)
import Test exposing (Test)
import VendingMachine.Good as Good exposing (Model)
import VendingMachine exposing (Msg(..))
import Fuzz exposing (Fuzzer)


tests : Fuzzer Msg -> List (ArchitectureTest Msg Model) -> Test
tests msgFuzzer specTests =
    -- TODO room for refactoring (handling updates by some helper function)
    ArchitectureTest.spec "Good Vending Machine"
        msgFuzzer
        app
        specTests


app : ArchitectureTest.App Msg Model
app =
    { update = Good.update
    , init = Good.init
    }
