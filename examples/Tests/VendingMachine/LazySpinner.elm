module Tests.VendingMachine.LazySpinner exposing (tests)

import ArchitectureTest exposing (ArchitectureTest)
import Test exposing (Test)
import VendingMachine.LazySpinner as LazySpinner exposing (Model)
import VendingMachine exposing (Msg(..))
import Fuzz exposing (Fuzzer)


tests : Fuzzer Msg -> List (ArchitectureTest Msg Model) -> Test
tests msgFuzzer specTests =
    ArchitectureTest.spec "Lazy Spinner Vending Machine"
        msgFuzzer
        app
        specTests


app : ArchitectureTest.App Msg Model
app =
    { update = LazySpinner.update
    , init = LazySpinner.init
    }
