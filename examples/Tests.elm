module Tests exposing (..)

import Test exposing (Test)
import Tests.VendingMachine as VendingMachine


all : Test
all =
    Test.describe "elm-architecture-test examples"
        [ VendingMachine.tests ]
