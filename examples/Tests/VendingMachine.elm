module Tests.VendingMachine exposing (tests)

import Tests.VendingMachine.Good as Good
import Tests.VendingMachine.Scrooge as Scrooge
import Tests.VendingMachine.Jammed as Jammed
import Tests.VendingMachine.LazySpinner as LazySpinner
import ArchitectureTest exposing (ArchitectureTest)
import VendingMachine exposing (Msg(..), Model)
import Test exposing (Test)
import Expect
import Expect.Extra as Expect
import Random
import Fuzz exposing (Fuzzer)


-- Bugs:
-- * communist vending machine (doesn't look at the price, gives product for free and returns any money)
-- * haggle (gives product under the price)
-- * ??? (eats the money, doesn't return it, doesn't give the product)
-- TODO what if product vended but we want to buy another thing?
-- TODO investigate generating next msg in a list based on the list-so-far
-- TODO what about apps with Cmd ?
-- TODO what about apps with subscriptions ?
-- TODO what about apps with view ? can we at least export a debug file for the 0.18 debugger?
-- TODO do we need the history of Msgs in msgTest?
-- Bonus: view function (independent of the model+update) for the Msgs and Model - animated scenario viewer


tests : Test
tests =
    Test.describe "VendingMachine"
        [ Good.tests msgFuzzer specTests
          --, Scrooge.tests msgFuzzer specTests
          --, Jammed.tests msgFuzzer specTests
        , LazySpinner.tests msgFuzzer specTests
        ]


specTests : List (ArchitectureTest Msg (Model a))
specTests =
    [ addingAddsToCounter
    , cancellingReturnsAllMoney
    , buyingUnderPriceDoesntBuy
    , takingCoinsTakesThem
    , takingProductTakesIt
    , buyingAboveOrEqPriceVendsProduct
    , buyingAboveOrEqPriceMakesCoinsCounterEmpty
    , buyingAboveOrEqPriceReturnsRestOfMoney
    , priceConstant
    , currentCoinsPositive
    , returnedCoinsPositive
    ]


msgFuzzer : Fuzzer Msg
msgFuzzer =
    Fuzz.frequencyOrCrash
        [ ( 3, Fuzz.map AddCoins (Fuzz.intRange 0 Random.maxInt) )
        , ( 1, Fuzz.constant Cancel )
        , ( 1, Fuzz.constant Buy )
        , ( 1, Fuzz.constant TakeCoins )
        , ( 1, Fuzz.constant TakeProduct )
        ]


addingAddsToCounter : VendingTest a
addingAddsToCounter =
    ArchitectureTest.msgTest
        "Adding adds to the counter"
        (Fuzz.map AddCoins (Fuzz.intRange 0 Random.maxInt))
    <|
        \msgs modelSoFar msg newModel ->
            case msg of
                AddCoins amount ->
                    newModel.currentCoins
                        |> Expect.equalWithDesc "currentCoins" (modelSoFar.currentCoins + amount)

                _ ->
                    Expect.pass


cancellingReturnsAllMoney : VendingTest a
cancellingReturnsAllMoney =
    ArchitectureTest.msgTest
        "Cancelling returns all money"
        (Fuzz.constant Cancel)
    <|
        \msgs modelSoFar msg newModel ->
            newModel.returnedCoins
                |> Expect.equalWithDesc "returnedCoins" (modelSoFar.returnedCoins + modelSoFar.currentCoins)


buyingUnderPriceDoesntBuy : VendingTest a
buyingUnderPriceDoesntBuy =
    ArchitectureTest.msgTestWithPrecondition
        "Trying to buy under price doesn't buy"
        (Fuzz.constant Buy)
        (\model -> model.currentCoins < model.productPrice && not model.isProductVended)
    <|
        \msgs modelSoFar msg newModel ->
            newModel.isProductVended
                |> Expect.false "Product shouldn't be vended when buying with not enough money"


takingCoinsTakesThem : VendingTest a
takingCoinsTakesThem =
    ArchitectureTest.msgTest
        "Taking coins takes them"
        (Fuzz.constant TakeCoins)
    <|
        \msgs modelSoFar msg newModel ->
            newModel.returnedCoins
                |> Expect.equalWithDesc "returnedCoins" 0


takingProductTakesIt : VendingTest a
takingProductTakesIt =
    ArchitectureTest.msgTest
        "Taking product takes it"
        (Fuzz.constant TakeProduct)
    <|
        \msgs modelSoFar msg newModel ->
            newModel.isProductVended
                |> Expect.false "Product shouldn't be vended after taking it"


buyingAboveOrEqPriceVendsProduct : VendingTest a
buyingAboveOrEqPriceVendsProduct =
    ArchitectureTest.msgTestWithPrecondition
        "Trying to buy above or equal to price vends the product"
        (Fuzz.constant Buy)
        (\model -> model.currentCoins >= model.productPrice)
    <|
        \msgs modelSoFar msg newModel ->
            newModel.isProductVended
                |> Expect.true "Product should be vended when buying with enough money"


buyingAboveOrEqPriceMakesCoinsCounterEmpty : VendingTest a
buyingAboveOrEqPriceMakesCoinsCounterEmpty =
    ArchitectureTest.msgTestWithPrecondition
        "Trying to buy above or equal to price makes current coins counter empty"
        (Fuzz.constant Buy)
        (\model -> model.currentCoins >= model.productPrice)
    <|
        \msgs modelSoFar msg newModel ->
            newModel.currentCoins
                |> Expect.equalWithDesc "currentCoins" 0


buyingAboveOrEqPriceReturnsRestOfMoney : VendingTest a
buyingAboveOrEqPriceReturnsRestOfMoney =
    ArchitectureTest.msgTestWithPrecondition
        "Trying to buy above or equal to price returns rest of money"
        (Fuzz.constant Buy)
        (\model -> model.currentCoins >= model.productPrice)
    <|
        \msgs modelSoFar msg newModel ->
            newModel.returnedCoins
                |> Expect.equalWithDesc "returnedCoins"
                    (modelSoFar.currentCoins
                        - modelSoFar.productPrice
                        + modelSoFar.returnedCoins
                    )


priceConstant : VendingTest a
priceConstant =
    ArchitectureTest.invariantTest
        "Price is constant"
    <|
        \oldModel msgs newModel ->
            newModel.productPrice
                |> Expect.equalWithDesc "productPrice" oldModel.productPrice


currentCoinsPositive : VendingTest a
currentCoinsPositive =
    ArchitectureTest.invariantTest
        "Current coins are always positive number"
    <|
        \oldModel msgs newModel ->
            newModel.currentCoins
                |> Expect.atLeast 0


returnedCoinsPositive : VendingTest a
returnedCoinsPositive =
    ArchitectureTest.invariantTest
        "Returned coins are always positive number"
    <|
        \oldModel msgs newModel ->
            newModel.returnedCoins
                |> Expect.atLeast 0


type alias VendingTest a =
    ArchitectureTest Msg (Model a)
