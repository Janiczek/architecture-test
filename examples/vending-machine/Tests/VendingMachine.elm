module Tests.VendingMachine exposing (..)

import ArchitectureTest exposing (TestedApp, TestedModel(ConstantModel), TestedUpdate(BeginnerUpdate))
import Expect
import Fuzz exposing (Fuzzer)
import Random.Pcg as Random
import Test exposing (Test)
import VendingMachine exposing (Model, Msg(..), init, update)


addCoinsFuzzer : Fuzzer Msg
addCoinsFuzzer =
    Fuzz.intRange 0 Random.maxInt
        |> Fuzz.map AddCoins


cancelFuzzer : Fuzzer Msg
cancelFuzzer =
    Fuzz.constant Cancel


buyFuzzer : Fuzzer Msg
buyFuzzer =
    Fuzz.constant Buy


takeProductFuzzer : Fuzzer Msg
takeProductFuzzer =
    Fuzz.constant TakeProduct


msgFuzzer : Fuzzer Msg
msgFuzzer =
    Fuzz.frequency
        [ ( 1, addCoinsFuzzer )
        , ( 1, cancelFuzzer )
        , ( 1, buyFuzzer )
        , ( 1, takeProductFuzzer )
        ]


app : TestedApp Model Msg
app =
    { model = ConstantModel init
    , update = BeginnerUpdate update
    , msgFuzzer = msgFuzzer
    }



-- Tests


addingAddsToCounter : Test
addingAddsToCounter =
    ArchitectureTest.msgTest
        "Adding adds to the counter"
        app
        addCoinsFuzzer
    <|
        \initModel msgs modelBeforeMsg msg finalModel ->
            case msg of
                AddCoins amount ->
                    finalModel.currentCoins
                        |> Expect.equal (modelBeforeMsg.currentCoins + amount)

                _ ->
                    Expect.pass


cancellingReturnsAllMoney : Test
cancellingReturnsAllMoney =
    ArchitectureTest.msgTest
        "Cancelling returns all money"
        app
        cancelFuzzer
    <|
        \initModel msgs modelBeforeMsg msg finalModel ->
            finalModel.currentCoins
                |> Expect.equal 0


buyingUnderPriceDoesntBuy : Test
buyingUnderPriceDoesntBuy =
    ArchitectureTest.msgTestWithPrecondition
        "Trying to buy under price doesn't buy"
        app
        buyFuzzer
        (\model -> model.currentCoins < model.productPrice && not model.isProductVended)
    <|
        \initModel msgs modelBeforeMsg msg finalModel ->
            finalModel.isProductVended
                |> Expect.false "Product shouldn't be vended when buying with not enough money"


takingProductTakesIt : Test
takingProductTakesIt =
    ArchitectureTest.msgTest
        "Taking product takes it"
        app
        takeProductFuzzer
    <|
        \initModel msgs modelBeforeMsg msg finalModel ->
            finalModel.isProductVended
                |> Expect.false "Product shouldn't be vended after taking it"


buyingAboveOrEqPriceVendsProduct : Test
buyingAboveOrEqPriceVendsProduct =
    ArchitectureTest.msgTestWithPrecondition
        "Trying to buy above or equal to price vends the product"
        app
        buyFuzzer
        (\model -> model.currentCoins >= model.productPrice)
    <|
        \initModel msgs modelBeforeMsg msg finalModel ->
            finalModel.isProductVended
                |> Expect.true "Product should be vended when buying with enough money"


buyingAboveOrEqPriceMakesCoinsCounterEmpty : Test
buyingAboveOrEqPriceMakesCoinsCounterEmpty =
    ArchitectureTest.msgTestWithPrecondition
        "Trying to buy above or equal to price makes current coins counter empty"
        app
        buyFuzzer
        (\model -> model.currentCoins >= model.productPrice)
    <|
        \initModel msgs modelBeforeMsg msg finalModel ->
            finalModel.currentCoins
                |> Expect.equal 0


priceConstant : Test
priceConstant =
    ArchitectureTest.invariantTest
        "Price is constant"
        app
    <|
        \initModel msgs finalModel ->
            finalModel.productPrice
                |> Expect.equal initModel.productPrice


currentCoinsPositive : Test
currentCoinsPositive =
    ArchitectureTest.invariantTest
        "Current coins are always positive number"
        app
    <|
        \initModel msgs finalModel ->
            finalModel.currentCoins
                |> Expect.atLeast 0
