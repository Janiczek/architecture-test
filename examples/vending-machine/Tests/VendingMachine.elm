module Tests.VendingMachine
    exposing
        ( addingAddsToCounter
        , buyingAboveOrEqPriceMakesCoinsCounterEmpty
        , buyingAboveOrEqPriceVendsProduct
        , buyingUnderPriceDoesntBuy
        , cancellingReturnsAllMoney
        , currentCoinsPositive
        , onlyAddCoinsAddsCoins
        , priceConstant
        , takingProductTakesIt
        )

import ArchitectureTest.Types exposing (..)
import ArchitectureTest
import Expect
import Fuzz exposing (Fuzzer)
import Random.Pcg as Random
import Test exposing (Test)
import VendingMachine exposing (Model, Msg(..), init, update)


addCoins : Fuzzer Msg
addCoins =
    Fuzz.intRange 0 Random.maxInt
        |> Fuzz.map AddCoins


cancel : Fuzzer Msg
cancel =
    Fuzz.constant Cancel


buy : Fuzzer Msg
buy =
    Fuzz.constant Buy


takeProduct : Fuzzer Msg
takeProduct =
    Fuzz.constant TakeProduct


msg : Fuzzer Msg
msg =
    ArchitectureTest.oneOfMsgs
        [ addCoins
        , cancel
        , buy
        , takeProduct
        ]


app : TestedApp Model Msg
app =
    { model = ConstantModel init
    , update = BeginnerUpdate update
    , msgFuzzer = msg
    }



-- Tests


addingAddsToCounter : Test
addingAddsToCounter =
    ArchitectureTest.msgTest
        "Adding adds to the counter"
        app
        addCoins
    <|
        \_ _ modelBeforeMsg msg finalModel ->
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
        cancel
    <|
        \_ _ _ _ finalModel ->
            finalModel.currentCoins
                |> Expect.equal 0


buyingUnderPriceDoesntBuy : Test
buyingUnderPriceDoesntBuy =
    ArchitectureTest.msgTestWithPrecondition
        "Trying to buy under price doesn't buy"
        app
        buy
        (\model -> model.currentCoins < model.productPrice && not model.isProductVended)
    <|
        \_ _ _ _ finalModel ->
            finalModel.isProductVended
                |> Expect.false "Product shouldn't be vended when buying with not enough money"


takingProductTakesIt : Test
takingProductTakesIt =
    ArchitectureTest.msgTest
        "Taking product takes it"
        app
        takeProduct
    <|
        \_ _ _ _ finalModel ->
            finalModel.isProductVended
                |> Expect.false "Product shouldn't be vended after taking it"


buyingAboveOrEqPriceVendsProduct : Test
buyingAboveOrEqPriceVendsProduct =
    ArchitectureTest.msgTestWithPrecondition
        "Trying to buy above or equal to price vends the product"
        app
        buy
        (\model -> model.currentCoins >= model.productPrice)
    <|
        \_ _ _ _ finalModel ->
            finalModel.isProductVended
                |> Expect.true "Product should be vended when buying with enough money"


buyingAboveOrEqPriceMakesCoinsCounterEmpty : Test
buyingAboveOrEqPriceMakesCoinsCounterEmpty =
    ArchitectureTest.msgTestWithPrecondition
        "Trying to buy above or equal to price makes current coins counter empty"
        app
        buy
        (\model -> model.currentCoins >= model.productPrice)
    <|
        \_ _ _ _ finalModel ->
            finalModel.currentCoins
                |> Expect.equal 0


priceConstant : Test
priceConstant =
    ArchitectureTest.invariantTest
        "Price is constant"
        app
    <|
        \initModel _ finalModel ->
            finalModel.productPrice
                |> Expect.equal initModel.productPrice


currentCoinsPositive : Test
currentCoinsPositive =
    ArchitectureTest.invariantTest
        "Current coins are always positive number"
        app
    <|
        \_ _ finalModel ->
            finalModel.currentCoins
                |> Expect.atLeast 0


msgWithoutAddCoins : Fuzzer Msg
msgWithoutAddCoins =
    ArchitectureTest.oneOfMsgs
        [ cancel
        , buy
        , takeProduct
        ]


onlyAddCoinsAddsCoins : Test
onlyAddCoinsAddsCoins =
    ArchitectureTest.msgTest
        "Only 'AddCoins' adds coins"
        app
        msgWithoutAddCoins
    <|
        \_ _ modelBeforeMsg _ finalModel ->
            finalModel.currentCoins
                |> Expect.atMost modelBeforeMsg.currentCoins
