module Tests exposing (..)

import InteractionTest exposing (InteractionTest, App)
import Fuzz exposing (Fuzzer)
import Test exposing (Test)
import Expect
import VendingMachine exposing (Msg(..))
import Random


all : Test
all =
    Test.describe "A Test Suite"
        [ InteractionTest.spec "Vending Machine"
            msgFuzzer
            app
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
        ]


msgFuzzer : Fuzzer VendingMachine.Msg
msgFuzzer =
    Fuzz.frequencyOrCrash
        [ ( 3, Fuzz.map AddCoins (Fuzz.intRange 0 Random.maxInt) )
        , ( 1, Fuzz.constant Cancel )
        , ( 1, Fuzz.constant Buy )
        , ( 1, Fuzz.constant TakeCoins )
        , ( 1, Fuzz.constant TakeProduct )
        ]


type alias VendingTest =
    InteractionTest VendingMachine.Msg VendingMachine.Model


addingAddsToCounter : VendingTest
addingAddsToCounter =
    InteractionTest.msgTest
        "Adding adds to the counter"
        (Fuzz.map AddCoins (Fuzz.intRange 0 Random.maxInt))
    <|
        \msgs modelSoFar msg newModel ->
            case msg of
                AddCoins amount ->
                    newModel.currentCoins
                        |> Expect.equal (modelSoFar.currentCoins + amount)

                _ ->
                    Expect.pass


cancellingReturnsAllMoney : VendingTest
cancellingReturnsAllMoney =
    InteractionTest.msgTest
        "Cancelling returns all money"
        (Fuzz.constant Cancel)
    <|
        \msgs modelSoFar msg newModel ->
            newModel.returnedCoins
                |> Expect.equal (modelSoFar.returnedCoins + modelSoFar.currentCoins)


buyingUnderPriceDoesntBuy : VendingTest
buyingUnderPriceDoesntBuy =
    InteractionTest.msgTestWithPrecondition
        "Trying to buy under price doesn't buy"
        (Fuzz.constant Buy)
        (\model -> model.currentCoins < model.productPrice && not model.isProductVended)
    <|
        \msgs modelSoFar msg newModel ->
            newModel.isProductVended
                |> Expect.false "Product shouldn't be vended when buying with not enough money"


takingCoinsTakesThem : VendingTest
takingCoinsTakesThem =
    InteractionTest.msgTest
        "Taking coins takes them"
        (Fuzz.constant TakeCoins)
    <|
        \msgs modelSoFar msg newModel ->
            newModel.returnedCoins
                |> Expect.equal 0


takingProductTakesIt : VendingTest
takingProductTakesIt =
    InteractionTest.msgTest
        "Taking product takes it"
        (Fuzz.constant TakeProduct)
    <|
        \msgs modelSoFar msg newModel ->
            newModel.isProductVended
                |> Expect.false "Product shouldn't be vended after taking it"


buyingAboveOrEqPriceVendsProduct : VendingTest
buyingAboveOrEqPriceVendsProduct =
    InteractionTest.msgTestWithPrecondition
        "Trying to buy above or equal to price vends the product"
        (Fuzz.constant Buy)
        (\model -> model.currentCoins >= model.productPrice)
    <|
        \msgs modelSoFar msg newModel ->
            newModel.isProductVended
                |> Expect.true "Product should be vended when buying with enough money"


buyingAboveOrEqPriceMakesCoinsCounterEmpty : VendingTest
buyingAboveOrEqPriceMakesCoinsCounterEmpty =
    InteractionTest.msgTestWithPrecondition
        "Trying to buy above or equal to price makes current coins counter empty"
        (Fuzz.constant Buy)
        (\model -> model.currentCoins >= model.productPrice)
    <|
        \msgs modelSoFar msg newModel ->
            newModel.currentCoins
                |> Expect.equal 0


buyingAboveOrEqPriceReturnsRestOfMoney : VendingTest
buyingAboveOrEqPriceReturnsRestOfMoney =
    InteractionTest.msgTestWithPrecondition
        "Trying to buy above or equal to price returns rest of money"
        (Fuzz.constant Buy)
        (\model -> model.currentCoins >= model.productPrice)
    <|
        \msgs modelSoFar msg newModel ->
            newModel.returnedCoins
                |> Expect.equal
                    (modelSoFar.currentCoins
                        - modelSoFar.productPrice
                        + modelSoFar.returnedCoins
                    )


priceConstant : VendingTest
priceConstant =
    InteractionTest.invariantTest
        "Price is constant"
    <|
        \oldModel msgs newModel ->
            newModel.productPrice
                |> Expect.equal oldModel.productPrice


currentCoinsPositive : VendingTest
currentCoinsPositive =
    InteractionTest.invariantTest
        "Current coins are always positive number"
    <|
        \oldModel msgs newModel ->
            newModel.currentCoins
                |> Expect.atLeast 0


returnedCoinsPositive : VendingTest
returnedCoinsPositive =
    InteractionTest.invariantTest
        "Returned coins are always positive number"
    <|
        \oldModel msgs newModel ->
            newModel.returnedCoins
                |> Expect.atLeast 0


app : InteractionTest.App VendingMachine.Msg VendingMachine.Model
app =
    { update = VendingMachine.update
    , init = VendingMachine.init
    }
