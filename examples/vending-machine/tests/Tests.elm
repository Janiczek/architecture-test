module Tests exposing
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

import ArchitectureTest exposing (..)
import Expect
import Fuzz exposing (Fuzzer)
import Random
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


msg_ : Fuzzer Msg
msg_ =
    Fuzz.oneOf
        [ addCoins
        , cancel
        , buy
        , takeProduct
        ]


app : TestedApp Model Msg
app =
    { model = ConstantModel init
    , update = UpdateWithoutCmds update
    , msgFuzzer = msg_
    , modelToString = modelToString
    , msgToString = msgToString
    }


modelToString : Model -> String
modelToString { currentCoins, productPrice, isProductVended } =
    "{ currentCoins = "
        ++ String.fromInt currentCoins
        ++ ", productPrice = "
        ++ String.fromInt productPrice
        ++ ", isProductVended = "
        ++ boolToString isProductVended
        ++ " }"


msgToString : Msg -> String
msgToString msg =
    case msg of
        AddCoins coins ->
            "AddCoins " ++ String.fromInt coins

        Cancel ->
            "Cancel"

        Buy ->
            "Buy"

        TakeProduct ->
            "TakeProduct"


boolToString : Bool -> String
boolToString bool =
    case bool of
        True ->
            "True"

        False ->
            "False"



-- Tests


addingAddsToCounter : Test
addingAddsToCounter =
    ArchitectureTest.msgTest
        "Adding adds to the counter"
        app
        addCoins
    <|
        \modelBeforeMsg msg finalModel ->
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
        \_ _ finalModel ->
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
        \_ _ finalModel ->
            finalModel.isProductVended
                |> Expect.equal False
                |> Expect.onFail "Product shouldn't be vended when buying with not enough money"


takingProductTakesIt : Test
takingProductTakesIt =
    ArchitectureTest.msgTest
        "Taking product takes it"
        app
        takeProduct
    <|
        \_ _ finalModel ->
            finalModel.isProductVended
                |> Expect.equal False
                |> Expect.onFail "Product shouldn't be vended after taking it"


buyingAboveOrEqPriceVendsProduct : Test
buyingAboveOrEqPriceVendsProduct =
    ArchitectureTest.msgTestWithPrecondition
        "Trying to buy above or equal to price vends the product"
        app
        buy
        (\model -> model.currentCoins >= model.productPrice)
    <|
        \_ _ finalModel ->
            finalModel.isProductVended
                |> Expect.equal True
                |> Expect.onFail "Product should be vended when buying with enough money"


buyingAboveOrEqPriceMakesCoinsCounterEmpty : Test
buyingAboveOrEqPriceMakesCoinsCounterEmpty =
    ArchitectureTest.msgTestWithPrecondition
        "Trying to buy above or equal to price makes current coins counter empty"
        app
        buy
        (\model -> model.currentCoins >= model.productPrice)
    <|
        \_ _ finalModel ->
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
    Fuzz.oneOf
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
        \modelBeforeMsg _ finalModel ->
            finalModel.currentCoins
                |> Expect.atMost modelBeforeMsg.currentCoins
