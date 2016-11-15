module VendingMachine.LazySpinner exposing (init, update, Model)

import VendingMachine exposing (Msg(..))


type alias Model =
    VendingMachine.Model { vendedProductsCounter : Int }


init : Model
init =
    { currentCoins = 0
    , returnedCoins = 0
    , productPrice = 25
    , isProductVended = False
    , vendedProductsCounter = 0
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddCoins amount ->
            { model | currentCoins = model.currentCoins + amount }

        Cancel ->
            { model
                | currentCoins = 0
                , returnedCoins = model.returnedCoins + model.currentCoins
            }

        Buy ->
            if model.currentCoins >= model.productPrice then
                let
                    ( newCounter, willFall ) =
                        ( model.vendedProductsCounter + 1
                        , model.vendedProductsCounter % 3 /= 2
                        )
                in
                    { model
                        | currentCoins = 0
                        , isProductVended = willFall
                        , returnedCoins = model.currentCoins - model.productPrice + model.returnedCoins
                        , vendedProductsCounter = newCounter
                    }
            else
                { model
                    | currentCoins = 0
                    , returnedCoins = model.currentCoins
                }

        TakeCoins ->
            { model | returnedCoins = 0 }

        TakeProduct ->
            { model | isProductVended = False }
