module VendingMachine.Scrooge exposing (init, update, Model)

import VendingMachine exposing (Msg(..))


type alias Model =
    VendingMachine.Model {}


init : Model
init =
    { currentCoins = 0
    , returnedCoins = 0
    , productPrice = 25
    , isProductVended = False
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
                { model
                    | currentCoins = 0
                    , isProductVended = True
                    , returnedCoins =
                        0
                        -- here's the bug
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
