module VendingMachine exposing (Model, Msg(..), init, update)


type Msg
    = AddCoins Int
    | Cancel
    | Buy
    | TakeProduct


type alias Model =
    { currentCoins : Int
    , productPrice : Int
    , isProductVended : Bool
    }


init : Model
init =
    { currentCoins = 0
    , productPrice = 25
    , isProductVended = False
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddCoins amount ->
            { model | currentCoins = model.currentCoins + amount }

        Cancel ->
            { model | currentCoins = 0 }

        Buy ->
            if model.currentCoins >= model.productPrice then
                { model
                    | currentCoins = 0
                    , isProductVended = True
                }
            else
                { model | currentCoins = 0 }

        TakeProduct ->
            { model | isProductVended = False }
