module VendingMachine exposing (..)


type Msg
    = AddCoins Int
    | Cancel
    | Buy
    | TakeCoins
    | TakeProduct


type alias Model =
    { currentCoins : Int
    , returnedCoins : Int
    , productPrice : Int
    , isProductVended : Bool
    }


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
                    , returnedCoins = model.currentCoins - model.productPrice + model.returnedCoins
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



-- Bugs:
-- * communist vending machine (doesn't look at the price, gives product for free and returns any money)
-- * haggle (gives product under the price)
-- * spins but not enough (does give every second product as a result)
-- * Scrooge McDuck (eats the money, doesn't return it, doesn't give the product)
-- * Scrooge McDuck (doesn't return extra money when vending product)
-- * secret code (give it 1 coin 5 times, gives you the product even though it's more expensive)
-- * jammed doors (product vended but can't take it)
-- Now for some vending machines that could implement this.
-- TODO what if product vended but we want to buy another thing?
-- TODO there might be a problem: what if the model internally was an ADT
-- (like a finite state machine)? what if it did things differently than our
-- mandatory model? should we have a function that takes their model and
-- makes our model from it?
-- "I have looked at various 'vending machine fail' images and you wouldn't
-- believe what can happen with a simple vending machine!"
-- Specifications:
-- TODO investigate generating next msg in a list based on the list-so-far
--test : MandatoryModel a -> MandatoryModel a -> List Msg -> Test
--test firstModel lastModel msgs =
-- adding money adds it to the counter
-- product price is always constant
-- having enough money, buying the product makes it vended
-- having enough money, buying the product removes the amount from counter
-- having enough money, buying the product returns the rest of money
-- having vended product, taking it removes it
-- having returned money, taking it removes it
-- TODO what about apps with Cmd ?
-- TODO what about apps with subscriptions ?
-- TODO what about apps with view ? can we at least export a debug file for the 0.18 debugger?
-- Bonus: view function (independent of the model+update)
-- for the Msgs and Model - animated scenario viewer
-- "The beauty of data!! I can use this elsewhere!"
