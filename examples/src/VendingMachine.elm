module VendingMachine exposing (Msg(..), Model)


type Msg
    = AddCoins Int
    | Cancel
    | Buy
    | TakeCoins
    | TakeProduct



-- This model is an extensible record because different implementations are
-- tested by one set of tests that expects an unified API -- (but the
-- implementations can have something extra which the tests don't care about.)
--
-- You should usually model the test types based on your application (ie. use
-- your app's Msg and Model) and in doing so miss this minor inconvenience
-- you see here.


type alias Model a =
    { a
        | currentCoins : Int
        , returnedCoins : Int
        , productPrice : Int
        , isProductVended : Bool
    }
