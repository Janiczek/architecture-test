module Expect.Extra exposing (custom, equalWithDesc)

import Expect exposing (Expectation)


custom : (a -> b -> Result String ()) -> a -> b -> Expectation
custom f expected actual =
    case f expected actual of
        Ok () ->
            Expect.pass

        Err str ->
            [ toString actual
            , "╷"
            , "│ " ++ str
            , "╵"
            , toString expected
            ]
                |> String.join "\n"
                |> Expect.fail


equalWithDesc : String -> comparable -> comparable -> Expectation
equalWithDesc description =
    custom <|
        \expected actual ->
            if expected == actual then
                Ok ()
            else
                Err ("Expect.equal (" ++ description ++ ")")
