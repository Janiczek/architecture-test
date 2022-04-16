module Test.Runner.Failure.Extra exposing (format)

{-| Test.Runner.Failure.format was removed in some version of elm-test, so we're
keeping it here.
-}

import Test.Runner.Failure exposing (..)


verticalBar : String -> String -> String -> String
verticalBar comparison expected actual =
    [ actual
    , "╵"
    , "│ " ++ comparison
    , "╷"
    , expected
    ]
        |> String.join "\n"


{-| Taken from an older version of elm-explorations/test.
-}
format : String -> Reason -> String
format description reason =
    case reason of
        Custom ->
            description

        Equality e a ->
            verticalBar description e a

        Comparison e a ->
            verticalBar description e a

        TODO ->
            description

        Invalid BadDescription ->
            if description == "" then
                "The empty string is not a valid test description."

            else
                "This is an invalid test description: " ++ description

        Invalid _ ->
            description

        ListDiff expected actual ->
            listDiffToString 0
                description
                { expected = expected
                , actual = actual
                }
                { originalExpected = expected
                , originalActual = actual
                }

        CollectionDiff { expected, actual, extra, missing } ->
            let
                extraStr =
                    if List.isEmpty extra then
                        ""

                    else
                        "\nThese keys are extra: "
                            ++ (extra |> String.join ", " |> (\d -> "[ " ++ d ++ " ]"))

                missingStr =
                    if List.isEmpty missing then
                        ""

                    else
                        "\nThese keys are missing: "
                            ++ (missing |> String.join ", " |> (\d -> "[ " ++ d ++ " ]"))
            in
            String.join ""
                [ verticalBar description expected actual
                , "\n"
                , extraStr
                , missingStr
                ]


toStringLists : List String -> String
toStringLists =
    String.join ", "


listDiffToString :
    Int
    -> String
    -> { expected : List String, actual : List String }
    -> { originalExpected : List String, originalActual : List String }
    -> String
listDiffToString index description { expected, actual } originals =
    case ( expected, actual ) of
        ( [], [] ) ->
            [ "Two lists were unequal previously, yet ended up equal later."
            , "This should never happen!"
            , "Please report this bug to https://github.com/elm-community/elm-test/issues - and include these lists: "
            , "\n"
            , toStringLists originals.originalExpected
            , "\n"
            , toStringLists originals.originalActual
            ]
                |> String.join ""

        ( first :: _, [] ) ->
            verticalBar (description ++ " was shorter than")
                (toStringLists originals.originalExpected)
                (toStringLists originals.originalActual)

        ( [], first :: _ ) ->
            verticalBar (description ++ " was longer than")
                (toStringLists originals.originalExpected)
                (toStringLists originals.originalActual)

        ( firstExpected :: restExpected, firstActual :: restActual ) ->
            if firstExpected == firstActual then
                -- They're still the same so far; keep going.
                listDiffToString (index + 1)
                    description
                    { expected = restExpected
                    , actual = restActual
                    }
                    originals

            else
                -- We found elements that differ; fail!
                String.join ""
                    [ verticalBar description
                        (toStringLists originals.originalExpected)
                        (toStringLists originals.originalActual)
                    , "\n\nThe first diff is at index "
                    , String.fromInt index
                    , ": it was `"
                    , firstActual
                    , "`, but `"
                    , firstExpected
                    , "` was expected."
                    ]
