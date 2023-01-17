module Selling exposing (Selling(..), sellingToString, toInt)


type Selling
    = ThreeMonths
    | FourMonths
    | FiveMonths
    | SixMonths
    | SevenMonths
    | EightMonths


toInt : Selling -> Int
toInt selling =
    case selling of
        ThreeMonths ->
            90

        FourMonths ->
            120

        FiveMonths ->
            150

        SixMonths ->
            180

        SevenMonths ->
            210

        EightMonths ->
            240


sellingToString : Selling -> String
sellingToString selling =
    case selling of
        ThreeMonths ->
            "3 mois"

        FourMonths ->
            "4 mois"

        FiveMonths ->
            "5 mois"

        SixMonths ->
            "6 mois"

        SevenMonths ->
            "7 mois"

        EightMonths ->
            "8 mois"
