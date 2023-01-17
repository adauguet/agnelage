module Tupping exposing (Duration(..), Tupping, toInt, toString)

import Date exposing (Date)


type alias Tupping =
    { date : Date
    , duration : Duration
    }


type Duration
    = OneCycle
    | OneCycleAndAHalf
    | TwoCycles
    | ThreeCycles


toInt : Duration -> Int
toInt duration =
    case duration of
        OneCycle ->
            16

        OneCycleAndAHalf ->
            24

        TwoCycles ->
            32

        ThreeCycles ->
            48


toString : Duration -> String
toString duration =
    case duration of
        OneCycle ->
            "1"

        OneCycleAndAHalf ->
            "1.5"

        TwoCycles ->
            "2"

        ThreeCycles ->
            "3"
