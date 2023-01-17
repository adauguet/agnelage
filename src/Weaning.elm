module Weaning exposing (Weaning(..), toInt)


type Weaning
    = SixtyDays
    | SeventyDays
    | EightyDays
    | NinetyDays


toInt : Weaning -> Int
toInt weaning =
    case weaning of
        SixtyDays ->
            60

        SeventyDays ->
            70

        EightyDays ->
            80

        NinetyDays ->
            90
