module Batch exposing
    ( Batch
    , Event
    , Selling(..)
    , TuppingDuration(..)
    , Weaning(..)
    , run
    , sellingToString
    , tuppingDurationToString
    , weaningToInt
    )

import Time exposing (Posix)


type alias Batch =
    { name : String
    , ewesCount : Int
    , tupping : Tupping
    , weaning : Weaning
    , selling : Selling
    }


type alias Tupping =
    { date : Posix
    , duration : TuppingDuration
    }


type TuppingDuration
    = OneCycle
    | OneCycleAndAHalf
    | TwoCycles
    | ThreeCycles


tuppingDurationToString : TuppingDuration -> String
tuppingDurationToString duration =
    case duration of
        OneCycle ->
            "1"

        OneCycleAndAHalf ->
            "1.5"

        TwoCycles ->
            "2"

        ThreeCycles ->
            "3"


type Weaning
    = SixtyDays
    | SeventyDays
    | EightyDays
    | NinetyDays


weaningToInt : Weaning -> Int
weaningToInt weaning =
    case weaning of
        SixtyDays ->
            60

        SeventyDays ->
            70

        EightyDays ->
            80

        NinetyDays ->
            90


type Selling
    = ThreeMonths
    | FourMonths
    | FiveMonths
    | SixMonths
    | SevenMonths
    | EightMonths


sellingToInt : Selling -> Int
sellingToInt selling =
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


f : Tupping -> Posix
f { date, duration } =
    let
        days =
            case duration of
                OneCycle ->
                    16

                OneCycleAndAHalf ->
                    24

                TwoCycles ->
                    32

                ThreeCycles ->
                    48
    in
    Time.posixToMillis date + daysToMillis days |> Time.millisToPosix


daysToMillis : Int -> Int
daysToMillis n =
    n * 24 * 3600 * 1000


type alias Event =
    { description : String
    , date : Posix
    }


run : Batch -> List Event
run { tupping, weaning, selling } =
    [ { description = "Augmentation des besoins en alimentation"
      , date = Time.posixToMillis tupping.date - daysToMillis 30 |> Time.millisToPosix
      }
    , { description = "Béliers vasomectisés"
      , date = Time.posixToMillis tupping.date - daysToMillis 16 |> Time.millisToPosix
      }
    , { description = "Mise en lutte"
      , date = tupping.date
      }
    , { description = "Retirer les béliers"
      , date = f tupping
      }
    , { description = "Prévoir échographie dans les 10 jours"
      , date = Time.posixToMillis (f tupping) + daysToMillis 35 |> Time.millisToPosix
      }
    , { description = "Augmentation des besoins en alimentation"
      , date = Time.posixToMillis tupping.date + daysToMillis 120 |> Time.millisToPosix
      }
    , { description = "Début agnelage"
      , date = Time.posixToMillis tupping.date + daysToMillis 180 |> Time.millisToPosix
      }
    , { description = "Fin agnelage"
      , date = Time.posixToMillis (f tupping) + daysToMillis 180 |> Time.millisToPosix
      }
    , { description = "Sevrage agneaux allaitement artificiel"
      , date = Time.posixToMillis tupping.date + daysToMillis (180 + 35) |> Time.millisToPosix
      }
    , { description = "Sevrage"
      , date = Time.posixToMillis tupping.date + daysToMillis (180 + weaningToInt weaning) |> Time.millisToPosix
      }
    , { description = "Période de vente Début"
      , date = Time.posixToMillis tupping.date + daysToMillis (180 + sellingToInt selling) |> Time.millisToPosix
      }
    ]
