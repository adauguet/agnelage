module Batch exposing
    ( Batch
    , Event
    , Selling(..)
    , TuppingDuration(..)
    , Weaning(..)
    , make
    , run
    , sellingToString
    , tuppingDurationToString
    , weaningToInt
    )

import Date exposing (Date)


type Batch
    = Batch
        { name : String
        , ewesCount : Int
        , tupping : Tupping
        , weaning : Weaning
        , selling : Selling
        }


make :
    { name : String
    , ewesCount : Int
    , tuppingDate : Date
    , tuppingDuration : TuppingDuration
    , weaning : Weaning
    , selling : Selling
    }
    -> Batch
make { name, ewesCount, tuppingDate, tuppingDuration, weaning, selling } =
    Batch
        { name = name
        , ewesCount = ewesCount
        , tupping =
            { date = tuppingDate
            , duration = tuppingDuration
            }
        , weaning = weaning
        , selling = selling
        }


type alias Tupping =
    { date : Date
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


f : Tupping -> Date
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
    Date.add Date.Days days date


type alias Event =
    { description : String
    , date : Date
    }


run : Batch -> List Event
run (Batch { tupping, weaning, selling }) =
    let
        gestation =
            150
    in
    [ { description = "Augmentation des besoins en alimentation"
      , date = Date.add Date.Days -30 tupping.date
      }
    , { description = "Béliers vasomectisés"
      , date = Date.add Date.Days -16 tupping.date
      }
    , { description = "Mise en lutte"
      , date = tupping.date
      }
    , { description = "Retirer les béliers"
      , date = f tupping
      }
    , { description = "Prévoir échographie dans les 10 jours"
      , date = Date.add Date.Days 35 (f tupping)
      }
    , { description = "Augmentation des besoins en alimentation"
      , date = Date.add Date.Days 120 tupping.date
      }
    , { description = "Début agnelage"
      , date = Date.add Date.Days gestation tupping.date
      }
    , { description = "Fin agnelage"
      , date = Date.add Date.Days gestation (f tupping)
      }
    , { description = "Sevrage agneaux allaitement artificiel"
      , date = Date.add Date.Days (gestation + 35) tupping.date
      }
    , { description = "Sevrage"
      , date = Date.add Date.Days (gestation + weaningToInt weaning) tupping.date
      }
    , { description = "Période de vente Début"
      , date = Date.add Date.Days (gestation + sellingToInt selling) tupping.date
      }
    ]
