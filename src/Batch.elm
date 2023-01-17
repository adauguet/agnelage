module Batch exposing
    ( Batch
    , Event
    , make
    , run
    )

import Date exposing (Date)
import Selling exposing (Selling)
import Tupping exposing (Duration(..), Tupping)
import Weaning exposing (Weaning(..))


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
    , tuppingDuration : Duration
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


f : Tupping -> Date
f { date, duration } =
    Date.add Date.Days (Tupping.toInt duration) date


type alias Event =
    { description : String
    , from : Date
    , to : Maybe Date
    }


run : Batch -> List Event
run (Batch { tupping, weaning, selling }) =
    let
        gestation =
            150
    in
    [ { description = "Augmentation des besoins en alimentation"
      , from = Date.add Date.Days -30 tupping.date
      , to = Nothing
      }
    , { description = "Béliers vasomectisés"
      , from = Date.add Date.Days -16 tupping.date
      , to = Nothing
      }
    , { description = "Lutte"
      , from = tupping.date
      , to = Just (f tupping)
      }
    , { description = "Prévoir échographie dans les 10 jours"
      , from = Date.add Date.Days 35 (f tupping)
      , to = Nothing
      }
    , { description = "Augmentation des besoins en alimentation"
      , from = Date.add Date.Days 120 tupping.date
      , to = Nothing
      }
    , let
        lambingDate =
            Date.add Date.Days gestation tupping.date
      in
      { description = "Agnelage"
      , from = lambingDate
      , to = Just (Date.add Date.Days (Tupping.toInt tupping.duration) lambingDate)
      }
    , { description = "Sevrage agneaux allaitement artificiel"
      , from = Date.add Date.Days (gestation + 35) tupping.date
      , to = Nothing
      }
    , { description = "Sevrage"
      , from = Date.add Date.Days (gestation + Weaning.toInt weaning) tupping.date
      , to = Nothing
      }
    , { description = "Début de la période de vente"
      , from = Date.add Date.Days (gestation + Selling.toInt selling) tupping.date
      , to = Nothing
      }
    ]
