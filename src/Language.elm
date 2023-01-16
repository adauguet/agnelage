module Language exposing (fr)

import Date
import Time exposing (Month(..), Weekday(..))


fr : Date.Language
fr =
    { monthName =
        \month ->
            case month of
                Jan ->
                    "janvier"

                Feb ->
                    "février"

                Mar ->
                    "mars"

                Apr ->
                    "avril"

                May ->
                    "mai"

                Jun ->
                    "juin"

                Jul ->
                    "juillet"

                Aug ->
                    "août"

                Sep ->
                    "septembre"

                Oct ->
                    "octobre"

                Nov ->
                    "novembre"

                Dec ->
                    "décembre"
    , monthNameShort =
        \month ->
            case month of
                Jan ->
                    "janv"

                Feb ->
                    "fév"

                Mar ->
                    "mars"

                Apr ->
                    "avr"

                May ->
                    "mai"

                Jun ->
                    "juin"

                Jul ->
                    "juil"

                Aug ->
                    "août"

                Sep ->
                    "sept"

                Oct ->
                    "oct"

                Nov ->
                    "nov"

                Dec ->
                    "déc"
    , weekdayName =
        \weekday ->
            case weekday of
                Mon ->
                    "lundi"

                Tue ->
                    "mardi"

                Wed ->
                    "mercredi"

                Thu ->
                    "jeudi"

                Fri ->
                    "vendredi"

                Sat ->
                    "samedi"

                Sun ->
                    "dimanche"
    , weekdayNameShort =
        \weekday ->
            case weekday of
                Mon ->
                    "lu"

                Tue ->
                    "ma"

                Wed ->
                    "me"

                Thu ->
                    "je"

                Fri ->
                    "ve"

                Sat ->
                    "sa"

                Sun ->
                    "di"
    , dayWithSuffix =
        \day ->
            case day of
                1 ->
                    "1er"

                2 ->
                    "2nd"

                n ->
                    String.fromInt n ++ "ème"
    }
