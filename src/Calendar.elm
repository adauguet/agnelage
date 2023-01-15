module Calendar exposing (calendar)

import Date exposing (Date)
import Element
    exposing
        ( Element
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , mouseOver
        , none
        , px
        , rgb255
        , row
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Time exposing (Month(..), Weekday(..))


calendar : Date -> Date -> (Date -> msg) -> msg -> msg -> Element msg
calendar currentMonth currentDate onSelectDay onClickPreviousMonth onClickNextMonth =
    let
        firstDayOfTheMonth =
            Date.floor Date.Month currentDate

        lastDayOfTheMonth =
            Date.ceiling Date.Month currentDate |> Date.add Date.Days -1

        dayViews =
            Date.range Date.Day 1 firstDayOfTheMonth lastDayOfTheMonth
                |> List.map (\date -> dayTile currentDate date onSelectDay)

        weeks =
            (List.repeat ((Date.weekdayToNumber <| Date.weekday firstDayOfTheMonth) - 1) emptyTile ++ dayViews ++ List.repeat ((Date.weekdayToNumber <| Date.weekday lastDayOfTheMonth) - 1) emptyTile)
                |> chunks 7
                |> List.map (row [ width fill ])
    in
    column
        [ spacing 20
        ]
        [ monthHeader currentMonth onClickPreviousMonth onClickNextMonth
        , column [ width fill, spacing 5 ]
            [ weekdaysHeader
            , column [ width fill ] weeks
            ]
        ]


monthHeader : Date -> msg -> msg -> Element msg
monthHeader date onClickPreviousMonth onClickNextMonth =
    row
        [ width fill ]
        [ monthHeaderButton "<" onClickPreviousMonth
        , toMonthYear date
            |> text
            |> el [ centerY ]
            |> el [ Font.heavy, height fill, centerX ]
        , monthHeaderButton ">" onClickNextMonth
        ]


fr : Date.Language
fr =
    { monthName =
        \month ->
            case month of
                Jan ->
                    "Janvier"

                Feb ->
                    "Février"

                Mar ->
                    "Mars"

                Apr ->
                    "Avril"

                May ->
                    "Mai"

                Jun ->
                    "Juin"

                Jul ->
                    "Juillet"

                Aug ->
                    "Août"

                Sep ->
                    "Septembre"

                Oct ->
                    "Octobre"

                Nov ->
                    "Novembre"

                Dec ->
                    "Décembre"
    , monthNameShort =
        \month ->
            case month of
                Jan ->
                    "Janvier"

                Feb ->
                    "Février"

                Mar ->
                    "Mars"

                Apr ->
                    "Avril"

                May ->
                    "Mai"

                Jun ->
                    "Juin"

                Jul ->
                    "Juillet"

                Aug ->
                    "Août"

                Sep ->
                    "Septembre"

                Oct ->
                    "Octobre"

                Nov ->
                    "Novembre"

                Dec ->
                    "Décembre"
    , weekdayName =
        \weekday ->
            case weekday of
                Mon ->
                    "Lundi"

                Tue ->
                    "Mardi"

                Wed ->
                    "Mercredi"

                Thu ->
                    "Jeudi"

                Fri ->
                    "Vendredi"

                Sat ->
                    "Samedi"

                Sun ->
                    "Dimanche"
    , weekdayNameShort = weekdayToString
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


toMonthYear : Date -> String
toMonthYear date =
    Date.formatWithLanguage fr "MMMM yyyy" date


monthHeaderButton : String -> msg -> Element msg
monthHeaderButton string msg =
    Input.button
        [ Font.size 24
        , width <| px 32
        , height <| px 32
        , Border.rounded 32
        , mouseOver [ Background.color gray ]
        ]
        { onPress = Just msg
        , label =
            el
                [ centerX
                , centerY
                ]
                (text string)
        }


weekdaysHeader : Element msg
weekdaysHeader =
    row [ width fill ]
        (List.map weekdayTile [ Mon, Tue, Wed, Thu, Fri, Sat, Sun ])


emptyTile : Element msg
emptyTile =
    el [ width fill, height fill ] none


weekdayTile : Weekday -> Element msg
weekdayTile weekday =
    weekday
        |> weekdayToString
        |> text
        |> el [ centerX, centerY ]
        |> el [ width fill, height fill ]


dayTile : Date -> Date -> (Date -> msg) -> Element msg
dayTile currentDay day msg =
    let
        element =
            el
                ([ Border.rounded 40
                 , centerX
                 , centerY
                 , width <| px 37
                 , height <| px 37
                 ]
                    ++ (if Date.compare day currentDay == EQ then
                            [ Background.color blue
                            , Font.color white
                            ]

                        else
                            [ mouseOver
                                [ Background.color gray ]
                            ]
                       )
                )
                (text (toDayString day) |> el [ centerX, centerY ])
    in
    Input.button
        [ width <| px 40, height <| px 40 ]
        { onPress = Just <| msg day, label = element }


toDayString : Date -> String
toDayString =
    Date.day >> String.fromInt


chunks : Int -> List a -> List (List a)
chunks count list =
    case ( List.take count list, List.drop count list ) of
        ( [], _ ) ->
            []

        ( take, drop ) ->
            take :: chunks count drop


blue =
    rgb255 0 0 255


white =
    rgb255 255 255 255


gray =
    rgb255 150 150 150


weekdayToString : Weekday -> String
weekdayToString weekday =
    case weekday of
        Mon ->
            "Lu"

        Tue ->
            "Ma"

        Wed ->
            "Me"

        Thu ->
            "Je"

        Fri ->
            "Ve"

        Sat ->
            "Sa"

        Sun ->
            "Di"
