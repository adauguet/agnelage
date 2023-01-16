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
import Language
import Time exposing (Month(..), Weekday(..))


calendar :
    { currentMonth : Month
    , currentYear : Int
    , currentDate : Date
    , onSelectDay : Date -> msg
    , onClickPreviousMonth : msg
    , onClickNextMonth : msg
    }
    -> Element msg
calendar { currentMonth, currentYear, currentDate, onSelectDay, onClickPreviousMonth, onClickNextMonth } =
    let
        firstDayOfTheMonth =
            Date.fromCalendarDate currentYear currentMonth 1

        firstDayOfNextMonth =
            Date.add Date.Months 1 firstDayOfTheMonth

        dayViews =
            Date.range Date.Day 1 firstDayOfTheMonth firstDayOfNextMonth
                |> List.map (\date -> dayTile currentDate date onSelectDay)

        weeks =
            (List.repeat ((Date.weekdayToNumber <| Date.weekday firstDayOfTheMonth) - 1) emptyTile ++ dayViews ++ List.repeat ((Date.weekdayToNumber <| Date.weekday firstDayOfNextMonth) - 1) emptyTile)
                |> chunks 7
                |> List.map (row [ width fill ])
    in
    column
        [ spacing 20
        ]
        [ monthHeader currentMonth currentYear onClickPreviousMonth onClickNextMonth
        , column [ width fill, spacing 5 ]
            [ weekdaysHeader
            , column [ width fill ] weeks
            ]
        ]


monthHeader : Month -> Int -> msg -> msg -> Element msg
monthHeader month year onClickPreviousMonth onClickNextMonth =
    row
        [ width fill ]
        [ monthHeaderButton "<" onClickPreviousMonth
        , (Language.fr.monthName month ++ " " ++ String.fromInt year)
            |> text
            |> el [ centerY ]
            |> el [ Font.heavy, height fill, centerX ]
        , monthHeaderButton ">" onClickNextMonth
        ]


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
        , label = el [ centerX, centerY ] <| text string
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
        |> Language.fr.weekdayNameShort
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


blue : Element.Color
blue =
    rgb255 0 0 255


white : Element.Color
white =
    rgb255 255 255 255


gray : Element.Color
gray =
    rgb255 150 150 150
