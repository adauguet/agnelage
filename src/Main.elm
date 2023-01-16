module Main exposing (..)

import Batch exposing (Event, Selling(..), TuppingDuration(..), Weaning(..))
import Browser
import Calendar
import Date exposing (Date)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Language
import Time exposing (Month(..))


type alias Model =
    { name : String
    , count : Int
    , tuppingDuration : TuppingDuration
    , tuppingDate : Date
    , weaning : Weaning
    , selling : Selling
    , currentMonth : Month
    , currentYear : Int
    , events : List Event
    }


init : Model
init =
    { name = ""
    , count = 1
    , tuppingDuration = OneCycle
    , tuppingDate = Date.fromCalendarDate 2023 Jan 15
    , weaning = SixtyDays
    , selling = ThreeMonths
    , currentMonth = Jan
    , currentYear = 2023
    , events = []
    }


type Msg
    = DidInputName String
    | DidInputCount String
    | DidChangeTuppingDuration TuppingDuration
    | DidInputTuppingDate String
    | DidInputWeaning Weaning
    | DidInputSelling Selling
    | DidChangeTuppingDate Date
    | ClickedPreviousMonth
    | ClickedNextMonth
    | ClickedGenerate


update : Msg -> Model -> Model
update msg model =
    case msg of
        DidInputName name ->
            { model | name = name }

        DidInputCount countString ->
            case String.toInt countString of
                Just count ->
                    { model | count = count }

                Nothing ->
                    model

        DidChangeTuppingDuration tuppingDuration ->
            { model | tuppingDuration = tuppingDuration }

        DidInputTuppingDate _ ->
            model

        DidInputWeaning weaning ->
            { model | weaning = weaning }

        DidInputSelling selling ->
            { model | selling = selling }

        DidChangeTuppingDate date ->
            { model | tuppingDate = date }

        ClickedPreviousMonth ->
            let
                previous =
                    Date.fromCalendarDate model.currentYear model.currentMonth 1
                        |> Date.add Date.Months -1
            in
            { model | currentMonth = Date.month previous, currentYear = Date.year previous }

        ClickedNextMonth ->
            let
                next =
                    Date.fromCalendarDate model.currentYear model.currentMonth 1
                        |> Date.add Date.Months 1
            in
            { model | currentMonth = Date.month next, currentYear = Date.year next }

        ClickedGenerate ->
            { model
                | events =
                    Batch.run <|
                        Batch.make
                            { name = model.name
                            , ewesCount = model.count
                            , tuppingDate = model.tuppingDate
                            , tuppingDuration = model.tuppingDuration
                            , weaning = model.weaning
                            , selling = model.selling
                            }
            }


view : Model -> Html Msg
view model =
    Element.layout [ Element.padding 24, Font.size 16 ] <|
        Element.column [ Element.centerX, Element.spacing 32 ]
            [ Element.el [ Font.extraBold, Font.size 22 ] <| Element.text "Agnelage"
            , Input.text []
                { onChange = DidInputName
                , text = model.name
                , placeholder = Nothing
                , label = Input.labelAbove [] <| Element.text "Nom"
                }
            , Input.text [ Font.alignRight ]
                { onChange = DidInputCount
                , text = model.count |> String.fromInt
                , placeholder = Nothing
                , label = Input.labelAbove [] <| Element.text "Nombre de brebis"
                }
            , Element.column
                [ Element.width Element.fill
                , Element.paddingXY 16 16
                , Element.spacing 32
                , Border.width 1
                ]
                [ Element.text "Lutte"
                , Calendar.calendar
                    { currentMonth = model.currentMonth
                    , currentYear = model.currentYear
                    , currentDate = model.tuppingDate
                    , onSelectDay = DidChangeTuppingDate
                    , onClickPreviousMonth = ClickedPreviousMonth
                    , onClickNextMonth = ClickedNextMonth
                    }
                , select
                    [ Element.width Element.fill
                    , Border.width 1
                    , Border.rounded 8
                    , Element.height (Element.px 30)
                    , Element.clip
                    ]
                    { onChange = DidChangeTuppingDuration
                    , options = [ OneCycle, OneCycleAndAHalf, TwoCycles, ThreeCycles ]
                    , selected = Just model.tuppingDuration
                    , label = Element.text "Nombre de cycles"
                    , toString = Batch.tuppingDurationToString
                    }
                ]
            , select
                [ Element.width Element.fill
                , Border.width 1
                , Border.rounded 8
                , Element.height (Element.px 30)
                , Element.clip
                ]
                { onChange = DidInputWeaning
                , options = [ SixtyDays, SeventyDays, EightyDays, NinetyDays ]
                , selected = Just model.weaning
                , label = Element.text "Sevrage"
                , toString = \weaning -> (Batch.weaningToInt weaning |> String.fromInt) ++ " jours"
                }
            , select
                [ Element.width Element.fill
                , Border.width 1
                , Border.rounded 8
                , Element.height (Element.px 30)
                , Element.clip
                ]
                { onChange = DidInputSelling
                , options = [ ThreeMonths, FourMonths, FiveMonths, SixMonths, SevenMonths, EightMonths ]
                , selected = Just model.selling
                , label = Element.text "Vente"
                , toString = Batch.sellingToString
                }
            , Input.button []
                { onPress = Just ClickedGenerate
                , label =
                    Element.el
                        [ Border.width 1
                        , Element.paddingXY 16 8
                        ]
                    <|
                        Element.text "Générer"
                }
            , Element.column [ Element.spacing 16 ] (List.map eventView model.events)
            ]


eventView : Event -> Element msg
eventView { description, date } =
    Element.column [ Element.spacing 4 ]
        [ Element.el [ Font.size 14, Font.color (Element.rgb255 150 150 150) ] <| Element.text <| Date.formatWithLanguage Language.fr "EEEE d MMM YYYY" date
        , Element.text description
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }



-- helpers


select :
    List (Element.Attribute msg)
    ->
        { onChange : a -> msg
        , options : List a
        , selected : Maybe a
        , label : Element msg
        , toString : a -> String
        }
    -> Element msg
select attributes { onChange, options, selected, label, toString } =
    let
        separator =
            Element.el
                [ Element.height Element.fill
                , Element.width (Element.px 1)
                , Background.color (Element.rgb255 0 0 0)
                ]
                Element.none
    in
    Element.column [ Element.width Element.fill, Element.spacing 5 ]
        [ label
        , Element.row attributes <| List.intersperse separator <| List.map (option onChange selected toString) options
        ]


option : (value -> msg) -> Maybe value -> (value -> String) -> value -> Element msg
option onChange selected toString value =
    let
        backgroundColor =
            if selected == Just value then
                Element.rgb255 200 100 100

            else
                Element.rgb255 255 255 255
    in
    Element.el
        [ Events.onClick (onChange value)
        , Background.color backgroundColor
        , Element.width Element.fill
        , Element.height Element.fill
        , Element.pointer
        , Element.paddingXY 12 4
        ]
    <|
        Element.el [ Element.centerX, Element.centerY ] <|
            Element.text (toString value)
