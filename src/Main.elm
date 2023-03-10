module Main exposing (main)

import Batch exposing (Event)
import Browser
import Calendar
import Date exposing (Date)
import Element exposing (Attribute, Element, inFront)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import Language
import Selling exposing (Selling(..))
import Time exposing (Month(..))
import Tupping exposing (Duration(..))
import Url.Builder
import Weaning exposing (Weaning(..))


type alias Model =
    { name : String
    , count : Int
    , tuppingDuration : Duration
    , tuppingDate : Date
    , weaning : Weaning
    , selling : Selling
    , currentMonth : Month
    , currentYear : Int
    , events : List Event
    , modal : Maybe Modal
    , sliderValue : Float
    }


type Modal
    = TuppingDateModal


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
    , modal = Nothing
    , sliderValue = 3
    }


type Msg
    = DidInputName String
    | DidInputCount String
    | DidChangeTuppingDuration Duration
    | DidInputTuppingDate String
    | DidInputWeaning Weaning
    | DidInputSelling Selling
    | DidChangeTuppingDate Date
    | ClickedPreviousMonth
    | ClickedNextMonth
    | ClickedGenerate
    | OpenModal
    | CloseModal
    | NoOp
    | AdjustValue Float


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
            { model | tuppingDate = date, modal = Nothing }

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

        OpenModal ->
            { model | modal = Just TuppingDateModal }

        CloseModal ->
            { model | modal = Nothing }

        NoOp ->
            model

        AdjustValue value ->
            { model | sliderValue = value }


view : Model -> Html Msg
view model =
    Element.layout [ Element.padding 24, Font.size 16, inFront (modalView model) ] <|
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
                , Element.spacing 24
                , Border.width 1
                , Border.rounded 3
                , Border.color (Element.rgb255 186 189 182)
                ]
                [ Element.column [ Element.spacing 5 ]
                    [ Element.el [ Font.heavy ] <| Element.text "Lutte"
                    , Input.button [ Border.width 1, Element.padding 10, Border.rounded 5 ]
                        { onPress = Just OpenModal
                        , label =
                            Element.row [ Element.spacing 5 ]
                                [ icon "fas fa-calendar-alt"
                                , Element.text <| Date.formatWithLanguage Language.fr "d MMMM yyyy" model.tuppingDate
                                ]
                        }
                    ]
                , select
                    [ Element.width Element.fill
                    , Border.width 1
                    , Border.rounded 5
                    , Element.height (Element.px 30)
                    , Element.clip
                    ]
                    { onChange = DidChangeTuppingDuration
                    , options = [ OneCycle, OneCycleAndAHalf, TwoCycles, ThreeCycles ]
                    , selected = Just model.tuppingDuration
                    , label = Element.text "Nombre de cycles"
                    , toString = Tupping.toString
                    }
                ]
            , select
                [ Element.width Element.fill
                , Border.width 1
                , Border.rounded 5
                , Element.height (Element.px 30)
                , Element.clip
                ]
                { onChange = DidInputWeaning
                , options = [ SixtyDays, SeventyDays, EightyDays, NinetyDays ]
                , selected = Just model.weaning
                , label = Element.text "Sevrage"
                , toString = \weaning -> (Weaning.toInt weaning |> String.fromInt) ++ " jours"
                }
            , select
                [ Element.width Element.fill
                , Border.width 1
                , Border.rounded 5
                , Element.height (Element.px 30)
                , Element.clip
                ]
                { onChange = DidInputSelling
                , options = [ ThreeMonths, FourMonths, FiveMonths, SixMonths, SevenMonths, EightMonths ]
                , selected = Just model.selling
                , label = Element.text "Vente"
                , toString = Selling.sellingToString
                }
            , Input.button []
                { onPress = Just ClickedGenerate
                , label =
                    Element.el
                        [ Border.width 1
                        , Border.rounded 5
                        , Element.paddingXY 20 10
                        ]
                    <|
                        Element.text "Calculer"
                }
            , Element.column [ Element.spacing 24 ] (List.map (eventView model.name) model.events)
            ]


eventView : String -> Event -> Element Msg
eventView name ({ description, from, to } as event) =
    let
        format =
            Date.formatWithLanguage Language.fr "d MMMM YYYY"
    in
    Element.column [ Element.spacing 4 ]
        [ Element.text description
        , Element.el [ Font.size 14, Font.color (Element.rgb255 150 150 150) ] <|
            Element.text <|
                case to of
                    Just to_ ->
                        "du " ++ format from ++ " au " ++ format to_

                    Nothing ->
                        format from
        , Element.newTabLink
            [ Font.size 14
            , Font.color (Element.rgb255 200 100 100)
            , Border.width 1
            , Border.rounded 5
            , Element.paddingXY 5 5
            ]
            { url = makeAddToCalendarLink name event
            , label =
                Element.row [ Element.spacing 4 ]
                    [ icon "fas fa-calendar-plus"
                    , Element.text "Ajouter au calendrier"
                    ]
            }
        ]


icon : String -> Element msg
icon id =
    Element.el [] <| Element.html <| Html.i [ Html.Attributes.class id ] []


makeAddToCalendarLink : String -> Event -> String
makeAddToCalendarLink name { description, from, to } =
    let
        format =
            Date.format "yyyyMMdd"
    in
    Url.Builder.crossOrigin "https://calendar.google.com"
        [ "calendar", "render" ]
        [ Url.Builder.string "action" "TEMPLATE"
        , Url.Builder.string "text" <|
            if String.isEmpty name then
                description

            else
                name ++ " | " ++ description
        , Url.Builder.string "dates" <| format from ++ "/" ++ format (to |> Maybe.withDefault from |> Date.add Date.Days 1)
        ]


modalView : Model -> Element Msg
modalView model =
    case model.modal of
        Just modal ->
            Element.el [ Element.width Element.fill, Element.height Element.fill, Background.color (Element.rgba255 200 200 200 0.8), Events.onClick CloseModal ] <|
                Element.el [ Element.centerX, Element.centerY, Background.color (Element.rgb255 255 255 255), Element.padding 20, Border.rounded 5, onClickStopPropagation NoOp ] <|
                    case modal of
                        TuppingDateModal ->
                            Calendar.calendar
                                { currentMonth = model.currentMonth
                                , currentYear = model.currentYear
                                , currentDate = model.tuppingDate
                                , onSelectDay = DidChangeTuppingDate
                                , onClickPreviousMonth = ClickedPreviousMonth
                                , onClickNextMonth = ClickedNextMonth
                                }

        Nothing ->
            Element.none


onClickStopPropagation : msg -> Attribute msg
onClickStopPropagation msg =
    Element.htmlAttribute <| Html.Events.stopPropagationOn "click" (Json.Decode.succeed ( msg, True ))


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
        , Element.paddingXY 4 0
        ]
    <|
        Element.el [ Element.centerX, Element.centerY ] <|
            Element.text (toString value)
