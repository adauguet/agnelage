module Main exposing (..)

import Batch exposing (Selling(..), TuppingDuration(..), Weaning(..))
import Browser
import Element
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)


type alias Model =
    { name : String
    , count : Int
    , tuppingDuration : TuppingDuration
    , tuppingDate : String
    , weaning : Weaning
    , selling : Selling
    }


init : Model
init =
    { name = ""
    , count = 1
    , tuppingDuration = OneCycle
    , tuppingDate = ""
    , weaning = SixtyDays
    , selling = ThreeMonths
    }


type Msg
    = DidInputName String
    | DidInputCount String
    | DidChangeTuppingDuration TuppingDuration
    | DidInputTuppingDate String
    | DidInputWeaning Weaning
    | DidInputSelling Selling


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


view : Model -> Html Msg
view model =
    Element.layout [ Element.padding 24, Font.size 16 ] <|
        Element.column [ Element.centerX, Element.spacing 32 ]
            [ Input.text []
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
                , Input.text []
                    { onChange = DidInputTuppingDate
                    , text = model.tuppingDate
                    , placeholder = Nothing
                    , label = Input.labelAbove [] <| Element.text "Date"
                    }
                , Input.radioRow [ Element.spacing 24 ]
                    { onChange = DidChangeTuppingDuration
                    , options =
                        [ Input.option OneCycle (Element.text "1")
                        , Input.option OneCycleAndAHalf (Element.text "1,5")
                        , Input.option TwoCycles (Element.text "2")
                        , Input.option ThreeCycles (Element.text "3")
                        ]
                    , selected = Just model.tuppingDuration
                    , label = Input.labelAbove [] <| Element.text "Nombre de cycles"
                    }
                ]
            , Input.radioRow [ Element.spacing 24 ]
                { onChange = DidInputWeaning
                , options =
                    [ Input.option SixtyDays (Element.text "60")
                    , Input.option SeventyDays (Element.text "70")
                    , Input.option EightyDays (Element.text "80")
                    , Input.option NinetyDays (Element.text "90")
                    ]
                , selected = Just model.weaning
                , label = Input.labelAbove [] <| Element.text "Sevrage"
                }
            , Input.radioRow [ Element.spacing 24 ]
                { onChange = DidInputSelling
                , options =
                    [ Input.option ThreeMonths (Element.text "3 mois")
                    , Input.option FourMonths (Element.text "4 mois")
                    , Input.option FiveMonths (Element.text "5 mois")
                    , Input.option SixMonths (Element.text "6 mois")
                    , Input.option SevenMonths (Element.text "7 mois")
                    , Input.option EightMonths (Element.text "8 mois")
                    ]
                , selected = Just model.selling
                , label = Input.labelAbove [] <| Element.text "Vente"
                }
            , Input.button []
                { onPress = Nothing
                , label =
                    Element.el
                        [ Border.width 1
                        , Element.paddingXY 16 8
                        ]
                    <|
                        Element.text "Générer"
                }
            ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
