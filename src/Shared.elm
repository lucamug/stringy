module Shared exposing
    ( AllValues
    , DebouncedValue(..)
    , DebouncedValues
    , Field(..)
    , Function
    , Msg(..)
    , allFunctions
    , arrow
    , color
    , coloredCircle
    , coloredCircleFromResult
    , elmentColorToElmColor
    , emptyDebouncedValues
    , fragmentSeparator
    , generateLocationHref
    , homePageView
    , inputField
    , myLink
    , myParagraph
    , resetValues
    , resetViewport
    , shadow
    , smallScreen
    , space
    )

import Base64
import Browser.Dom
import Color
import Color.Convert
import Debounce
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes
import Html.Events
import Http
import Json.Decode
import Material.Icons.Outlined
import Material.Icons.Types
import Task
import Url


type alias DebouncedValues a =
    { a
        | debounced_query : String
        , debounced_valu1 : String
        , debounced_valu2 : String
        , debounced_valu3 : String
        , debounced_valu4 : String
        , debounced_value : String
    }


type alias AllValues a =
    { a
        | query : String
        , valu1 : String
        , valu2 : String
        , valu3 : String
        , valu4 : String
        , value : String
        , debounced_query : String
        , debounced_valu1 : String
        , debounced_valu2 : String
        , debounced_valu3 : String
        , debounced_valu4 : String
        , debounced_value : String
    }


color :
    { inputFieldBackground : Color
    , inputFieldBorder : Color
    , inputFieldRounded : number
    , link : Color
    , primary : Color
    , primaryAsString : String
    , overedBackground : Color
    , focusedBackground : Color
    }
color =
    { primary = rgb255 156 125 101
    , primaryAsString = "rgb(156,125,101)"
    , link = rgba 0 0.4 0.8 1
    , inputFieldBorder = rgba 0 0 0 0.4
    , inputFieldBackground = rgba255 246 240 237 1
    , inputFieldRounded = 10
    , overedBackground = rgba 0 0 0 0.05
    , focusedBackground = rgba 0 0 0 0.08
    }


emptyDebouncedValues : DebouncedValues {}
emptyDebouncedValues =
    { debounced_query = ""
    , debounced_valu1 = ""
    , debounced_valu2 = ""
    , debounced_valu3 = ""
    , debounced_valu4 = ""
    , debounced_value = ""
    }


homePageView : List ( String, DebouncedValues a ) -> Int -> String -> Element Msg
homePageView listOfLinks innerWidth locationHref =
    let
        subModel : DebouncedValues { locationHref : String }
        subModel =
            { debounced_query = ""
            , debounced_valu1 = ""
            , debounced_valu2 = ""
            , debounced_valu3 = ""
            , debounced_valu4 = ""
            , debounced_value = ""
            , locationHref = locationHref
            }

        paddingSize : Int
        paddingSize =
            if smallScreen innerWidth then
                10

            else
                40

        attrs : List (Attribute msg)
        attrs =
            [ Border.width 1
            , Border.rounded color.inputFieldRounded
            , Border.color color.inputFieldBorder
            , Background.color color.inputFieldBackground
            , shadow
            , alignTop
            , padding 20
            , height fill
            , width (fill |> minimum 200)
            , spacing 20
            ]
    in
    column
        [ paddingEach { top = 20, right = paddingSize, bottom = 40, left = paddingSize }
        , Font.size 16
        , width fill
        , spacing 0
        ]
        ([ wrappedRow [ spacing 30, Font.center ]
            [ column attrs
                [ el [ centerX ] <| html <| Material.Icons.Outlined.airline_stops 48 (Material.Icons.Types.Color <| Color.rgb 0 0 0)
                , paragraph []
                    [ text "This website contains a collection of functions that you can use to "
                    , el [ Font.bold ] <| text "transform strings"
                    , text " into something else."
                    ]
                ]
            , column attrs
                [ el [ centerX ] <| html <| Material.Icons.Outlined.bolt 48 (Material.Icons.Types.Color <| Color.rgb 0 0 0)
                , paragraph []
                    [ text "Edit the argument of functions anywhere in the page and see the result updated in "
                    , el [ Font.bold ] <| text "real-time"
                    , text "."
                    ]
                ]
            , column attrs
                [ el [ centerX ] <| html <| Material.Icons.Outlined.share 48 (Material.Icons.Types.Color <| Color.rgb 0 0 0)
                , paragraph []
                    [ el [ Font.bold ] <| text "Share the result"
                    , text " simply sharing the URL, it contains all the necessary data."
                    ]
                ]
            ]
         , el
            [ paddingEach { top = 30, right = 0, bottom = 30, left = 0 }
            , centerX
            , width fill
            ]
           <|
            image
                [ Border.width 1
                , width fill
                , Border.rounded color.inputFieldRounded
                , shadow
                , clip
                ]
                { description = "", src = "/images/animation.gif" }
         , myLink
            [ Border.rounded 50
            , shadow
            , Background.color <| color.primary
            , Font.color <| rgb 1 1 1
            , Font.size 20
            , centerX
            , paddingEach { top = 15, right = 25, bottom = 15, left = 25 }
            ]
            { label = paragraph [] [ text "Try it now!" ]
            , url = generateLocationHref { subModel | debounced_query = "" }
            }
         , paragraph [ Font.size 20, Font.center, paddingEach { top = 40, right = 0, bottom = 40, left = 15 } ] [ text "Examples of things that you can do here" ]
         ]
            ++ List.map
                (\( string, m2 ) ->
                    homePageRow string
                        { debounced_query = m2.debounced_query
                        , debounced_valu1 = m2.debounced_valu1
                        , debounced_valu2 = m2.debounced_valu2
                        , debounced_valu3 = m2.debounced_valu3
                        , debounced_valu4 = m2.debounced_valu4
                        , debounced_value = m2.debounced_value
                        , locationHref = locationHref
                        }
                )
                listOfLinks
            ++ [ paragraph [ Font.center, paddingEach { top = 20, right = 0, bottom = 0, left = 0 } ]
                    [ text "Any missing transformation? "
                    , newTabLink [ Font.color color.link ] { url = "https://github.com/lucamug/stringy/issues/new", label = text "Open an issue" }
                    , text "."
                    ]
               ]
            ++ [ paragraph
                    [ paddingEach { top = 50, right = 0, bottom = 0, left = 0 }
                    , Font.size 13
                    , Font.color <| rgba 0 0 0 0.5
                    ]
                    [ text "For a more mature and comprehensive tool, have a look at "
                    , newTabLink [ Font.color color.link ] { label = text "CyberChef", url = "https://gchq.github.io/CyberChef/" }
                    , text " developed by the UK's intelligence, security and cyber agency."
                    ]
               ]
            ++ [ paragraph
                    [ paddingEach { top = 20, right = 0, bottom = 0, left = 0 }
                    , Font.size 13
                    , Font.color <| rgba 0 0 0 0.5
                    ]
                    [ text " \"QR Code\" is a registered trademark of DENSO WAVE INCORPORATED" ]
               ]
        )


shadow : Attr decorative msg
shadow =
    Border.shadow { offset = ( 5, 5 ), size = 0, blur = 0, color = rgba 0 0 0 0.15 }


smallScreen : Int -> Bool
smallScreen innerWidth =
    innerWidth < 640


elmentColorToElmColor : Color -> Color.Color
elmentColorToElmColor color_ =
    let
        rgba : { alpha : Float, blue : Float, green : Float, red : Float }
        rgba =
            toRgb color_
    in
    Color.rgba rgba.red rgba.green rgba.blue rgba.alpha


fragmentSeparator : String
fragmentSeparator =
    "/"


allFunctions : String
allFunctions =
    "/all_functions"


generateLocationHref : DebouncedValues { a | locationHref : String } -> String
generateLocationHref args =
    args.locationHref
        |> Url.fromString
        |> Maybe.map
            (\url ->
                if args.debounced_query ++ args.debounced_valu1 ++ args.debounced_valu2 ++ args.debounced_valu3 ++ args.debounced_valu4 ++ args.debounced_value == "" then
                    { url | path = allFunctions }

                else
                    let
                        path : String
                        path =
                            "/"
                                ++ String.join fragmentSeparator
                                    [ String.replace "=" "" (Base64.encode args.debounced_value)
                                    , String.replace "=" "" (Base64.encode args.debounced_query)
                                    , String.replace "=" "" (Base64.encode args.debounced_valu1)
                                    , String.replace "=" "" (Base64.encode args.debounced_valu2)
                                    , String.replace "=" "" (Base64.encode args.debounced_valu3)
                                    , String.replace "=" "" (Base64.encode args.debounced_valu4)
                                    ]
                                |> removeTrailingEmptyValues
                                |> removeTrailingEmptyValues
                                |> removeTrailingEmptyValues
                                |> removeTrailingEmptyValues
                                |> removeTrailingEmptyValues
                    in
                    { url | path = path }
            )
        |> Maybe.map Url.toString
        |> Maybe.withDefault ""


removeTrailingEmptyValues : String -> String
removeTrailingEmptyValues string =
    if String.endsWith "/" string then
        String.dropRight 1 string

    else
        string


resetValues : AllValues a -> AllValues a
resetValues model =
    { model
        | value = ""
        , valu1 = ""
        , valu2 = ""
        , valu3 = ""
        , valu4 = ""
        , query = ""
        , debounced_value = ""
        , debounced_valu1 = ""
        , debounced_valu2 = ""
        , debounced_valu3 = ""
        , debounced_valu4 = ""
        , debounced_query = ""
    }


type Msg
    = MsgInput DebouncedValue String
    | MsgDebounced DebouncedValue String
    | MsgDebounceMsg DebouncedValue Debounce.Msg
    | MsgPushUrl { url : String, sendItBack : Bool }
    | MsgUrlChanged String
    | MsgGotElmJson (Result Http.Error String)
    | MsgNoOp


resetViewport : Cmd Msg
resetViewport =
    Task.perform (\_ -> MsgNoOp) (Browser.Dom.setViewport 0 0)


type DebouncedValue
    = Value
    | Valu1
    | Valu2
    | Valu3
    | Valu4
    | Query


myLink :
    List (Attribute Msg)
    -> { label : Element Msg, url : String }
    -> Element Msg
myLink attrs args =
    let
        preventDefault : msg -> Html.Attribute msg
        preventDefault href =
            Html.Events.preventDefaultOn "click" (Json.Decode.succeed ( href, True ))
    in
    link ([ htmlAttribute <| preventDefault <| MsgPushUrl { url = args.url, sendItBack = True } ] ++ attrs) args


myParagraph : List (Attribute msg) -> List (Element msg) -> Element msg
myParagraph attrs =
    paragraph ([ htmlAttribute <| Html.Attributes.style "word-break" "break-word" ] ++ attrs)


type alias Function =
    { prefix : String
    , function : String
    , signature : String
    , description : String
    , executionBefore : List (Element Msg)
    , executionAfter : List (Element Msg)
    , outcome : List (Element Msg)
    , package : String
    }


arrow : Element msg
arrow =
    el
        [ Font.size 30
        , paddingEach { top = 0, right = 5, bottom = 0, left = 5 }
        ]
        (text "➔")


coloredCircle : Color.Color -> Element msg
coloredCircle color_ =
    el
        [ height <| px 30
        , width <| px 30
        , moveUp 7
        , Border.rounded 40
        , Border.width 1
        , Border.color <| rgb 0.5 0.5 0.5
        , htmlAttribute <|
            Html.Attributes.style "background-color"
                (Color.Convert.colorToCssRgba color_)
        ]
        (text "\u{3000}")


coloredCircleFromResult : Result String String -> Element msg
coloredCircleFromResult resultColor =
    case resultColor of
        Ok color_ ->
            el
                [ height <| px 30
                , width <| px 30
                , moveUp 7
                , Border.rounded 40
                , Border.width 1
                , Border.color <| rgb 0.5 0.5 0.5
                , htmlAttribute <| Html.Attributes.style "background-color" color_
                ]
                (text "\u{3000}")

        Err err ->
            column
                [ height <| px 30
                , width <| px 30
                , moveDown 4
                , Border.rounded 40
                , Border.width 1
                , Border.color <| rgb 0.8 0 0
                ]
                [ el
                    [ Background.color <| rgb 0.8 0 0
                    , width <| px 2
                    , height <| px 30
                    , htmlAttribute <| Html.Attributes.style "transform" "rotate(45deg)"
                    , htmlAttribute <| Html.Attributes.style "position" "absolute"
                    , htmlAttribute <| Html.Attributes.style "left" "13px"
                    , htmlAttribute <| Html.Attributes.style "top" "-1px"
                    ]
                    none
                , el
                    [ Background.color <| rgb 0.8 0 0
                    , width <| px 2
                    , height <| px 30
                    , htmlAttribute <| Html.Attributes.style "transform" "rotate(-45deg)"
                    , htmlAttribute <| Html.Attributes.style "position" "absolute"
                    , htmlAttribute <| Html.Attributes.style "left" "13px"
                    , htmlAttribute <| Html.Attributes.style "top" "-1px"
                    ]
                    none
                ]


type Field
    = FieldString
    | FieldInt
    | FieldFloat
    | FieldChar


inputField :
    { a
        | query : String
        , valu1 : String
        , valu2 : String
        , valu3 : String
        , valu4 : String
        , value : String
    }
    -> DebouncedValue
    -> Field
    -> List (Element Msg)
inputField model debouncedValue fieldType =
    let
        v : String
        v =
            case debouncedValue of
                Value ->
                    model.value

                Valu1 ->
                    model.valu1

                Valu2 ->
                    model.valu2

                Valu3 ->
                    model.valu3

                Valu4 ->
                    model.valu4

                Query ->
                    model.query

        field : Element Msg
        field =
            Input.text
                ([ paddingEach { top = 8, right = 6, bottom = 8, left = 6 }
                 , htmlAttribute <| Html.Attributes.style "width" (String.fromInt (String.length v) ++ "ch")
                 , Border.width 0
                 , Border.rounded color.inputFieldRounded
                 , Border.color color.inputFieldBorder
                 , Background.color color.inputFieldBackground
                 , htmlAttribute <| Html.Attributes.tabindex 10
                 ]
                    ++ (let
                            errorAttrs : List (Attribute msg)
                            errorAttrs =
                                [ Font.size 11
                                , Font.color <| rgba 0.8 0 0 0.8
                                , moveDown 2
                                , centerX
                                , width <| px 75
                                , Font.center
                                ]
                        in
                        case fieldType of
                            FieldChar ->
                                if String.length v == 1 then
                                    []

                                else
                                    [ below <| el errorAttrs <| text <| "One char only" ]

                            FieldString ->
                                []

                            FieldInt ->
                                case String.toInt v of
                                    Just _ ->
                                        []

                                    Nothing ->
                                        [ below <| el errorAttrs <| text <| "Not an Int" ]

                            FieldFloat ->
                                case String.toFloat v of
                                    Just _ ->
                                        []

                                    Nothing ->
                                        [ below <| el errorAttrs <| text <| "Not a Float" ]
                       )
                )
                { label = Input.labelHidden ""
                , onChange = MsgInput debouncedValue
                , placeholder = Nothing
                , text = v
                }
    in
    case fieldType of
        FieldString ->
            [ text "\""
            , field
            , text "\""
            ]

        FieldChar ->
            [ text "'"
            , field
            , text "'"
            ]

        FieldInt ->
            [ field ]

        FieldFloat ->
            [ field ]


homePageRow : String -> DebouncedValues { locationHref : String } -> Element Msg
homePageRow string model =
    myLink
        [ width fill
        , mouseOver [ Background.color color.overedBackground ]
        , focused [ Background.color color.focusedBackground ]
        , Border.rounded 10
        ]
        { label =
            row []
                [ el [ alignTop, padding 12 ] <| text "⚡"
                , paragraph [ padding 12, Font.color color.link ] [ text string ]
                ]
        , url = generateLocationHref model
        }


space : List (Element msg)
space =
    [ text " " ]
