port module Main exposing (Env, Flags, Model, main)

import Base64
import Browser
import Color
import Color.Convert
import Debounce
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html
import Html.Attributes
import Http
import Json.Decode
import Material.Icons.Outlined
import Material.Icons.Types
import PageFunctions
import PageHome
import Shared
import Svg
import Svg.Attributes
import Task
import Url


title : String
title =
    "Stringy"


port pushUrl : { url : String, sendItBack : Bool } -> Cmd msg


port onUrlChange : (String -> msg) -> Sub msg


port changeMeta : { querySelector : String, fieldName : String, content : String } -> Cmd msg


debounceConfig : Shared.DebouncedValue -> Debounce.Config Shared.Msg
debounceConfig debouncedValue =
    { strategy = Debounce.later 300
    , transform = Shared.MsgDebounceMsg debouncedValue
    }


type alias Env =
    { serviceName : String
    , tenantName : String
    , branchName : String
    , commitHash : String
    }


type alias Model =
    { value : String
    , valu1 : String
    , valu2 : String
    , valu3 : String
    , valu4 : String
    , query : String
    , debounced_value : String
    , debounced_valu1 : String
    , debounced_valu2 : String
    , debounced_valu3 : String
    , debounced_valu4 : String
    , debounced_query : String
    , debounce : Debounce.Debounce String
    , locationHref : String
    , innerWidth : Int
    , dependencies : Dict.Dict String String
    , env : Env
    }


type alias Flags =
    { env : Env
    , locationHref : String
    , innerWidth : Int
    }


init : Flags -> ( Model, Cmd Shared.Msg )
init flags =
    { value = ""
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
    , debounce = Debounce.init
    , locationHref = flags.locationHref
    , innerWidth = flags.innerWidth
    , dependencies = Dict.empty
    , env = flags.env
    }
        |> urlChanged
        |> (\( model, cmd ) ->
                ( model
                , Cmd.batch
                    [ cmd
                    , Http.get
                        { url = "/elm.json"
                        , expect = Http.expectString Shared.MsgGotElmJson
                        }
                    ]
                )
           )


update : Shared.Msg -> Model -> ( Model, Cmd Shared.Msg )
update msg model =
    case msg of
        Shared.MsgGotElmJson (Ok string) ->
            let
                elmJsonDecoder : Json.Decode.Decoder (Dict.Dict String String)
                elmJsonDecoder =
                    Json.Decode.string
                        |> Json.Decode.dict
                        |> Json.Decode.field "direct"
                        |> Json.Decode.field "dependencies"
            in
            ( { model
                | dependencies =
                    case Json.Decode.decodeString elmJsonDecoder string of
                        Ok dependencies ->
                            dependencies

                        Err _ ->
                            Dict.empty
              }
            , Cmd.none
            )

        Shared.MsgGotElmJson (Err _) ->
            ( model, Cmd.none )

        Shared.MsgPushUrl args ->
            ( model, pushUrl args )

        Shared.MsgUrlChanged newLocationHref ->
            let
                oldLocationHref : String
                oldLocationHref =
                    model.locationHref

                ( newModel, cmd ) =
                    urlChanged { model | locationHref = newLocationHref }

                cmdScrollToTop : Cmd Shared.Msg
                cmdScrollToTop =
                    if locationHrefToPage oldLocationHref /= locationHrefToPage newLocationHref then
                        Shared.resetViewport

                    else
                        Cmd.none
            in
            ( newModel, Cmd.batch [ cmd, cmdScrollToTop ] )

        Shared.MsgInput debouncedValue s ->
            let
                ( debounce, cmd ) =
                    Debounce.push (debounceConfig debouncedValue) s model.debounce
            in
            ( { model | debounce = debounce }
                |> (\m ->
                        case debouncedValue of
                            Shared.Value ->
                                { m | value = s }

                            Shared.Valu1 ->
                                { m | valu1 = s }

                            Shared.Valu2 ->
                                { m | valu2 = s }

                            Shared.Valu3 ->
                                { m | valu3 = s }

                            Shared.Valu4 ->
                                { m | valu4 = s }

                            Shared.Query ->
                                { m | query = s }
                   )
            , cmd
            )

        Shared.MsgDebounceMsg debouncedValue msg_ ->
            let
                save : String -> Cmd Shared.Msg
                save s =
                    Task.perform (Shared.MsgDebounced debouncedValue) (Task.succeed s)

                ( debounce, cmd ) =
                    Debounce.update
                        (debounceConfig Shared.Value)
                        (Debounce.takeLast save)
                        msg_
                        model.debounce
            in
            ( { model | debounce = debounce }, cmd )

        Shared.MsgDebounced debouncedValue s ->
            let
                newModel : Model
                newModel =
                    case debouncedValue of
                        Shared.Query ->
                            { model | debounced_query = s }

                        Shared.Value ->
                            { model | debounced_value = s }

                        Shared.Valu1 ->
                            { model | debounced_valu1 = s }

                        Shared.Valu2 ->
                            { model | debounced_valu2 = s }

                        Shared.Valu3 ->
                            { model | debounced_valu3 = s }

                        Shared.Valu4 ->
                            { model | debounced_valu4 = s }

                newLocationHref : String
                newLocationHref =
                    Shared.generateLocationHref newModel
            in
            ( { newModel | locationHref = newLocationHref }
            , Cmd.batch
                [ updateMetaTitle newModel
                , pushUrl
                    -- In this case we modify the model directly wihtout getting
                    -- the URL back from JavaScript because we want to be fast,
                    -- as the user is typing.
                    { sendItBack = False
                    , url = newLocationHref
                    }
                ]
            )

        Shared.MsgNoOp ->
            ( model, Cmd.none )


urlChanged : Model -> ( Model, Cmd Shared.Msg )
urlChanged model =
    case Url.fromString model.locationHref of
        Just url ->
            if url.path == "/" then
                ( Shared.resetValues model
                , changeMeta { content = title, fieldName = "innerHTML", querySelector = "title" }
                )

            else
                newModelFromLocationHref
                    (if String.startsWith Shared.allFunctions url.path then
                        String.replace Shared.allFunctions "" url.path

                     else
                        String.dropLeft 1 url.path
                    )
                    model

        Nothing ->
            ( model, Cmd.none )


newModelFromLocationHref : String -> Model -> ( Model, Cmd Shared.Msg )
newModelFromLocationHref path model =
    let
        newModel : Model
        newModel =
            path
                |> fragmentDecoder
                |> (\maybeDecodedFragment ->
                        case maybeDecodedFragment of
                            Just decodedFragment ->
                                { model
                                    | debounced_value = decodedFragment.debounced_value
                                    , debounced_valu1 = decodedFragment.debounced_valu1
                                    , debounced_valu2 = decodedFragment.debounced_valu2
                                    , debounced_valu3 = decodedFragment.debounced_valu3
                                    , debounced_valu4 = decodedFragment.debounced_valu4
                                    , debounced_query = decodedFragment.debounced_query
                                    , value = decodedFragment.debounced_value
                                    , valu1 = decodedFragment.debounced_valu1
                                    , valu2 = decodedFragment.debounced_valu2
                                    , valu3 = decodedFragment.debounced_valu3
                                    , valu4 = decodedFragment.debounced_valu4
                                    , query = decodedFragment.debounced_query
                                }

                            Nothing ->
                                model
                   )
    in
    ( newModel
    , updateMetaTitle newModel
    )


updateMetaTitle : Shared.DebouncedValues { a | locationHref : String } -> Cmd msg
updateMetaTitle model =
    changeMeta
        { content = dynamicTitle model
        , fieldName = "innerHTML"
        , querySelector = "title"
        }


dynamicTitle : Shared.DebouncedValues { a | locationHref : String } -> String
dynamicTitle model =
    title
        :: (case locationHrefToPage model.locationHref of
                PageHome ->
                    []

                PageSitemap ->
                    [ "Sitemap" ]

                PageFunctions ->
                    [ model.debounced_query
                    , model.debounced_value
                    , model.debounced_valu1
                    , model.debounced_valu2
                    , model.debounced_valu3
                    , model.debounced_valu4
                    ]
                        |> List.filter (not << String.isEmpty)
                        |> (\list ->
                                if List.isEmpty list then
                                    [ "All Functions" ]

                                else
                                    list
                           )
           )
        |> String.join " - "


fragmentDecoder :
    String
    ->
        Maybe
            { debounced_query : String
            , debounced_valu1 : String
            , debounced_valu2 : String
            , debounced_valu3 : String
            , debounced_valu4 : String
            , debounced_value : String
            }
fragmentDecoder fragment =
    fragment
        |> (\f -> f ++ String.repeat 5 Shared.fragmentSeparator)
        |> String.split Shared.fragmentSeparator
        |> List.map Base64.decode
        |> (\list ->
                case list of
                    (Ok debounced_value) :: (Ok debounced_query) :: (Ok debounced_valu1) :: (Ok debounced_valu2) :: (Ok debounced_valu3) :: (Ok debounced_valu4) :: _ ->
                        Just
                            { debounced_query = debounced_query
                            , debounced_value = debounced_value
                            , debounced_valu1 = debounced_valu1
                            , debounced_valu2 = debounced_valu2
                            , debounced_valu3 = debounced_valu3
                            , debounced_valu4 = debounced_valu4
                            }

                    _ ->
                        Nothing
           )


main : Program Flags Model Shared.Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> onUrlChange Shared.MsgUrlChanged
        }


header : List (Attribute Shared.Msg) -> Int -> Model -> Element Shared.Msg
header attrs tabindex model =
    let
        paddingLeftRight : Int
        paddingLeftRight =
            if Shared.smallScreen model.innerWidth then
                10

            else
                40
    in
    column
        (attrs
            ++ [ width fill
               , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
               , borderColor
               ]
        )
        [ wrappedRow [ spacing 20, width fill, padding 0 ]
            ((if Shared.smallScreen model.innerWidth then
                []

              else
                [ Shared.myLink
                    [ htmlAttribute <| Html.Attributes.tabindex -1 ]
                    { url = "/"
                    , label =
                        image [ width <| px 200, centerX ]
                            { description = "String image"
                            , src = "images/string_small.png"
                            }
                    }
                ]
             )
                ++ [ column [ spacing 20, width fill, paddingEach { top = 0, right = paddingLeftRight, bottom = 0, left = paddingLeftRight } ]
                        [ wrappedRow
                            [ centerX
                            , Font.color Shared.color.primary
                            , Font.center
                            , Font.family [ Font.typeface "Amatic SC" ]

                            -- , explain Debug.todo
                            -- , width (fill |> minimum 210)
                            ]
                            [ Shared.myParagraph
                                [ Font.size
                                    (if Shared.smallScreen model.innerWidth then
                                        40

                                     else
                                        60
                                    )
                                , Region.heading 1
                                ]
                                [ Shared.myLink [ htmlAttribute <| Html.Attributes.tabindex -1 ] { label = text title, url = "/" } ]
                            , Shared.myParagraph
                                [ Font.alignLeft
                                , spacing 5
                                , Font.size
                                    (if Shared.smallScreen model.innerWidth then
                                        18

                                     else
                                        24
                                    )
                                , alignBottom
                                ]
                                [ text "the string transformer" ]
                            ]
                        , Input.multiline
                            [ width fill
                            , Border.width 1
                            , Border.rounded Shared.color.inputFieldRounded
                            , Border.color Shared.color.inputFieldBorder
                            , Background.color Shared.color.inputFieldBackground
                            , htmlAttribute <| Html.Attributes.tabindex tabindex
                            , Shared.shadow
                            , inFront <|
                                Shared.myLink
                                    [ alignRight
                                    , centerY
                                    , Font.size 20
                                    , htmlAttribute <| Html.Attributes.tabindex -1
                                    , Background.color <| Shared.color.inputFieldBackground
                                    , padding 12
                                    , Border.roundEach { topLeft = 0, topRight = 10, bottomLeft = 0, bottomRight = 10 }
                                    ]
                                    { label = text "✕"
                                    , url = Shared.generateLocationHref { model | debounced_value = "" }
                                    }
                            ]
                            { label = Input.labelHidden ""
                            , onChange = Shared.MsgInput Shared.Value
                            , placeholder = Just <| Input.placeholder [] <| text "Your string here..."
                            , text = model.value
                            , spellcheck = True
                            }
                        , row
                            [ width fill
                            , spacing 10
                            , paddingEach { top = 0, right = 0, bottom = 20, left = 0 }
                            ]
                            [ Input.text
                                [ width fill
                                , Border.width 1
                                , Border.rounded Shared.color.inputFieldRounded
                                , Border.color Shared.color.inputFieldBorder
                                , Background.color Shared.color.inputFieldBackground
                                , Font.size 14
                                , Shared.shadow
                                , htmlAttribute <| Html.Attributes.tabindex tabindex
                                , paddingEach { top = 10, right = 30, bottom = 10, left = 40 }
                                , inFront <|
                                    el
                                        [ paddingEach { top = 9, right = 7, bottom = 9, left = 7 }
                                        , centerY
                                        , Font.size 16
                                        , Background.color <| Shared.color.inputFieldBackground
                                        , Border.roundEach { topLeft = 10, topRight = 0, bottomLeft = 10, bottomRight = 0 }
                                        ]
                                    <|
                                        text "🔍"
                                , inFront <|
                                    Shared.myLink
                                        [ alignRight
                                        , centerY
                                        , Font.size 16
                                        , htmlAttribute <| Html.Attributes.tabindex -1
                                        , Background.color <| Shared.color.inputFieldBackground
                                        , padding 8
                                        , Border.roundEach { topLeft = 0, topRight = 10, bottomLeft = 0, bottomRight = 10 }
                                        ]
                                        { label = text "✕"
                                        , url = Shared.generateLocationHref { model | debounced_query = "" }
                                        }
                                ]
                                { label = Input.labelHidden ""
                                , onChange = Shared.MsgInput Shared.Query
                                , placeholder = Just <| Input.placeholder [] <| text "Filter by package or function..."
                                , text = model.query
                                }
                            , Shared.myLink
                                [ Font.size 16
                                , mouseOver [ Background.color Shared.color.overedBackground ]
                                , focused [ Background.color Shared.color.focusedBackground ]
                                , paddingEach { top = 10, right = 20, bottom = 10, left = 20 }
                                , Border.rounded 30
                                , Background.color <| rgba 0 0 0 0.05
                                , htmlAttribute <| Html.Attributes.tabindex tabindex
                                , Border.color <| rgb 0.85 0.85 0.85
                                , Border.width 1
                                , Shared.shadow
                                ]
                                { label = text "View all"
                                , url = Shared.generateLocationHref { model | debounced_query = "" }
                                }
                            , Shared.myLink
                                [ mouseOver [ Background.color Shared.color.overedBackground ]
                                , focused [ Background.color Shared.color.focusedBackground ]
                                , paddingEach { top = 10, right = 20, bottom = 10, left = 20 }
                                , Border.rounded 30
                                , Background.color <| rgba 0 0 0 0.05
                                , Border.color <| rgb 0.85 0.85 0.85
                                , Border.width 1
                                , htmlAttribute <| Html.Attributes.tabindex tabindex
                                , Shared.shadow
                                ]
                                { label = text "⌂", url = "/" }
                            ]
                        ]
                   ]
            )
        ]


borderColor : Attr decorative msg
borderColor =
    Border.color <| rgba 0 0 0 0.1


type Page
    = PageSitemap
    | PageHome
    | PageFunctions


locationHrefToPage : String -> Page
locationHrefToPage locationHref =
    let
        path : String
        path =
            locationHref
                |> Url.fromString
                |> Maybe.map .path
                |> Maybe.withDefault "/"
    in
    if path == "/sitemap" then
        PageSitemap

    else if path == "/" then
        PageHome

    else
        PageFunctions


productionUrl : String
productionUrl =
    "https://stringy.guupa.com/"


view : Model -> Html.Html Shared.Msg
view model =
    case locationHrefToPage model.locationHref of
        PageSitemap ->
            Html.div [] <|
                let
                    urlList : List String
                    urlList =
                        List.map
                            (\( _, m2 ) ->
                                Shared.generateLocationHref
                                    { debounced_query = m2.debounced_query
                                    , debounced_valu1 = m2.debounced_valu1
                                    , debounced_valu2 = m2.debounced_valu2
                                    , debounced_valu3 = m2.debounced_valu3
                                    , debounced_valu4 = m2.debounced_valu4
                                    , debounced_value = m2.debounced_value
                                    , locationHref = model.locationHref
                                    }
                                    |> String.replace "http://localhost:4200/" productionUrl
                            )
                            PageHome.list
                in
                List.map
                    (\url -> Html.div [] [ Html.text url ])
                    ([ productionUrl
                     , productionUrl ++ "all_functions"
                     ]
                        ++ urlList
                    )

        PageHome ->
            pageView model [ Shared.homePageView PageHome.list model.innerWidth model.locationHref ]

        PageFunctions ->
            let
                pageContent : List (Element Shared.Msg)
                pageContent =
                    let
                        myRow : Shared.Function -> Element Shared.Msg
                        myRow =
                            myRow_ model

                        paddingSize : Int
                        paddingSize =
                            if Shared.smallScreen model.innerWidth then
                                0

                            else
                                20

                        filteredList : List (Element Shared.Msg)
                        filteredList =
                            List.map myRow
                                (List.filter
                                    (\item ->
                                        String.contains
                                            (String.toLower model.debounced_query)
                                            (String.toLower (item.prefix ++ "." ++ item.function))
                                            || String.contains (String.toLower model.debounced_query) (String.toLower item.package)
                                    )
                                    (PageFunctions.list model)
                                )
                    in
                    [ column [ paddingEach { top = 0, right = paddingSize, bottom = 0, left = paddingSize }, width fill ] <|
                        if List.isEmpty filteredList then
                            [ column
                                [ centerX
                                , paddingEach { top = 100, right = 0, bottom = 100, left = 0 }
                                , spacing 20
                                , Font.size 16
                                , Font.center
                                ]
                                [ el [ centerX ] <| html <| Material.Icons.Outlined.error 60 (Material.Icons.Types.Color <| Color.rgba 0 0 0 0.3)
                                , paragraph [] [ text "No functions found, you may need to change the filter." ]
                                ]
                            ]

                        else
                            filteredList
                    ]
            in
            pageView model pageContent


pageView : Model -> List (Element Shared.Msg) -> Html.Html Shared.Msg
pageView model pageContent =
    let
        bluePaddingSize : Int
        bluePaddingSize =
            if Shared.smallScreen model.innerWidth then
                10

            else
                30

        attrsHeaderContainer : List (Attribute msg)
        attrsHeaderContainer =
            [ Background.color <| rgb 1 1 1
            , borderColor
            , spacing 20
            , centerX
            , width (fill |> maximum 900)
            , paddingEach { top = bluePaddingSize, right = 0, bottom = 0, left = 0 }
            ]
    in
    layoutWith
        { options =
            [ focusStyle
                { borderColor = Nothing
                , backgroundColor = Just <| rgb 1 1 1
                , shadow =
                    Just
                        { color = Shared.color.primary
                        , offset = ( 0, 0 )
                        , blur = 1
                        , size = 2
                        }
                }
            ]
        }
        [ Background.color Shared.color.primary
        , padding bluePaddingSize
        , inFront <|
            el
                [ Background.color Shared.color.primary
                , paddingEach { top = bluePaddingSize, right = bluePaddingSize, bottom = 0, left = bluePaddingSize }
                , width fill
                ]
                (column
                    (Border.roundEach { topLeft = 10, topRight = 10, bottomLeft = 0, bottomRight = 0 }
                        :: attrsHeaderContainer
                    )
                    [ header [] 1 model ]
                )
        , inFront <|
            html <|
                Html.node "style" [] [ Html.text <| ".white-space-pre > div {white-space: break-spaces !important} body {background-color:" ++ Shared.color.primaryAsString ++ "}" ]
        ]
    <|
        column [ width fill, spacing 40, height fill ]
            [ column
                (Border.rounded 10 :: attrsHeaderContainer)
                ([ header [ htmlAttribute <| Html.Attributes.style "opacity" "0" ] -1 model ] ++ pageContent)
            , paragraph [ alignBottom, Font.center, Font.color <| rgba 1 1 1 0.6, Font.size 14 ]
                [ newTabLink [] { url = "https://github.com/lucamug/stringy", label = text "github" }
                , text " - "
                , newTabLink [] { url = "https://twitter.com/luca_mug/status/1585022666259726338", label = text "twitter" }
                , text " - "
                , newTabLink [] { url = "https://elm-lang.org/", label = text "elmlang" }
                , text " - "
                , newTabLink [] { url = "https://github.com/lucamug/stringy/commit/" ++ model.env.commitHash, label = text model.env.commitHash }
                ]
            ]


myRow_ : Model -> Shared.Function -> Element Shared.Msg
myRow_ model args =
    let
        colorSignature : Color
        colorSignature =
            Shared.color.primary

        paddingSize : Int
        paddingSize =
            70
    in
    column [ width fill, spacing 30 ]
        [ column
            [ paddingEach { top = paddingSize, right = 20, bottom = 0, left = 20 }
            , spacing 5
            , Font.size 18
            ]
            [ Shared.myParagraph
                [ Font.family [ Font.typeface "Fira Code", Font.monospace ]
                , htmlAttribute <| Html.Attributes.class "white-space-pre"
                ]
                (args.executionBefore
                    ++ [ text args.prefix
                       , text "."
                       , el [ Font.bold ] <| text args.function
                       , text " "
                       ]
                    ++ args.executionAfter
                    ++ [ Shared.myParagraph
                            -- [ Font.color Shared.color.primary ]
                            [ Font.color <| rgb 0.9 0.1 0.1, Font.heavy ]
                            ([ text " -- " ]
                                ++ args.outcome
                            )
                       ]
                )
            ]
        , column
            [ paddingEach { top = 0, right = 20, bottom = paddingSize, left = 50 }
            , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
            , borderColor
            , Font.size 16
            , spacing 8
            , width fill
            ]
            [ Shared.myParagraph []
                [ text args.description
                , text " ["
                , newTabLink
                    [ Font.color Shared.color.link
                    , htmlAttribute <| Html.Attributes.tabindex 10
                    , mouseOver [ Background.color Shared.color.overedBackground ]
                    , focused [ Background.color Shared.color.focusedBackground ]
                    ]
                    { url = packageUrlGenerator "official" args model.dependencies
                    , label =
                        Shared.myParagraph []
                            [ text " "
                            , text args.package
                            , case Dict.get args.package model.dependencies of
                                Just version ->
                                    text <| " " ++ version

                                Nothing ->
                                    text ""
                            , text " "
                            ]
                    }
                , newTabLink
                    [ Font.color Shared.color.link
                    , htmlAttribute <| Html.Attributes.tabindex 10
                    , mouseOver [ Background.color Shared.color.overedBackground ]
                    , focused [ Background.color Shared.color.focusedBackground ]
                    ]
                    { url = packageUrlGenerator "" args model.dependencies
                    , label =
                        Shared.myParagraph []
                            [ el [] <| html iconExternalLink
                            , text " "
                            ]
                    }
                , text "]"
                ]
            , Shared.myParagraph
                [ Font.family [ Font.monospace ]
                , Font.color colorSignature

                -- , Font.size 16
                , htmlAttribute <| Html.Attributes.class "white-space-pre"
                ]
                ([ el [ Font.bold ] <| text args.function
                 , el [ Font.color <| rgba 0 0 0 0.6 ] <| text " : "
                 ]
                    -- → ⮕
                    ++ List.map
                        (\s ->
                            if s == "->" then
                                el [ Font.color <| rgba 0 0 0 0.6 ] <| text <| "→" ++ " "

                            else
                                text <| s ++ " "
                        )
                        (String.split " " args.signature)
                )
            ]
        ]


iconExternalLink : Html.Html msg
iconExternalLink =
    Svg.svg
        [ Svg.Attributes.width "20"
        , Svg.Attributes.height "20"
        , Svg.Attributes.viewBox "0 0 22 18"
        , Svg.Attributes.xmlLang "http://www.w3.org/2000/svg"
        ]
        [ Svg.path
            [ Svg.Attributes.d "M17 13.5v6H5v-12h6m3-3h6v6m0-6-9 9"
            , Svg.Attributes.stroke <| Color.Convert.colorToCssRgba <| Shared.elmentColorToElmColor Shared.color.link
            , Svg.Attributes.strokeWidth "1.5"
            , Svg.Attributes.fill "none"
            , Svg.Attributes.fillRule "evenodd"
            , Svg.Attributes.strokeLinecap "round"
            , Svg.Attributes.strokeLinejoin "round"
            ]
            []
        ]


packageUrlGenerator :
    String
    -> { a | function : String, package : String, prefix : String }
    -> Dict.Dict String String
    -> String
packageUrlGenerator type_ args dependencies =
    (if type_ == "official" then
        "https://package.elm-lang.org/packages/"

     else
        "https://elm.dmy.fr/packages/"
    )
        ++ args.package
        ++ "/"
        ++ (case Dict.get args.package dependencies of
                Just version ->
                    version

                Nothing ->
                    "latest"
           )
        ++ "/"
        ++ args.prefix
        ++ "#"
        ++ args.function
