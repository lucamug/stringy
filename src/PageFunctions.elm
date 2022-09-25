module PageFunctions exposing (list)

import Base64
import Color
import Color.Accessibility
import Color.Convert
import Color.Manipulate
import Crypto.Hash
import Crypto.Strings
import Date
import Element exposing (..)
import Element.Font as Font
import Hex
import Html
import Html.Attributes
import Iso8601
import Json.Decode
import Json.Decode.Generic
import MD5
import Murmur3
import Path
import Path.Platform
import QRCode
import Random
import Regex
import Shared
import String.Extra
import Time
import Url


list : Shared.AllValues a -> List Shared.Function
list model =
    []
        --
        -- pablohirafuji/elm-qrcode
        --
        ++ [ { prefix = "QRCode"
             , function = "fromString"
             , signature = "String -> Result Error QRCode"
             , description = "Transform a string into a QR Code. (\"QR Code\" is registered trademark of DENSO WAVE INCORPORATED)"
             , package = "pablohirafuji/elm-qrcode"
             , executionBefore = []
             , executionAfter = Shared.inputField model Shared.Value Shared.FieldString
             , outcome =
                let
                    qrCodeView : String -> QRCode.ErrorCorrection -> Html.Html msg
                    qrCodeView message errorCorrection =
                        QRCode.fromStringWith errorCorrection message
                            |> Result.map
                                (QRCode.toSvg
                                    [ Html.Attributes.width 100
                                    , Html.Attributes.height 100
                                    ]
                                )
                            |> Result.withDefault (Html.text "Error while encoding to QRCode.")
                in
                [ text <|
                    case QRCode.fromString model.debounced_value of
                        Ok _ ->
                            "Ok QRCode"

                        Err _ ->
                            "Err QRCode.Error"
                , column [ paddingEach { top = 0, right = 0, bottom = 0, left = 5 } ]
                    [ el [ centerX ] <| html <| qrCodeView model.debounced_value QRCode.Low
                    , Shared.myParagraph [ Font.size 10, Font.center ] [ text "Low Correction" ]
                    ]
                , text " "
                , column [ paddingEach { top = 0, right = 0, bottom = 0, left = 5 } ]
                    [ el [ centerX ] <| html <| qrCodeView model.debounced_value QRCode.High
                    , Shared.myParagraph [ Font.size 10, Font.center ] [ text "High Correction" ]
                    ]
                ]
             }
           ]
        --
        -- elm/core
        --
        ++ [ { prefix = "String"
             , function = "isEmpty"
             , signature = "String -> Bool"
             , description = "Determine if a string is empty"
             , package = "elm/core"
             , executionBefore = []
             , executionAfter = Shared.inputField model Shared.Value Shared.FieldString
             , outcome = [ text <| Debug.toString <| String.isEmpty model.debounced_value ]
             }
           , { prefix = "String"
             , function = "length"
             , signature = "String -> Int"
             , description = "Get the length of a string."
             , package = "elm/core"
             , executionBefore = []
             , executionAfter = Shared.inputField model Shared.Value Shared.FieldString
             , outcome = [ text <| Debug.toString <| String.length model.debounced_value ]
             }
           , { prefix = "String"
             , function = "reverse"
             , signature = "String -> String"
             , description = "Reverse a string."
             , package = "elm/core"
             , executionBefore = []
             , executionAfter = Shared.inputField model Shared.Value Shared.FieldString
             , outcome = [ text <| Debug.toString <| String.reverse model.debounced_value ]
             }
           , { prefix = "String"
             , function = "repeat"
             , signature = "Int -> String -> String"
             , description = "Repeat a string n times."
             , package = "elm/core"
             , executionBefore = []
             , executionAfter = Shared.inputField model Shared.Valu1 Shared.FieldInt ++ Shared.space ++ Shared.inputField model Shared.Value Shared.FieldString
             , outcome = [ text <| Debug.toString <| String.repeat (Maybe.withDefault 1 (String.toInt model.debounced_valu1)) model.debounced_value ]
             }
           , { prefix = "String"
             , function = "replace"
             , signature = "String -> String -> String -> String"
             , description = "Replace all occurrences of some substring."
             , package = "elm/core"
             , executionBefore = []
             , executionAfter = Shared.inputField model Shared.Valu1 Shared.FieldString ++ Shared.space ++ Shared.inputField model Shared.Valu2 Shared.FieldString ++ Shared.space ++ Shared.inputField model Shared.Value Shared.FieldString
             , outcome = [ text <| Debug.toString <| String.replace model.debounced_valu1 model.debounced_valu2 model.debounced_value ]
             }

           --
           , { prefix = "String"
             , function = "append"
             , signature = "String -> String -> String"
             , description = "Append two strings. You can also use the (++) operator to do this."
             , package = "elm/core"
             , executionBefore = []
             , executionAfter = Shared.inputField model Shared.Valu1 Shared.FieldString ++ Shared.space ++ Shared.inputField model Shared.Value Shared.FieldString
             , outcome = [ text <| Debug.toString <| String.append model.debounced_valu1 model.debounced_value ]
             }
           , { prefix = "String"
             , function = "split"
             , signature = "String -> String -> List String"
             , description = "Split a string using a given separator."
             , package = "elm/core"
             , executionBefore = []
             , executionAfter = Shared.inputField model Shared.Valu1 Shared.FieldString ++ Shared.space ++ Shared.inputField model Shared.Value Shared.FieldString
             , outcome = [ text <| Debug.toString <| String.split model.valu1 model.debounced_value ]
             }
           , { prefix = "String"
             , function = "words"
             , signature = "String -> List String"
             , description = "Break a string into words, splitting on chunks of whitespace."
             , package = "elm/core"
             , executionBefore = []
             , executionAfter = Shared.inputField model Shared.Value Shared.FieldString
             , outcome = [ text <| Debug.toString <| String.words model.debounced_value ]
             }
           , { prefix = "String"
             , function = "lines"
             , signature = "String -> List String"
             , description = "Break a string into lines, splitting on newlines."
             , package = "elm/core"
             , executionBefore = []
             , executionAfter = Shared.inputField model Shared.Value Shared.FieldString
             , outcome = [ text <| Debug.toString <| String.lines model.debounced_value ]
             }

           -- Get Substrings https://elm.dmy.fr/packages/elm/core/latest/String#get-substrings
           , { prefix = "String"
             , function = "slice"
             , signature = "Int -> Int -> String -> String"
             , description = "Take a substring given a start and end index. Negative indexes are taken starting from the end of the list."
             , package = "elm/core"
             , executionBefore = []
             , executionAfter = Shared.inputField model Shared.Valu1 Shared.FieldInt ++ Shared.space ++ Shared.inputField model Shared.Valu2 Shared.FieldInt ++ Shared.space ++ Shared.inputField model Shared.Value Shared.FieldString
             , outcome = [ text <| Debug.toString <| String.slice (Maybe.withDefault 0 (String.toInt model.debounced_valu1)) (Maybe.withDefault 0 (String.toInt model.debounced_valu2)) model.debounced_value ]
             }
           , { prefix = "String"
             , function = "left"
             , signature = "Int -> String -> String"
             , description = "Take n characters from the left side of a string."
             , package = "elm/core"
             , executionBefore = []
             , executionAfter = Shared.inputField model Shared.Valu1 Shared.FieldInt ++ Shared.space ++ Shared.inputField model Shared.Value Shared.FieldString
             , outcome = [ text <| Debug.toString <| String.left (Maybe.withDefault 0 (String.toInt model.debounced_valu1)) model.debounced_value ]
             }
           , { prefix = "String"
             , function = "right"
             , signature = "Int -> String -> String"
             , description = "Take n characters from the right side of a string."
             , package = "elm/core"
             , executionBefore = []
             , executionAfter = Shared.inputField model Shared.Valu1 Shared.FieldInt ++ Shared.space ++ Shared.inputField model Shared.Value Shared.FieldString
             , outcome = [ text <| Debug.toString <| String.right (Maybe.withDefault 0 (String.toInt model.debounced_valu1)) model.debounced_value ]
             }
           , { prefix = "String"
             , function = "dropLeft"
             , signature = "Int -> String -> String"
             , description = "Drop n characters from the left side of a string."
             , package = "elm/core"
             , executionBefore = []
             , executionAfter = Shared.inputField model Shared.Valu1 Shared.FieldInt ++ Shared.space ++ Shared.inputField model Shared.Value Shared.FieldString
             , outcome = [ text <| Debug.toString <| String.dropLeft (Maybe.withDefault 0 (String.toInt model.debounced_valu1)) model.debounced_value ]
             }
           , { prefix = "String"
             , function = "dropRight"
             , signature = "Int -> String -> String"
             , description = "Drop n characters from the right side of a string."
             , package = "elm/core"
             , executionBefore = []
             , executionAfter = Shared.inputField model Shared.Valu1 Shared.FieldInt ++ Shared.space ++ Shared.inputField model Shared.Value Shared.FieldString
             , outcome = [ text <| Debug.toString <| String.dropRight (Maybe.withDefault 0 (String.toInt model.debounced_valu1)) model.debounced_value ]
             }

           -- Check for Substrings https://elm.dmy.fr/packages/elm/core/latest/String#check-for-substrings
           , { prefix = "String"
             , function = "contains"
             , signature = "String -> String -> Bool"
             , description = "See if the second string contains the first one."
             , package = "elm/core"
             , executionBefore = []
             , executionAfter = Shared.inputField model Shared.Valu1 Shared.FieldString ++ Shared.space ++ Shared.inputField model Shared.Value Shared.FieldString
             , outcome = [ text <| Debug.toString <| String.contains model.debounced_valu1 model.debounced_value ]
             }
           , { prefix = "String"
             , function = "startsWith"
             , signature = "String -> String -> Bool"
             , description = "See if the second string starts with the first one."
             , package = "elm/core"
             , executionBefore = []
             , executionAfter = Shared.inputField model Shared.Valu1 Shared.FieldString ++ Shared.space ++ Shared.inputField model Shared.Value Shared.FieldString
             , outcome = [ text <| Debug.toString <| String.startsWith model.debounced_valu1 model.debounced_value ]
             }
           , { prefix = "String"
             , function = "endsWith"
             , signature = "String -> String -> Bool"
             , description = "See if the second string ends with the first one."
             , package = "elm/core"
             , executionBefore = []
             , executionAfter = Shared.inputField model Shared.Valu1 Shared.FieldString ++ Shared.space ++ Shared.inputField model Shared.Value Shared.FieldString
             , outcome = [ text <| Debug.toString <| String.endsWith model.debounced_valu1 model.debounced_value ]
             }
           , { prefix = "String"
             , function = "indexes"
             , signature = "String -> String -> List Int"
             , description = "Get all of the indexes for a substring in another string."
             , package = "elm/core"
             , executionBefore = []
             , executionAfter = Shared.inputField model Shared.Valu1 Shared.FieldString ++ Shared.space ++ Shared.inputField model Shared.Value Shared.FieldString
             , outcome = [ text <| Debug.toString <| String.indexes model.debounced_valu1 model.debounced_value ]
             }

           --
           , { prefix = "String"
             , function = "toInt"
             , signature = "String -> Maybe Int"
             , description = "Try to convert a string into an int, failing on improperly formatted strings."
             , package = "elm/core"
             , executionBefore = []
             , executionAfter = Shared.inputField model Shared.Value Shared.FieldString
             , outcome = [ text <| Debug.toString <| String.toInt model.debounced_value ]
             }
           , { prefix = "String"
             , function = "toFloat"
             , signature = "String -> Maybe Float"
             , description = "Try to convert a string into a float, failing on improperly formatted strings."
             , package = "elm/core"
             , executionBefore = []
             , executionAfter = Shared.inputField model Shared.Value Shared.FieldString
             , outcome = [ text <| Debug.toString <| String.toFloat model.debounced_value ]
             }

           -- Formatting
           -- https://elm.dmy.fr/packages/elm/core/latest/String#toUpper
           --
           , { prefix = "String"
             , function = "toUpper"
             , signature = "String -> Float"
             , description = "Convert a string to all upper case. Useful for case-insensitive comparisons and VIRTUAL YELLING."
             , package = "elm/core"
             , executionBefore = []
             , executionAfter = Shared.inputField model Shared.Value Shared.FieldString
             , outcome = [ text <| Debug.toString <| String.toUpper model.debounced_value ]
             }
           , { prefix = "String"
             , function = "toLower"
             , signature = "String -> Float"
             , description = "Convert a string to all lower case. Useful for case-insensitive comparisons."
             , package = "elm/core"
             , executionBefore = []
             , executionAfter = Shared.inputField model Shared.Value Shared.FieldString
             , outcome = [ text <| Debug.toString <| String.toLower model.debounced_value ]
             }
           , { prefix = "String"
             , function = "pad"
             , signature = "Int -> Char -> String -> String"
             , description = "Pad a string on both sides until it has a given length."
             , package = "elm/core"
             , executionBefore = []
             , executionAfter =
                Shared.inputField model Shared.Valu1 Shared.FieldInt
                    ++ Shared.space
                    ++ Shared.inputField model Shared.Valu2 Shared.FieldChar
                    ++ Shared.space
                    ++ Shared.inputField model Shared.Value Shared.FieldString
             , outcome =
                [ text <|
                    Debug.toString <|
                        String.pad
                            (Maybe.withDefault 0 (String.toInt model.debounced_valu1))
                            (Maybe.withDefault ' ' (List.head (String.toList model.debounced_valu2)))
                            model.debounced_value
                ]
             }
           , { prefix = "String"
             , function = "trim"
             , signature = "String -> String"
             , description = "Get rid of whitespace on both sides of a string."
             , package = "elm/core"
             , executionBefore = []
             , executionAfter = Shared.inputField model Shared.Value Shared.FieldString
             , outcome = [ text <| Debug.toString <| String.trim model.debounced_value ]
             }
           ]
        --
        -- elm/regex
        --
        ++ [ let
                maybeRegex : Maybe Regex.Regex
                maybeRegex =
                    Regex.fromString model.debounced_value
             in
             { prefix = "Regex"
             , function = "fromString"
             , signature = "String -> Maybe Regex"
             , description = "Try to create a Regex."
             , package = "elm/regex"
             , executionBefore = []
             , executionAfter = Shared.inputField model Shared.Value Shared.FieldString
             , outcome =
                [ text <|
                    case maybeRegex of
                        Just _ ->
                            "Just Regex"

                        Nothing ->
                            "Nothing"
                ]
             }
           , { prefix = "Regex"
             , function = "find"
             , signature = "Regex -> String -> List Match"
             , description = "Find matches in a string."
             , package = "elm/regex"
             , executionBefore = []
             , executionAfter = [ text "(Maybe.withDefault Regex.never (fromString \"" ] ++ Shared.inputField model Shared.Value Shared.FieldString ++ [ text ")) " ] ++ Shared.inputField model Shared.Valu1 Shared.FieldString
             , outcome = [ Regex.find (Maybe.withDefault Regex.never (Regex.fromString model.debounced_value)) model.debounced_valu1 |> Debug.toString |> text ]
             }
           ]
        --
        -- elm/time
        --
        ++ [ { prefix = "Time"
             , function = "toYear"
             , signature = "Zone -> Posix -> Int"
             , description = "Converta Posix into Year."
             , package = "elm/time"
             , executionBefore = []
             , executionAfter = [ text "Time.utc (Time.millisToPosix " ] ++ Shared.inputField model Shared.Value Shared.FieldInt ++ [ text ")" ]
             , outcome = [ model.debounced_value |> String.toInt |> Maybe.withDefault 0 |> Time.millisToPosix |> Time.toYear Time.utc |> Debug.toString |> text ]
             }
           ]
        --
        -- truqu/elm-base64
        --
        ++ [ { prefix = "Base64"
             , function = "encode"
             , signature = "String -> String"
             , description = "Encode any string in Base64"
             , package = "truqu/elm-base64"
             , executionBefore = []
             , executionAfter = Shared.inputField model Shared.Value Shared.FieldString
             , outcome = [ text <| Debug.toString <| Base64.encode model.debounced_value ]
             }
           , { prefix = "Base64"
             , function = "decode"
             , signature = "String -> Result String String"
             , description = "Decode Base64-encoded strings"
             , package = "truqu/elm-base64"
             , executionBefore = []
             , executionAfter = Shared.inputField model Shared.Value Shared.FieldString
             , outcome = [ text <| Debug.toString <| Base64.decode model.debounced_value ]
             }
           ]
        --
        -- rtfeldman/elm-iso8601-date-strings
        --
        ++ [ { prefix = "Iso8601"
             , function = "fromTime"
             , signature = "Posix -> String"
             , description = "Inflate a Posix integer into a more memory-intensive ISO-8601 date string (Format: YYYY-MM-DDTHH:mm:ss.SSSZ)."
             , package = "rtfeldman/elm-iso8601-date-strings"
             , executionBefore = []
             , executionAfter =
                [ text "(Time.millisToPosix " ]
                    ++ Shared.inputField model Shared.Value Shared.FieldInt
                    ++ [ text ")" ]
             , outcome =
                [ model.debounced_value
                    |> String.toInt
                    |> Maybe.map Time.millisToPosix
                    |> Maybe.map Iso8601.fromTime
                    |> Debug.toString
                    |> text
                ]
             }
           , { prefix = "Iso8601"
             , function = "toTime"
             , signature = "String -> Result (List DeadEnd) Posix"
             , description = "Convert from an ISO-8601 date string to a Time.Posix value."
             , package = "rtfeldman/elm-iso8601-date-strings"
             , executionBefore = []
             , executionAfter = Shared.inputField model Shared.Value Shared.FieldString
             , outcome = [ model.debounced_value |> Iso8601.toTime |> Debug.toString |> text ]
             }
           ]
        --
        -- justinmimbs/date
        --
        ++ [ { prefix = "Date"
             , function = "fromIsoString"
             , signature = "String -> Result String Date"
             , description = "Attempt to create a date from a string in ISO 8601 format. Calendar dates, week dates, and ordinal dates are all supported in extended and basic format. Rata Die (RD) is a system for assigning numbers to calendar days, where the number 1 represents the date 1 January 0001."
             , package = "justinmimbs/date"
             , executionBefore = []
             , executionAfter = Shared.inputField model Shared.Value Shared.FieldString
             , outcome = [ text <| Debug.toString <| Date.fromIsoString model.debounced_value ]
             }
           , { prefix = "Date"
             , function = "format"
             , signature = "String -> Date -> String"
             , description = "Format a date using a string as a template. Rata Die (RD) is a system for assigning numbers to calendar days, where the number 1 represents the date 1 January 0001."
             , package = "justinmimbs/date"
             , executionBefore = []
             , executionAfter = Shared.inputField model Shared.Value Shared.FieldString ++ [ text " (Date.fromRateDie " ] ++ Shared.inputField model Shared.Valu1 Shared.FieldInt ++ [ text ")" ]
             , outcome = [ text <| Debug.toString <| Date.format model.debounced_value (Date.fromRataDie (Maybe.withDefault 0 (String.toInt model.debounced_valu1))) ]
             }
           ]
        --
        -- rtfeldman/elm-hex
        --
        ++ [ { prefix = "Hex"
             , function = "fromString"
             , signature = "String -> Result String Int"
             , description = "Convert a hexdecimal string such as \"abc94f\" to a decimal integer."
             , package = "rtfeldman/elm-hex"
             , executionBefore = []
             , executionAfter = Shared.inputField model Shared.Value Shared.FieldString
             , outcome = [ model.debounced_value |> Hex.fromString |> Debug.toString |> text ]
             }
           , { prefix = "Hex"
             , function = "toString"
             , signature = "Int -> String"
             , description = "Convert a decimal integer to a hexdecimal string such as \"abc94f\""
             , package = "rtfeldman/elm-hex"
             , executionBefore = []
             , executionAfter = Shared.inputField model Shared.Value Shared.FieldInt
             , outcome =
                [ text <|
                    if
                        model.debounced_value
                            |> String.toInt
                            |> Maybe.withDefault 0
                            |> (>) 100000000000
                    then
                        model.debounced_value
                            |> String.toInt
                            |> Maybe.withDefault 0
                            |> Hex.toString
                            |> Debug.toString

                    else
                        "Number too big, see https://github.com/rtfeldman/elm-hex/issues/3"
                ]
             }
           ]
        --
        -- the-sett/decode-generic
        --
        ++ [ { prefix = "Json.Decode.Generic"
             , function = "json"
             , signature = "Decoder Json"
             , description = "A JSON decoder that works with any JSON, decoding into the generic data model"
             , package = "the-sett/decode-generic"
             , executionBefore = []
             , executionAfter = Shared.inputField model Shared.Value Shared.FieldString
             , outcome = [ text <| Debug.toString <| Json.Decode.decodeString Json.Decode.Generic.json model.value ]
             }
           ]
        --
        -- noahzgordon/elm-color-extra
        --
        ++ [ { prefix = "Color.Convert"
             , function = "colorToCssRgba"
             , signature = "Color -> String"
             , description = "Converts a color from HEX format to RGBA (Red, Green, Blue, and Alpha) format."
             , package = "noahzgordon/elm-color-extra"
             , executionBefore = [ text "Result.map " ]
             , executionAfter =
                [ text "( Color.Convert.hexToColor " ]
                    ++ Shared.inputField model Shared.Value Shared.FieldString
                    ++ [ text " )" ]
             , outcome =
                let
                    result : Result String String
                    result =
                        Result.map Color.Convert.colorToCssRgba (Color.Convert.hexToColor model.debounced_value)
                in
                [ text <| Debug.toString result, text " ", Shared.coloredCircleFromResult result ]
             }
           , { prefix = "Color.Convert"
             , function = "colorToCssHsla"
             , signature = "Color -> String"
             , description = "Converts a color from HEX format to HSLA (Hue, Saturation, Lightness, and Alpha) format."
             , package = "noahzgordon/elm-color-extra"
             , executionBefore = [ text "Result.map " ]
             , executionAfter =
                [ text "( Color.Convert.hexToColor " ]
                    ++ Shared.inputField model Shared.Value Shared.FieldString
                    ++ [ text " )" ]
             , outcome =
                let
                    result : Result String String
                    result =
                        Result.map Color.Convert.colorToCssHsla (Color.Convert.hexToColor model.debounced_value)
                in
                [ text <| Debug.toString result, text " ", Shared.coloredCircleFromResult result ]
             }
           , { prefix = "Color.Manipulate"
             , function = "darken"
             , signature = "Float -> Color -> Color"
             , description = "Decrease the lightning of a color."
             , package = "noahzgordon/elm-color-extra"
             , executionBefore = []
             , executionAfter =
                Shared.inputField model Shared.Valu1 Shared.FieldFloat
                    ++ Shared.space
                    ++ Shared.inputField model Shared.Value Shared.FieldString
             , outcome =
                let
                    maybeStartColor : Result String Color.Color
                    maybeStartColor =
                        Color.Convert.hexToColor model.debounced_value
                in
                case maybeStartColor of
                    Ok startColor ->
                        let
                            endColor : Color.Color
                            endColor =
                                Color.Manipulate.darken (Maybe.withDefault 0 (String.toFloat model.debounced_valu1)) startColor
                        in
                        [ text <| "Ok (Color " ++ Color.Convert.colorToHexWithAlpha endColor ++ ") "
                        , Shared.coloredCircle startColor
                        , Shared.arrow
                        , Shared.coloredCircle endColor
                        ]

                    Err _ ->
                        [ text <| Debug.toString maybeStartColor ]
             }
           , { prefix = "Color.Manipulate"
             , function = "lighten"
             , signature = "Float -> Color -> Color"
             , description = "Increase the lightning of a color."
             , package = "noahzgordon/elm-color-extra"
             , executionBefore = []
             , executionAfter =
                Shared.inputField model Shared.Valu1 Shared.FieldFloat
                    ++ Shared.space
                    ++ Shared.inputField model Shared.Value Shared.FieldString
             , outcome =
                let
                    maybeStartColor : Result String Color.Color
                    maybeStartColor =
                        Color.Convert.hexToColor model.debounced_value
                in
                case maybeStartColor of
                    Ok startColor ->
                        let
                            endColor : Color.Color
                            endColor =
                                Color.Manipulate.lighten (Maybe.withDefault 0 (String.toFloat model.debounced_valu1)) startColor
                        in
                        [ text <| "Ok (Color " ++ Color.Convert.colorToHexWithAlpha endColor ++ ") "
                        , Shared.coloredCircle startColor
                        , Shared.arrow
                        , Shared.coloredCircle endColor
                        ]

                    Err _ ->
                        [ text <| Debug.toString maybeStartColor ]
             }
           , { prefix = "Color.Manipulate"
             , function = "saturate"
             , signature = "Float -> Color -> Color"
             , description = "Increase the saturation of a color."
             , package = "noahzgordon/elm-color-extra"
             , executionBefore = []
             , executionAfter =
                Shared.inputField model Shared.Valu1 Shared.FieldFloat
                    ++ Shared.space
                    ++ Shared.inputField model Shared.Value Shared.FieldString
             , outcome =
                let
                    maybeStartColor : Result String Color.Color
                    maybeStartColor =
                        Color.Convert.hexToColor model.debounced_value
                in
                case maybeStartColor of
                    Ok startColor ->
                        let
                            endColor : Color.Color
                            endColor =
                                Color.Manipulate.saturate (Maybe.withDefault 0 (String.toFloat model.debounced_valu1)) startColor
                        in
                        [ text <| "Ok (Color " ++ Color.Convert.colorToHexWithAlpha endColor ++ ") "
                        , Shared.coloredCircle startColor
                        , Shared.arrow
                        , Shared.coloredCircle endColor
                        ]

                    Err _ ->
                        [ text <| Debug.toString maybeStartColor ]
             }
           , { prefix = "Color.Manipulate"
             , function = "desaturate"
             , signature = "Float -> Color -> Color"
             , description = "Decrease the saturation of a color."
             , package = "noahzgordon/elm-color-extra"
             , executionBefore = []
             , executionAfter =
                Shared.inputField model Shared.Valu1 Shared.FieldFloat
                    ++ Shared.space
                    ++ Shared.inputField model Shared.Value Shared.FieldString
             , outcome =
                let
                    maybeStartColor : Result String Color.Color
                    maybeStartColor =
                        Color.Convert.hexToColor model.debounced_value
                in
                case maybeStartColor of
                    Ok startColor ->
                        let
                            endColor : Color.Color
                            endColor =
                                Color.Manipulate.desaturate (Maybe.withDefault 0 (String.toFloat model.debounced_valu1)) startColor
                        in
                        [ text <| "Ok (Color " ++ Color.Convert.colorToHexWithAlpha endColor ++ ") "
                        , Shared.coloredCircle startColor
                        , Shared.arrow
                        , Shared.coloredCircle endColor
                        ]

                    Err _ ->
                        [ text <| Debug.toString maybeStartColor ]
             }
           , { prefix = "Color.Manipulate"
             , function = "rotateHue"
             , signature = "Float -> Color -> Color"
             , description = "Change the hue of a color. The angle value must be in degrees."
             , package = "noahzgordon/elm-color-extra"
             , executionBefore = []
             , executionAfter = Shared.inputField model Shared.Valu1 Shared.FieldFloat ++ Shared.space ++ Shared.inputField model Shared.Value Shared.FieldString
             , outcome =
                let
                    maybeStartColor : Result String Color.Color
                    maybeStartColor =
                        Color.Convert.hexToColor model.debounced_value
                in
                case maybeStartColor of
                    Ok startColor ->
                        let
                            endColor : Color.Color
                            endColor =
                                Color.Manipulate.rotateHue (Maybe.withDefault 0 (String.toFloat model.debounced_valu1)) startColor
                        in
                        [ text <| "Ok (Color " ++ Color.Convert.colorToHexWithAlpha endColor ++ ") "
                        , Shared.coloredCircle startColor
                        , Shared.arrow
                        , Shared.coloredCircle endColor
                        ]

                    Err _ ->
                        [ text <| Debug.toString maybeStartColor ]
             }
           , { prefix = "Color.Manipulate"
             , function = "grayscale"
             , signature = "Color -> Color"
             , description = "Convert the color to a greyscale version, aka set saturation to 0."
             , package = "noahzgordon/elm-color-extra"
             , executionBefore = []
             , executionAfter = Shared.inputField model Shared.Value Shared.FieldString
             , outcome =
                let
                    maybeStartColor : Result String Color.Color
                    maybeStartColor =
                        Color.Convert.hexToColor model.debounced_value
                in
                case maybeStartColor of
                    Ok startColor ->
                        let
                            endColor : Color.Color
                            endColor =
                                Color.Manipulate.grayscale startColor
                        in
                        [ text <| "Ok (Color " ++ Color.Convert.colorToHexWithAlpha endColor ++ ") "
                        , Shared.coloredCircle startColor
                        , Shared.arrow
                        , Shared.coloredCircle endColor
                        ]

                    Err _ ->
                        [ text <| Debug.toString maybeStartColor ]
             }
           , { prefix = "Color.Manipulate"
             , function = "scaleHsl"
             , signature = "{ saturationScale : Float, lightnessScale : Float, alphaScale : Float } -> Color -> Color"
             , description = "Fluidly scale saturation, lightness and alpha channel. This function is inspired by the Sass function scale-color."
             , package = "noahzgordon/elm-color-extra"
             , executionBefore = []
             , executionAfter =
                [ text "{saturationScale = " ]
                    ++ Shared.inputField model Shared.Valu1 Shared.FieldFloat
                    ++ [ text ", lightnessScale = " ]
                    ++ Shared.inputField model Shared.Valu2 Shared.FieldFloat
                    ++ [ text ", alphaScale = " ]
                    ++ Shared.inputField model Shared.Valu3 Shared.FieldFloat
                    ++ [ text "} " ]
                    ++ Shared.inputField model Shared.Value Shared.FieldString
             , outcome =
                let
                    maybeStartColor : Result String Color.Color
                    maybeStartColor =
                        Color.Convert.hexToColor model.debounced_value
                in
                case maybeStartColor of
                    Ok startColor ->
                        let
                            endColor : Color.Color
                            endColor =
                                Color.Manipulate.scaleHsl
                                    { saturationScale = Maybe.withDefault 0 (String.toFloat model.debounced_valu1)
                                    , lightnessScale = Maybe.withDefault 0 (String.toFloat model.debounced_valu2)
                                    , alphaScale = Maybe.withDefault 0 (String.toFloat model.debounced_valu3)
                                    }
                                    startColor
                        in
                        [ text <| "Ok (Color " ++ Color.Convert.colorToHexWithAlpha endColor ++ ") "
                        , Shared.coloredCircle startColor
                        , Shared.arrow
                        , Shared.coloredCircle endColor
                        ]

                    Err _ ->
                        [ text <| Debug.toString maybeStartColor ]
             }
           , { prefix = "Color.Manipulate"
             , function = "scaleRgb"
             , signature = "{ redScale : Float, greenScale : Float, blueScale : Float, alphaScale : Float } -> Color -> Color"
             , description = "Fluidly scale red, green, blue, and alpha channels."
             , package = "noahzgordon/elm-color-extra"
             , executionBefore = []
             , executionAfter =
                [ text "{ redScale = " ]
                    ++ Shared.inputField model Shared.Valu1 Shared.FieldFloat
                    ++ [ text ", greenScale = " ]
                    ++ Shared.inputField model Shared.Valu2 Shared.FieldFloat
                    ++ [ text ", blueScale = " ]
                    ++ Shared.inputField model Shared.Valu3 Shared.FieldFloat
                    ++ [ text ", alphaScale = " ]
                    ++ Shared.inputField model Shared.Valu4 Shared.FieldFloat
                    ++ [ text " } " ]
                    ++ Shared.inputField model Shared.Value Shared.FieldString
             , outcome =
                let
                    maybeStartColor : Result String Color.Color
                    maybeStartColor =
                        Color.Convert.hexToColor model.debounced_value
                in
                case maybeStartColor of
                    Ok startColor ->
                        let
                            endColor : Color.Color
                            endColor =
                                Color.Manipulate.scaleRgb
                                    { redScale = Maybe.withDefault 0 (String.toFloat model.debounced_valu1)
                                    , greenScale = Maybe.withDefault 0 (String.toFloat model.debounced_valu2)
                                    , blueScale = Maybe.withDefault 0 (String.toFloat model.debounced_valu3)
                                    , alphaScale = Maybe.withDefault 0 (String.toFloat model.debounced_valu4)
                                    }
                                    startColor
                        in
                        [ text <| "Ok (Color " ++ Color.Convert.colorToHexWithAlpha endColor ++ ") "
                        , Shared.coloredCircle startColor
                        , Shared.arrow
                        , Shared.coloredCircle endColor
                        ]

                    Err _ ->
                        [ text <| Debug.toString maybeStartColor ]
             }
           , { prefix = "Color.Manipulate"
             , function = "mix"
             , signature = "Color -> Color -> Color"
             , description = "Mixes two colors together. This is the same as calling weightedMix with a weight of 0.5."
             , package = "noahzgordon/elm-color-extra"
             , executionBefore = []
             , executionAfter =
                Shared.inputField model Shared.Value Shared.FieldString
                    ++ Shared.space
                    ++ Shared.inputField model Shared.Valu1 Shared.FieldString
             , outcome =
                let
                    maybeStartColor : Result String Color.Color
                    maybeStartColor =
                        Color.Convert.hexToColor model.debounced_value

                    maybeStartColo1 : Result String Color.Color
                    maybeStartColo1 =
                        Color.Convert.hexToColor model.debounced_valu1
                in
                case ( maybeStartColor, maybeStartColo1 ) of
                    ( Ok startColor, Ok startColo1 ) ->
                        let
                            endColor : Color.Color
                            endColor =
                                Color.Manipulate.mix
                                    startColor
                                    startColo1
                        in
                        [ text <| "Ok (Color " ++ Color.Convert.colorToHexWithAlpha endColor ++ ") "
                        , Shared.coloredCircle startColor
                        , Shared.arrow
                        , Shared.coloredCircle startColo1
                        , Shared.arrow
                        , Shared.coloredCircle endColor
                        ]

                    _ ->
                        [ text <| Debug.toString ( maybeStartColor, maybeStartColo1 ) ]
             }
           , { prefix = "Color.Manipulate"
             , function = "weightedMix"
             , signature = "Color -> Color -> Float -> Color"
             , description = "Mixes two colors together."
             , package = "noahzgordon/elm-color-extra"
             , executionBefore = []
             , executionAfter = Shared.inputField model Shared.Value Shared.FieldString ++ Shared.space ++ Shared.inputField model Shared.Valu1 Shared.FieldString ++ Shared.space ++ Shared.inputField model Shared.Valu2 Shared.FieldFloat
             , outcome =
                let
                    maybeStartColor : Result String Color.Color
                    maybeStartColor =
                        Color.Convert.hexToColor model.debounced_value

                    maybeStartColo1 : Result String Color.Color
                    maybeStartColo1 =
                        Color.Convert.hexToColor model.debounced_valu1
                in
                case ( maybeStartColor, maybeStartColo1 ) of
                    ( Ok startColor, Ok startColo1 ) ->
                        let
                            endColor : Color.Color
                            endColor =
                                Color.Manipulate.weightedMix
                                    startColor
                                    startColo1
                                    (Maybe.withDefault 0 (String.toFloat model.debounced_valu2))
                        in
                        [ text <| "Ok (Color " ++ Color.Convert.colorToHexWithAlpha endColor ++ ") "
                        , Shared.coloredCircle startColor
                        , Shared.arrow
                        , Shared.coloredCircle startColo1
                        , Shared.arrow
                        , Shared.coloredCircle endColor
                        ]

                    _ ->
                        [ text <| Debug.toString ( maybeStartColor, maybeStartColo1 ) ]
             }

           -- https://elm.dmy.fr/packages/noahzgordon/elm-color-extra/latest/Color-Accessibility
           , { prefix = "Color.Accessibility"
             , function = "contrastRatio"
             , signature = "Color -> Color -> Float"
             , description = "Get the contrast ratio of two colors represented as a Float."
             , package = "noahzgordon/elm-color-extra"
             , executionBefore = [ text "Result.map2 " ]
             , executionAfter =
                [ text "(Color.Convert.hexToColor " ]
                    ++ Shared.inputField model Shared.Value Shared.FieldString
                    ++ [ text ") (Color.Convert.hexToColor " ]
                    ++ Shared.inputField model Shared.Valu1 Shared.FieldString
                    ++ [ text ")" ]
             , outcome =
                let
                    maybeStartColor : Result String Color.Color
                    maybeStartColor =
                        Color.Convert.hexToColor model.debounced_value

                    maybeStartColo1 : Result String Color.Color
                    maybeStartColo1 =
                        Color.Convert.hexToColor model.debounced_valu1
                in
                [ text <|
                    Debug.toString <|
                        Result.map2
                            Color.Accessibility.contrastRatio
                            maybeStartColor
                            maybeStartColo1
                ]
                    ++ (case ( maybeStartColor, maybeStartColo1 ) of
                            ( Ok startColor, Ok startColo1 ) ->
                                [ text " "
                                , Shared.coloredCircle startColor
                                , text " "
                                , Shared.coloredCircle startColo1
                                ]

                            _ ->
                                []
                       )
             }
           ]
        --
        -- thomasin/elm-path
        --
        ++ [ { prefix = "Path"
             , function = "fromString"
             , signature = "Platform -> String -> Result String Path"
             , description = "Create a path from a string. The path will be normalised and any trailing slashes will be dropped. Note this will not raise an error on invalid path characters."
             , package = "thomasin/elm-path"
             , executionBefore = []
             , executionAfter = [ text "Path.Platform.posix " ] ++ Shared.inputField model Shared.Value Shared.FieldString
             , outcome = [ text <| Debug.toString <| Path.fromString Path.Platform.posix model.debounced_value ]
             }
           , { prefix = "Path"
             , function = "fromString"
             , signature = "Platform -> String -> Result String Path"
             , description = "Create a path from a string. The path will be normalised and any trailing slashes will be dropped. Note this will not raise an error on invalid path characters."
             , package = "thomasin/elm-path"
             , executionBefore = []
             , executionAfter = [ text "Path.Platform.win32 " ] ++ Shared.inputField model Shared.Value Shared.FieldString
             , outcome = [ text <| Debug.toString <| Path.fromString Path.Platform.win32 model.debounced_value ]
             }
           ]
        --
        -- elm/url
        --
        ++ [ { prefix = "Url"
             , function = "fromString"
             , signature = "String -> Maybe Url"
             , description = "Attempt to break a URL up into its components. This is useful in single-page apps when you want to parse certain chunks of a URL to figure out what to show on screen."
             , package = "elm/url"
             , executionBefore = []
             , executionAfter = Shared.inputField model Shared.Value Shared.FieldString
             , outcome = [ text <| Debug.toString <| Url.fromString model.debounced_value ]
             }
           , { prefix = "Url"
             , function = "percentDecode"
             , signature = "String -> Maybe String"
             , description = "Percent-decoding is how the official URI spec “unescapes” special characters. You can still represent a ? even though it is reserved for queries."
             , package = "elm/url"
             , executionBefore = []
             , executionAfter = Shared.inputField model Shared.Value Shared.FieldString
             , outcome = [ text <| Debug.toString (Url.percentDecode model.debounced_value) ]
             }
           , { prefix = "Url"
             , function = "percentEncode"
             , signature = "String -> String"
             , description = "Percent-encoding is how the official URI spec “escapes” special characters. You can still represent a ? even though it is reserved for queries."
             , package = "elm/url"
             , executionBefore = []
             , executionAfter = Shared.inputField model Shared.Value Shared.FieldString
             , outcome = [ text <| Debug.toString (Url.percentEncode model.debounced_value) ]
             }
           ]
        --
        -- elm-community/string-extra
        --
        ++ [ { prefix = "String.Extra"
             , function = "toSentenceCase"
             , signature = "String -> String"
             , description = "Capitalize the first letter of a string."
             , package = "elm-community/string-extra"
             , executionBefore = []
             , executionAfter = Shared.inputField model Shared.Value Shared.FieldString
             , outcome = [ text <| String.Extra.toSentenceCase model.debounced_value ]
             }
           , { prefix = "String.Extra"
             , function = "toTitleCase"
             , signature = "String -> String"
             , description = "Capitalize the first character of each word in a string."
             , package = "elm-community/string-extra"
             , executionBefore = []
             , executionAfter = Shared.inputField model Shared.Value Shared.FieldString
             , outcome = [ text <| String.Extra.toTitleCase model.debounced_value ]
             }
           , { prefix = "String.Extra"
             , function = "decapitalize"
             , signature = "String -> String"
             , description = "Decapitalize the first letter of a string."
             , package = "elm-community/string-extra"
             , executionBefore = []
             , executionAfter = Shared.inputField model Shared.Value Shared.FieldString
             , outcome = [ text <| String.Extra.decapitalize model.debounced_value ]
             }
           , { prefix = "String.Extra"
             , function = "camelize"
             , signature = "String -> String"
             , description = "Convert an underscored or dasherized string to a camelized one."
             , package = "elm-community/string-extra"
             , executionBefore = []
             , executionAfter = Shared.inputField model Shared.Value Shared.FieldString
             , outcome = [ text <| String.Extra.camelize model.debounced_value ]
             }
           , { prefix = "String.Extra"
             , function = "classify"
             , signature = "String -> String"
             , description = "Convert a string to a camelized string starting with an uppercase letter. All non-word characters will be stripped out of the original string."
             , package = "elm-community/string-extra"
             , executionBefore = []
             , executionAfter = Shared.inputField model Shared.Value Shared.FieldString
             , outcome = [ text <| String.Extra.classify model.debounced_value ]
             }
           , { prefix = "String.Extra"
             , function = "underscored"
             , signature = "String -> String"
             , description = "Return a string joined by underscores after separating it by its uppercase characters. Any sequence of spaces or dashes will also be converted to a single underscore. The final string will be lowercased."
             , package = "elm-community/string-extra"
             , executionBefore = []
             , executionAfter = Shared.inputField model Shared.Value Shared.FieldString
             , outcome = [ text <| String.Extra.underscored model.debounced_value ]
             }
           , { prefix = "String.Extra"
             , function = "dasherize"
             , signature = "String -> String"
             , description = "Return a string joined by dashes after separating it by its uppercase characters. Any sequence of spaces or underscores will also be converted to a single dash. The final string will be lowercased."
             , package = "elm-community/string-extra"
             , executionBefore = []
             , executionAfter = Shared.inputField model Shared.Value Shared.FieldString
             , outcome = [ text <| String.Extra.dasherize model.debounced_value ]
             }
           , { prefix = "String.Extra"
             , function = "humanize"
             , signature = "String -> String"
             , description = "Convert an underscored, camelized, or dasherized string into one that can be read by humans. Also remove beginning and ending whitespace, and removes the postfix '_id'. The first character will be capitalized."
             , package = "elm-community/string-extra"
             , executionBefore = []
             , executionAfter = Shared.inputField model Shared.Value Shared.FieldString
             , outcome = [ text <| String.Extra.humanize model.debounced_value ]
             }

           --
           -- https://elm.dmy.fr/packages/robinheghan/murmur3/latest/Murmur3
           --
           , { prefix = "Murmur3"
             , function = "hashString"
             , signature = "Int -> String -> Int"
             , description = "Takes a seed and a string. Produces a MurmurHash3 hash (integer). Given the same seed and string, it will always produce the same hash."
             , package = "robinheghan/murmur3"
             , executionBefore = []
             , executionAfter =
                Shared.inputField model Shared.Valu1 Shared.FieldInt
                    ++ Shared.space
                    ++ Shared.inputField model Shared.Value Shared.FieldString
             , outcome = [ text <| Debug.toString <| Murmur3.hashString (Maybe.withDefault 0 (String.toInt model.debounced_valu1)) model.debounced_value ]
             }
           ]
        --
        -- https://elm.dmy.fr/packages/ktonon/elm-crypto/latest/Crypto-Hash
        --
        ++ [ { prefix = "Crypto.Hash"
             , function = "sha224"
             , signature = "String -> String"
             , description = "Secure Hash Algorithm using 32-bit words and 64 rounds (truncated)."
             , package = "ktonon/elm-crypto"
             , executionBefore = []
             , executionAfter = Shared.inputField model Shared.Value Shared.FieldString
             , outcome = [ text <| Debug.toString <| Crypto.Hash.sha224 model.debounced_value ]
             }
           , { prefix = "Crypto.Hash"
             , function = "sha256"
             , signature = "String -> String"
             , description = "Secure Hash Algorithm using 32-bit words and 64 rounds."
             , package = "ktonon/elm-crypto"
             , executionBefore = []
             , executionAfter = Shared.inputField model Shared.Value Shared.FieldString
             , outcome = [ text <| Debug.toString <| Crypto.Hash.sha256 model.debounced_value ]
             }
           , { prefix = "Crypto.Hash"
             , function = "sha384"
             , signature = "String -> String"
             , description = "Secure Hash Algorithm using 64-bit words and 80 rounds (truncated)."
             , package = "ktonon/elm-crypto"
             , executionBefore = []
             , executionAfter = Shared.inputField model Shared.Value Shared.FieldString
             , outcome = [ text <| Debug.toString <| Crypto.Hash.sha384 model.debounced_value ]
             }
           , { prefix = "Crypto.Hash"
             , function = "sha512"
             , signature = "String -> String"
             , description = "Secure Hash Algorithm using 64-bit words and 80 rounds."
             , package = "ktonon/elm-crypto"
             , executionBefore = []
             , executionAfter = Shared.inputField model Shared.Value Shared.FieldString
             , outcome = [ text <| Debug.toString <| Crypto.Hash.sha512 model.debounced_value ]
             }
           , { prefix = "Crypto.Hash"
             , function = "sha512_224"
             , signature = "String -> String"
             , description = "Secure Hash Algorithm using 64-bit words and 80 rounds (truncated to 224)."
             , package = "ktonon/elm-crypto"
             , executionBefore = []
             , executionAfter = Shared.inputField model Shared.Value Shared.FieldString
             , outcome = [ text <| Debug.toString <| Crypto.Hash.sha512_224 model.debounced_value ]
             }
           , { prefix = "Crypto.Hash"
             , function = "sha512_256"
             , signature = "String -> String"
             , description = "Secure Hash Algorithm using 64-bit words and 80 rounds (truncated to 256)."
             , package = "ktonon/elm-crypto"
             , executionBefore = []
             , executionAfter = Shared.inputField model Shared.Value Shared.FieldString
             , outcome = [ text <| Debug.toString <| Crypto.Hash.sha512_256 model.debounced_value ]
             }
           ]
        --
        -- https://elm.dmy.fr/packages/truqu/elm-md5/latest/MD5
        --
        ++ [ { prefix = "MD5"
             , function = "hex"
             , signature = "String -> String"
             , description = "Given a string of arbitrary length, returns a string of 32 hexadecimal characters (a-f, 0-9) representing the 128-bit MD5 message digest."
             , package = "truqu/elm-md5"
             , executionBefore = []
             , executionAfter = Shared.inputField model Shared.Value Shared.FieldString
             , outcome = [ text <| Debug.toString <| MD5.hex model.debounced_value ]
             }
           ]
        --
        -- https://elm.dmy.fr/packages/billstclair/elm-crypto-string/latest/
        --
        ++ [ { prefix = "Crypto.Strings"
             , function = "encrypt"
             , signature = "Seed -> Passphrase -> Plaintext -> Result String ( Ciphertext, Seed )"
             , description = "Encrypt a string. Encode the output as Base64 with 80-character lines."
             , package = "billstclair/elm-crypto-string"
             , executionBefore = []
             , executionAfter =
                [ text "(Random.initialSeed " ]
                    ++ Shared.inputField model Shared.Valu1 Shared.FieldInt
                    ++ [ text ") " ]
                    ++ Shared.inputField model Shared.Valu2 Shared.FieldString
                    ++ Shared.space
                    ++ Shared.inputField model Shared.Value Shared.FieldString
             , outcome =
                [ text <|
                    Debug.toString <|
                        Crypto.Strings.encrypt
                            (Random.initialSeed (Maybe.withDefault 0 (String.toInt model.debounced_valu1)))
                            model.debounced_valu2
                            model.debounced_value
                ]
             }
           , { prefix = "Crypto.Strings"
             , function = "decrypt"
             , signature = "Passphrase -> Ciphertext -> Result String Plaintext"
             , description = "Decrypt a string created with encrypt."
             , package = "billstclair/elm-crypto-string"
             , executionBefore = []
             , executionAfter =
                Shared.inputField model Shared.Valu2 Shared.FieldString
                    ++ Shared.space
                    ++ Shared.inputField model Shared.Value Shared.FieldString
             , outcome =
                [ text <|
                    Debug.toString <|
                        Crypto.Strings.decrypt
                            model.debounced_valu2
                            model.debounced_value
                ]
             }
           ]
