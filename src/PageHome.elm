module PageHome exposing (list)

import Shared


list : List ( String, Shared.DebouncedValues {} )
list =
    let
        m : Shared.DebouncedValues {}
        m =
            Shared.emptyDebouncedValues
    in
    [ ( "Generate QR codes"
      , { m | debounced_query = "QRCode.fromString", debounced_value = "Hello!" }
      )
    , ( "Convert colors from HEX format to RGBA or HSLA format"
      , { m | debounced_query = "Color.Convert.colorToCss", debounced_value = "fa0" }
      )
    , ( "Convert a string to \"Sentence case\""
      , { m | debounced_query = "String.Extra.toSentenceCase", debounced_value = "a sentence example" }
      )
    , ( "Parse a URL breaking it up into its components"
      , { m | debounced_query = "Url.fromString", debounced_value = "https://example.com/a?b=c#d" }
      )
    , ( "Test a Regular Expression (Regex)"
      , { m | debounced_query = "Regex.find", debounced_value = "^(.+)@(.+)$", debounced_valu1 = "taro@example.com" }
      )
    , ( "Verify that a string contains a correct Regular Expression"
      , { m | debounced_query = "Regex.fromString", debounced_value = "^(.+)@(.+)$" }
      )
    , ( "Format a date using a string template"
      , { m | debounced_query = "Date.format", debounced_value = "EEEE, ddd MMMM y", debounced_valu1 = "738156" }
      )
    , ( "Convert a Posix integer (number of milliseconds from 1 January 1970) to an ISO-8601 date string"
      , { m | debounced_query = "Iso8601.fromTime", debounced_value = "0" }
      )
    , ( "Convert an ISO-8601 date string to a Rata Die (RD, number of days since 1 January 0001)"
      , { m | debounced_query = "Date.fromIsoString", debounced_value = "0001-01-01" }
      )
    , ( "Convert an ISO-8601 date string to Posix integer (number of milliseconds from 1 January 1970)"
      , { m | debounced_query = "Iso8601.toTime", debounced_value = "1970-01-01" }
      )
    , ( "Convert a hexadecimal string such as \"ffff\" to a decimal integer"
      , { m | debounced_query = "Hex.fromString", debounced_value = "ffff" }
      )
    , ( "Convert a decimal integer to a hexadecimal string such as \"ffff\""
      , { m | debounced_query = "Hex.toString", debounced_value = "65535" }
      )
    , ( "Check if a string contains a valid JSON formatted data"
      , { m | debounced_query = "Json.Decode.Generic.json", debounced_value = """{ "a": "b", "c": 23, "d": [1,2,3] }""" }
      )
    , ( "Understand how \"slice\" works"
      , { m | debounced_query = "String.slice", debounced_value = "Snakes on a plane!", debounced_valu1 = "-6", debounced_valu2 = "-1" }
      )
    , ( "Take the most left characters of a string"
      , { m | debounced_query = "String.left", debounced_value = "Snakes on a plane!", debounced_valu1 = "6" }
      )
    , ( "Drop characters from a string"
      , { m | debounced_query = "String.drop", debounced_value = "Snakes on a plane!", debounced_valu1 = "7" }
      )
    , ( "Check if a string contains another string"
      , { m | debounced_query = "String.contains", debounced_value = "Snakes on a plane!", debounced_valu1 = "on" }
      )
    , ( "Get all of the indexes for a substring in another string"
      , { m | debounced_query = "String.indexes", debounced_value = "Mississippi", debounced_valu1 = "ss" }
      )
    , ( "Try to convert a string to an Int or a Float"
      , { m | debounced_query = "String.to", debounced_value = "-22e3" }
      )
    , ( "See all the functions that transform a String from the \"elm/core\" package"
      , { m | debounced_query = "elm/core", debounced_value = "Your string here..." }
      )
    , ( "Manipulate a color (darken, lighten, saturate, desaturate, rotateHue, grayscale, etc.)"
      , { m | debounced_query = "Color.Manipulate", debounced_value = "77c", debounced_valu1 = "0.3", debounced_valu2 = "0.5", debounced_valu3 = "-0.3", debounced_valu4 = "-0.2" }
      )
    , ( "Calculate the contrast ratio between two colors"
      , { m | debounced_query = "Color.Accessibility.contrastRatio", debounced_value = "000", debounced_valu1 = "aaa" }
      )
    , ( "Mix two colors"
      , { m | debounced_query = "Color.Manipulate.mix", debounced_value = "33f", debounced_valu1 = "f20" }
      )
    , ( "Generate several hashes"
      , { m | debounced_query = "hash", debounced_value = "Your string here...", debounced_valu1 = "10" }
      )
    , ( "Convert a string to a 128-bit MD5 message digest"
      , { m | debounced_query = "md5", debounced_value = "Your string here..." }
      )
    , ( "Encrypt and decrypt a string"
      , { m | debounced_query = "Crypto.Strings", debounced_value = "KGBVaeOQHSuIAGaOPdL6yce+M656uloGdRQwLj8KT9w=", debounced_valu1 = "1", debounced_valu2 = "Passphrase" }
      )
    ]
