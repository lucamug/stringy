module Tests exposing (suite)

import Expect
import StringDistance4 exposing (..)
import Test



-- To run these tests:
--
-- cmd/test
--
-- Check out http://package.elm-lang.org/packages/elm-community/elm-test/latest
-- to learn more about testing in Elm!
--
-- To run a sub-set of tests use "only <|"


suite : Test.Test
suite =
    Test.describe "Testing Soite"
        [ Test.describe "Tests"
            (List.indexedMap
                (\index ( a, b, r ) ->
                    Test.test ("Test " ++ String.fromInt index) <| \_ -> Expect.equal (sift4Distance a b 5) r
                )
                list
            )
        ]


list =
    [ ( "", "abc", 3 )
    , ( "abc", "", 3 )
    , ( "abc", "abc", 0 )
    , ( "a", "ab", 1 )
    , ( "a", "b", 1 )
    , ( "a", "bc", 2 )
    , ( "b", "abc", 2 )
    , ( "bc", "abcd", 2 )
    , ( "ab", "abcd", 2 )
    , ( "ac", "abcd", 2 )
    , ( "ad", "abcd", 2 )
    , ( "dcba", "abcd", 3 )
    , ( "123@gma.com", "123@gmail.com", 2 )
    ]
