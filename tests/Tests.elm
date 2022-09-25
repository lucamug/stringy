module Tests exposing (suite)

import Expect
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
    Test.describe "Testing Soute"
        [ Test.describe "Description 1" <|
            [ Test.test "Test 1" <|
                \_ ->
                    Expect.equal
                        1
                        1
            ]
        ]
