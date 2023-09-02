module StringDistance4 exposing (..)

-- Note: This code is not canonical Elm code style. It is a translation line by line
--       of "Simplest Sift4" at https://siderite.dev/blog/super-fast-and-accurate-string-distance.html
--       We keep as closer as JavaScript as possible for easy debugging.
--
-- Note: We don't use the { a | b = c } syntax becuase is les performant
--       than crating a new dictionary


type alias Constants =
    { s1 : String
    , s2 : String
    , l1 : Int
    , l2 : Int
    , maxOffset : Int
    }


type alias Variables =
    { c1 : Int
    , c2 : Int
    , lcss : Int
    , local_cs : Int
    }


sift4Distance : String -> String -> Int -> Int
sift4Distance s1 s2 maxOffset =
    let
        l1 : Int
        l1 =
            String.length s1

        l2 : Int
        l2 =
            String.length s2
    in
    case ( l1, l2 ) of
        ( 0, _ ) ->
            l2

        ( _, 0 ) ->
            l1

        _ ->
            let
                c : Constants
                c =
                    { s1 = s1
                    , s2 = s2
                    , l1 = l1
                    , l2 = l2
                    , maxOffset = maxOffset
                    }

                v : Variables
                v =
                    --
                    -- JS:
                    --     var c1 = 0; //cursor for string 1
                    --     var c2 = 0; //cursor for string 2
                    --     var lcss = 0; //largest common subsequence
                    --     var local_cs = 0; //local common substring
                    --
                    { c1 = 0
                    , c2 = 0
                    , lcss = 0
                    , local_cs = 0
                    }
            in
            whileLoop c v
                |> (\v2 ->
                        --
                        -- JS:
                        --     lcss += local_cs;
                        --     return Math.round(Math.max(l1, l2) - lcss);
                        --
                        max l1 l2 - (v2.lcss + v2.local_cs)
                   )


whileLoop : Constants -> Variables -> Variables
whileLoop c v =
    --
    -- JS:
    --     while ((c1 < l1) && (c2 < l2)) {..}
    --
    if (v.c1 < c.l1) && (v.c2 < c.l2) then
        whileLoop
            c
            (whileLoopInnerPart c v)

    else
        v


incrementC1C2 : Variables -> Variables
incrementC1C2 v =
    --
    -- JS:
    --     c1++;
    --     c2++;
    --
    { c1 = v.c1 + 1
    , c2 = v.c2 + 1
    , lcss = v.lcss
    , local_cs = v.local_cs
    }


whileLoopInnerPart : Constants -> Variables -> Variables
whileLoopInnerPart c v =
    incrementC1C2
        --
        -- JS:
        --     if (s1.charAt(c1) == s2.charAt(c2)) {..}
        --
        (if charAt v.c1 c.s1 == charAt v.c2 c.s2 then
            --
            -- JS:
            --     local_cs++;
            --
            { c1 = v.c1
            , c2 = v.c2
            , lcss = v.lcss
            , local_cs = v.local_cs + 1
            }

         else
            whileLoopInnerInnerPart c
                --
                -- JS:
                --     lcss += local_cs;
                --     local_cs = 0;
                --
                { c1 = v.c1
                , c2 = v.c2
                , lcss = v.lcss + v.local_cs
                , local_cs = 0
                }
        )


whileLoopInnerInnerPart : Constants -> Variables -> Variables
whileLoopInnerInnerPart c v =
    --
    -- JS:
    --     if (c1 != c2) {..}
    --
    if v.c1 /= v.c2 then
        let
            m : Int
            m =
                max v.c1 v.c2
        in
        forLoop
            { i = 0
            , v =
                --
                -- JS:
                --     c1 = c2 = Math.max(c1, c2); //using max to bypass the need for computer transpositions ('ab' vs 'ba')
                --
                { c1 = m
                , c2 = m
                , lcss = v.lcss
                , local_cs = v.local_cs + 1
                }
            , c = c
            }

    else
        forLoop
            { i = 0
            , v = v
            , c = c
            }


forLoop : { i : Int, v : Variables, c : Constants } -> Variables
forLoop { i, v, c } =
    --
    -- JS:
    --     for (var i = 0; i < maxOffset && (c1 + i < l1 || c2 + i < l2); i++) {..}
    --
    if i < c.maxOffset && (v.c1 + i < c.l1 || v.c2 + i < c.l2) then
        --
        -- JS:
        --     if ((c1 + i < l1) && (s1.charAt(c1 + i) == s2.charAt(c2))) {
        --
        if (v.c1 + 1 < c.l1) && charAt (v.c1 + 1) c.s1 == charAt v.c2 c.s2 then
            --
            -- JS:
            --     c1 += i;
            --     local_cs++;
            --     break;
            --
            { c1 = v.c1 + i
            , c2 = v.c2
            , lcss = v.lcss
            , local_cs = v.local_cs + 1
            }

        else
        --
        -- JS:
        --     if ((c2 + i < l2) && (s1.charAt(c1) == s2.charAt(c2 + i))) {
        --
        if
            (v.c2 + i < c.l2) && (charAt v.c1 c.s1 == charAt (v.c2 + i) c.s2)
        then
            --
            -- JS:
            --     c2 += i;
            --     local_cs++;
            --     break;
            --
            { c1 = v.c1
            , c2 = v.c2 + i
            , lcss = v.lcss
            , local_cs = v.local_cs + 1
            }

        else
            forLoop
                --
                -- JS:
                --     i++
                --
                { i = i + 1
                , v = v
                , c = c
                }

    else
        -- We go out of the for-loop.
        -- We simply return the current variables
        v


charAt : Int -> String -> String
charAt int string =
    String.slice int (int + 1) string



--
--
-- // Sift4 - simplest version
-- // online algorithm to compute the distance between two strings in O(n)
-- // maxOffset is the number of characters to search for matching letters
-- function sift4(s1, s2, maxOffset) {
--     if (!s1 || !s1.length) {
--         if (!s2) {
--             return 0;
--         }
--         return s2.length;
--     }
--
--     if (!s2 || !s2.length) {
--         return s1.length;
--     }
--
--     var l1 = s1.length;
--     var l2 = s2.length;
--
--     var c1 = 0; //cursor for string 1
--     var c2 = 0; //cursor for string 2
--     var lcss = 0; //largest common subsequence
--     var local_cs = 0; //local common substring
--     while ((c1 < l1) && (c2 < l2)) {
--         if (s1.charAt(c1) == s2.charAt(c2)) {
--             console.log("xxx6",
--                 [ [s1,s2]
--                 , s1.charAt(c1)
--                 , s2.charAt(c2)
--                 ]
--             );
--             local_cs++;
--         } else {
--             lcss += local_cs;
--             local_cs = 0;
--             if (c1 != c2) {
--                 c1 = c2 = Math.max(c1, c2); //using max to bypass the need for computer transpositions ('ab' vs 'ba')
--             }
--             console.log("Before for-loop start", {c1 : c1, c2 : c2, lcss : lcss, local_cs: local_cs});
--             i = 0;
--             console.log("Before for-loop start condition", i < maxOffset && (c1 + i < l1 || c2 + i < l2));
--             for (var i = 0; i < maxOffset && (c1 + i < l1 || c2 + i < l2); i++) {
--                 if ((c1 + i < l1) && (s1.charAt(c1 + i) == s2.charAt(c2))) {
--                     console.log("increase c1");
--                     c1 += i;
--                     local_cs++;
--                     break;
--                 }
--                 if ((c2 + i < l2) && (s1.charAt(c1) == s2.charAt(c2 + i))) {
--                     console.log("increase c2");
--                     c2 += i;
--                     local_cs++;
--                     break;
--                 }
--             }
--         }
--         c1++;
--         c2++;
--     }
--     lcss += local_cs;
--     return Math.round(Math.max(l1, l2) - lcss);
-- }
--     x = "abc";
--     y = "ac";
--     console.log("yyy Diff between", x, y, sift4(x,y,5));
--     x = "abcd";
--     y = "acd";
--     console.log("yyy Diff between", x, y, sift4(x,y,5));
--     x = "gma.com";
--     y = "gmail.com";
--     console.log("yyy Diff between", x, y, sift4(x,y,5));
--
--
--
-- main : Html.Html msg
-- main =
--     div []
--         [ text ""
--         , div [] [ text <| Debug.toString <| sift4Distance "abc" "ac" 5 ]
--         , div [] [ text <| Debug.toString <| sift4Distance "gma.com" "gmail.com" 5 ]
--         ]
--
--
