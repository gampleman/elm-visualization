module Shape.StackTests exposing (offset, series3x3, series4x4)

import Expect
import Shape
import Test exposing (..)


series3x3 =
    [ [ ( 0, 1 ), ( 0, 2 ), ( 0, 1 ) ]
    , [ ( 0, 3 ), ( 0, 4 ), ( 0, 2 ) ]
    , [ ( 0, 5 ), ( 0, 2 ), ( 0, 4 ) ]
    ]


series4x4 =
    [ [ ( 0, 1 ), ( 0, 2 ), ( 0, 1 ), ( 1, 0 ) ]
    , [ ( 0, 3 ), ( 0, 4 ), ( 0, 2 ), ( 2, 0 ) ]
    , [ ( 0, 5 ), ( 0, 2 ), ( 0, 4 ), ( 4, 0 ) ]
    , [ ( 1, 3 ), ( 6, 3 ), ( 1, 5 ), ( 5, 1 ) ]
    ]


offset : Test
offset =
    describe "stack offset"
        [ test "stackOffsetNone produces correct result " <|
            \_ ->
                let
                    expected =
                        [ [ ( 0, 1 ), ( 0, 2 ), ( 0, 1 ) ]
                        , [ ( 1, 4 ), ( 2, 6 ), ( 1, 3 ) ]
                        , [ ( 4, 9 ), ( 6, 8 ), ( 3, 7 ) ]
                        ]
                in
                Shape.stackOffsetNone series3x3
                    |> Expect.equal expected
        , test "stackOffsetSilhouette produces correct result " <|
            \_ ->
                let
                    expected =
                        [ [ ( -4.5, -3.5 ), ( -4, -2 ), ( -3.5, -2.5 ) ]
                        , [ ( -3.5, -0.5 ), ( -2, 2 ), ( -2.5, -0.5 ) ]
                        , [ ( -0.5, 4.5 ), ( 2, 4 ), ( -0.5, 3.5 ) ]
                        ]
                in
                Shape.stackOffsetSilhouette series3x3
                    |> Expect.equal expected
        , test "stackOffsetWiggle prodices correct result " <|
            \_ ->
                let
                    expected =
                        [ [ ( 0, 1 ), ( -1, 1 ), ( 0.7857142857142858, 1.7857142857142858 ) ]
                        , [ ( 1, 4 ), ( 1, 5 ), ( 1.7857142857142858, 3.7857142857142856 ) ]
                        , [ ( 4, 9 ), ( 5, 7 ), ( 3.7857142857142856, 7.785714285714286 ) ]
                        ]
                in
                Shape.stackOffsetWiggle series3x3
                    |> Expect.equal expected
        , test "stackOffsetWiggle prodices correct result for 4 x 4 " <|
            \_ ->
                let
                    expected =
                        [ [ ( 0, 1 ), ( -0.45454545454545453, 1.5454545454545454 ), ( 0.5871212121212122, 1.5871212121212122 ), ( 9.587121212121213, 9.587121212121213 ) ]
                        , [ ( 1, 4 ), ( 1.5454545454545454, 5.545454545454545 ), ( 1.5871212121212122, 3.587121212121212 ), ( 9.587121212121213, 9.587121212121213 ) ]
                        , [ ( 4, 9 ), ( 5.545454545454545, 7.545454545454545 ), ( 3.587121212121212, 7.587121212121212 ), ( 9.587121212121213, 9.587121212121213 ) ]
                        , [ ( 9, 12 ), ( 7.545454545454545, 10.545454545454545 ), ( 7.587121212121212, 12.587121212121211 ), ( 9.587121212121213, 10.587121212121213 ) ]
                        ]
                in
                Shape.stackOffsetWiggle series4x4
                    |> Expect.equal expected
        , test "stackOffsetSilhouette produces correct result for 4 x 4 " <|
            \_ ->
                let
                    expected =
                        [ [ ( -6, -5 ), ( -5.5, -3.5 ), ( -6, -5 ), ( -0.5, -0.5 ) ]
                        , [ ( -5, -2 ), ( -3.5, 0.5 ), ( -5, -3 ), ( -0.5, -0.5 ) ]
                        , [ ( -2, 3 ), ( 0.5, 2.5 ), ( -3, 1 ), ( -0.5, -0.5 ) ]
                        , [ ( 3, 6 ), ( 2.5, 5.5 ), ( 1, 6 ), ( -0.5, 0.5 ) ]
                        ]
                in
                Shape.stackOffsetSilhouette series4x4
                    |> Expect.equal expected
        , test "stackOffsetExpand produces identity when all values are in [0,1]" <|
            \_ ->
                let
                    expected =
                        [ [ ( 0, 0.1 ), ( 0, 0.5 ) ]
                        , [ ( 0.1, 0.5 ), ( 0.5, 0.7 ) ]
                        , [ ( 0.5, 1.0 ), ( 0.7, 1.0 ) ]
                        ]
                in
                Shape.stackOffsetExpand expected
                    |> Expect.equal expected
        , test "stackOffsetExpand produces correct result for 4 x 4 " <|
            \_ ->
                let
                    expected =
                        [ [ ( 0, 0.09090909090909091 ), ( 0, 0.18181818181818182 ), ( 0, 0.09090909090909091 ), ( 0, 0.09090909090909091 ) ], [ ( 0.09090909090909091, 0.36363636363636365 ), ( 0.18181818181818182, 0.5454545454545454 ), ( 0.09090909090909091, 0.2727272727272727 ), ( 0.09090909090909091, 0.2727272727272727 ) ], [ ( 0.36363636363636365, 0.8181818181818181 ), ( 0.5454545454545454, 0.7272727272727273 ), ( 0.2727272727272727, 0.6363636363636364 ), ( 0.2727272727272727, 0.6363636363636364 ) ], [ ( 0.8181818181818181, 1 ), ( 0.7272727272727273, 1 ), ( 0.6363636363636364, 1 ), ( 0.6363636363636364, 1 ) ] ]
                in
                Shape.stackOffsetExpand series4x4
                    |> Expect.equal expected
        , test "stackOffsetDiverging produces correct result for 4 x 4 " <|
            \_ ->
                let
                    expected =
                        [ [ ( 0, 1 ), ( 0, 2 ), ( 0, 1 ), ( -1, 0 ) ]
                        , [ ( 1, 4 ), ( 2, 6 ), ( 1, 3 ), ( -3, -1 ) ]
                        , [ ( 4, 9 ), ( 6, 8 ), ( 3, 7 ), ( -7, -3 ) ]
                        , [ ( 9, 11 ), ( -3, 0 ), ( 7, 11 ), ( -11, -7 ) ]
                        ]
                in
                Shape.stackOffsetDiverging series4x4
                    |> Expect.equal expected
        ]
