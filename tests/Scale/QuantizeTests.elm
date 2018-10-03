module Scale.QuantizeTests exposing (all)

import Expect
import Fuzz exposing (..)
import Helper exposing (expectAll, isAbout, isBetween)
import Scale
import Test exposing (..)


type Data
    = A
    | B
    | C


all : Test
all =
    describe "Scale.quantize"
        [ test "convert maps a number to a discrete value in the range" <|
            \() ->
                let
                    convert =
                        Scale.convert (Scale.quantize ( 0, [ 1, 2 ] ) ( 0, 1 ))
                in
                expectAll
                    [ convert 0.0 |> Expect.equal 0
                    , convert 0.2 |> Expect.equal 0
                    , convert 0.4 |> Expect.equal 1
                    , convert 0.6 |> Expect.equal 1
                    , convert 0.8 |> Expect.equal 2
                    , convert 1.0 |> Expect.equal 2
                    ]
        , test "clamps input values to the domain" <|
            \() ->
                let
                    convert =
                        Scale.convert (Scale.quantize ( A, [ B, C ] ) ( 0, 1 ))
                in
                expectAll
                    [ convert -0.5 |> Expect.equal A
                    , convert 1.5 |> Expect.equal C
                    ]
        , test "invertExtent maps a value in the range to a domain extent" <|
            \() ->
                let
                    invertExtent =
                        Scale.invertExtent (Scale.quantize ( 0, [ 1, 2, 3 ] ) ( 0, 1 ))
                in
                expectAll
                    [ invertExtent 0 |> Expect.equal (Just ( 0.0, 0.25 ))
                    , invertExtent 1 |> Expect.equal (Just ( 0.25, 0.5 ))
                    , invertExtent 2 |> Expect.equal (Just ( 0.5, 0.75 ))
                    , invertExtent 3 |> Expect.equal (Just ( 0.75, 1.0 ))
                    ]
        , test "rangeExtent returns the first and last value of the list" <|
            \() ->
                let
                    rangeExtent range =
                        Scale.rangeExtent (Scale.quantize range ( 0, 1 ))
                in
                expectAll
                    [ rangeExtent ( A, [ B, C ] ) |> Expect.equal ( A, C )
                    , rangeExtent ( A, [ B ] ) |> Expect.equal ( A, B )
                    , rangeExtent ( A, [] ) |> Expect.equal ( A, A )
                    , rangeExtent ( 0, [ 1, 2, 3, 4 ] ) |> Expect.equal ( 0, 4 )
                    ]
        ]
