module Scale.QuantileTests exposing (all)

import Expect
import Fuzz exposing (..)
import Helper exposing (expectAll, isAbout, isBetween)
import Scale
import Test exposing (..)


type Data
    = A
    | B
    | C


convert val scale =
    Scale.convert scale val


datum =
    Fuzz.oneOf [ Fuzz.constant A, Fuzz.constant B, Fuzz.constant C ]


randomRange =
    tuple ( datum, list datum )


all : Test
all =
    describe "Scale.quantile"
        [ test "uses the R-7 algorithm to compute quantiles" <|
            \() ->
                Scale.quantile ( 0, [ 1, 2, 3 ] ) [ 3, 6, 7, 8, 8, 10, 13, 15, 16, 20 ]
                    |> Expect.all
                        [ \scale -> List.map (Scale.convert scale) [ 3, 6, 6.9, 7, 7.1 ] |> Expect.equalLists [ 0, 0, 0, 0, 0 ]
                        , \scale -> List.map (Scale.convert scale) [ 8, 8.9 ] |> Expect.equalLists [ 1, 1 ]
                        , \scale -> List.map (Scale.convert scale) [ 9, 9.1, 10, 13 ] |> Expect.equalLists [ 2, 2, 2, 2 ]
                        , \scale -> List.map (Scale.convert scale) [ 14.9, 15, 15.1, 16, 20 ] |> Expect.equalLists [ 3, 3, 3, 3, 3 ]
                        ]
        , test "Scale.quantiles returns the inner quantiles" <|
            \() ->
                Scale.quantile ( 0, [ 1, 2, 3 ] ) [ 3, 6, 7, 8, 8, 10, 13, 15, 16, 20 ]
                    |> Scale.quantiles
                    |> Expect.equalLists [ 7.25, 9, 14.5 ]
        , fuzz randomRange "the cardinality of the range determines the number of quanitles" <|
            \range ->
                Scale.quantile range [ 3, 6, 7, 8, 8, 10, 13, 15, 16, 20 ]
                    |> Scale.quantiles
                    |> List.length
                    |> Expect.equal (List.length (Tuple.second range))
        , test "invertExtent maps the range to the domain" <|
            \() ->
                Scale.quantile ( A, [ B ] ) [ 3, 6, 7, 8, 8, 10, 13, 15, 16, 20 ]
                    |> Expect.all
                        [ \scale -> Scale.invertExtent scale A |> Expect.equal (Just ( 3, 9 ))
                        , \scale -> Scale.invertExtent scale B |> Expect.equal (Just ( 9, 20 ))
                        , \scale -> Scale.invertExtent scale C |> Expect.equal Nothing
                        ]
        , test "invertExtent returns the first match if duplicate values exist in the range" <|
            \() ->
                Scale.quantile ( A, [ B, C, A ] ) [ 3, 6, 7, 8, 8, 10, 13, 15, 16, 20 ]
                    |> Expect.all
                        [ \scale -> Scale.invertExtent scale A |> Expect.equal (Just ( 3, 7.25 ))
                        , \scale -> Scale.invertExtent scale B |> Expect.equal (Just ( 7.25, 9 ))
                        , \scale -> Scale.invertExtent scale C |> Expect.equal (Just ( 9, 14.5 ))
                        ]
        ]
