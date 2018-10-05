module HistogramTests exposing (histogram)

import Expect
import Fuzz exposing (..)
import Helper exposing (expectAll, expectAny)
import Histogram
import Test exposing (..)


histogram : Test
histogram =
    describe "histogram"
        [ fuzz (list float) "keeps all the data" <|
            \list ->
                Histogram.float
                    |> Histogram.compute list
                    |> List.foldl (\item sum -> sum + item.length) 0
                    |> Expect.equal (List.length list)
        , fuzz2 float (list float) "computes continous buckets" <|
            \head tail ->
                let
                    minI =
                        Maybe.withDefault head <| List.minimum (head :: tail)
                in
                Histogram.float
                    |> Histogram.compute (head :: tail)
                    |> List.foldl (\item ( fail, minV ) -> ( fail && item.x0 == minV, item.x1 )) ( True, minI )
                    |> Tuple.first
                    |> Expect.true "Expected ranges to be continous"
        ]
