module Histogram.ArrayTests exposing (bisectRight)

import Array exposing (Array)
import Array.Hamt as Hamt
import Expect
import Fuzz exposing (..)
import Helper exposing (expectAll, expectAny)
import Test exposing (..)
import Visualization.Histogram.Array as Array


bisectRight : Test
bisectRight =
    describe "bisectRight"
        [ fuzz2 int (list int) "returns the correct insertion point for maintaining sort" <|
            \item list ->
                let
                    sorted =
                        list
                            |> List.sort
                            |> Array.fromList

                    bisection =
                        Array.bisectRight item sorted Nothing

                    -- HAMT is needed in 0.18 because Core array crashes
                    hamtSorted =
                        list
                            |> List.sort
                            |> Hamt.fromList

                    before =
                        Hamt.slice 0 bisection hamtSorted |> Hamt.toList

                    after =
                        Hamt.slice bisection (Hamt.length hamtSorted) hamtSorted |> Hamt.toList

                    allSmaller i =
                        Expect.true ("Expected " ++ toString i ++ " to be smaller than " ++ toString after) <| List.all ((<=) item) after

                    allGreater i =
                        Expect.true ("Expected " ++ toString i ++ " to be greater than " ++ toString before) <| List.all ((>) item) before
                in
                    Expect.all [ allGreater, allSmaller ] item
        ]
