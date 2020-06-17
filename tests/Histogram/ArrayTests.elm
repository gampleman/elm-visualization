module Histogram.ArrayTests exposing (bisectRight)

import Array
import Expect
import Fuzz exposing (int, list)
import Histogram.Array as Array
import Test exposing (Test, describe, fuzz2)


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

                    before =
                        Array.slice 0 bisection sorted |> Array.toList

                    after =
                        Array.slice bisection (Array.length sorted) sorted |> Array.toList

                    allSmaller i =
                        Expect.true ("Expected " ++ String.fromInt i ++ " to be smaller than " ++ Debug.toString after) <| List.all ((<) item) after

                    allGreater i =
                        Expect.true ("Expected " ++ String.fromInt i ++ " to be greater than " ++ Debug.toString before) <| List.all ((>=) item) before
                in
                Expect.all [ allGreater, allSmaller ] item
        ]
