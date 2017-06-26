module ListTests exposing (tickStep, range)

import Expect
import Fuzz exposing (..)
import Helper exposing (expectAll, expectAny)
import Test exposing (..)
import Visualization.List as VList


tickStep : Test
tickStep =
    describe "tickStep"
        [ fuzz3 float float int "tickStep start stop count returns -tickStep stop start count" <|
            \start stop count ->
                expectAny
                    [ VList.tickStep start stop count
                        |> Expect.equal (-(VList.tickStep stop start count))
                    , Expect.true "was not NaN" <| isNaN <| VList.tickStep start stop count
                    ]
        , test "returns approximately count + 1 tickStep when start < stop" <|
            \() ->
                expectAll
                    [ VList.tickStep 0 1 10
                        |> Expect.equal 0.1
                    , VList.tickStep 0 1 9
                        |> Expect.equal 0.1
                    , VList.tickStep 0 1 8
                        |> Expect.equal 0.1
                    , VList.tickStep 0 1 7
                        |> Expect.equal 0.2
                    , VList.tickStep 0 1 6
                        |> Expect.equal 0.2
                    , VList.tickStep 0 1 5
                        |> Expect.equal 0.2
                    , VList.tickStep 0 1 4
                        |> Expect.equal 0.2
                    , VList.tickStep 0 1 3
                        |> Expect.equal 0.5
                    , VList.tickStep 0 1 2
                        |> Expect.equal 0.5
                    , VList.tickStep 0 1 1
                        |> Expect.equal 1.0
                    , VList.tickStep 0 10 10
                        |> Expect.equal 1
                    , VList.tickStep 0 10 9
                        |> Expect.equal 1
                    , VList.tickStep 0 10 8
                        |> Expect.equal 1
                    , VList.tickStep 0 10 7
                        |> Expect.equal 2
                    , VList.tickStep 0 10 6
                        |> Expect.equal 2
                    , VList.tickStep 0 10 5
                        |> Expect.equal 2
                    , VList.tickStep 0 10 4
                        |> Expect.equal 2
                    , VList.tickStep 0 10 3
                        |> Expect.equal 5
                    , VList.tickStep 0 10 2
                        |> Expect.equal 5
                    , VList.tickStep 0 10 1
                        |> Expect.equal 10
                    , VList.tickStep -10 10 10
                        |> Expect.equal 2
                    , VList.tickStep -10 10 9
                        |> Expect.equal 2
                    , VList.tickStep -10 10 8
                        |> Expect.equal 2
                    , VList.tickStep -10 10 7
                        |> Expect.equal 2
                    , VList.tickStep -10 10 6
                        |> Expect.equal 5
                    , VList.tickStep -10 10 5
                        |> Expect.equal 5
                    , VList.tickStep -10 10 4
                        |> Expect.equal 5
                    , VList.tickStep -10 10 3
                        |> Expect.equal 5
                    , VList.tickStep -10 10 2
                        |> Expect.equal 10
                    , VList.tickStep -10 10 1
                        |> Expect.equal 20
                    ]
        ]


range : Test
range =
    describe "range start stop step"
        [ test "returns [start, start + step, start + 2 * step, … stop - step]" <|
            \() ->
                expectAll
                    [ VList.range 0 5 1
                        |> Expect.equal [ 0, 1, 2, 3, 4 ]
                    , VList.range 0 5 2
                        |> Expect.equal [ 0, 2, 4 ]
                    , VList.range 2 5 2
                        |> Expect.equal [ 2, 4 ]
                    , VList.range -1 3 2
                        |> Expect.equal [ -1, 1 ]
                    ]
        , test "allows a negative step" <|
            \() ->
                expectAll
                    [ VList.range 5 0 -1
                        |> Expect.equal [ 5, 4, 3, 2, 1 ]
                    , VList.range 5 0 -2
                        |> Expect.equal [ 5, 3, 1 ]
                    , VList.range 5 2 -2
                        |> Expect.equal [ 5, 3 ]
                    , VList.range 3 -1 -2
                        |> Expect.equal [ 3, 1 ]
                    ]
        , test "returns an empty array if start >= stop and step > 0" <|
            \() ->
                expectAll
                    [ VList.range 5 5 2
                        |> Expect.equal []
                    , VList.range 6 5 2
                        |> Expect.equal []
                    , VList.range 10 10 1
                        |> Expect.equal []
                    , VList.range 10 10 0.5
                        |> Expect.equal []
                    , VList.range 0 0 1
                        |> Expect.equal []
                    , VList.range 0 0 0.5
                        |> Expect.equal []
                    , VList.range 20 10 2
                        |> Expect.equal []
                    , VList.range 20 10 1
                        |> Expect.equal []
                    , VList.range 20 10 0.5
                        |> Expect.equal []
                    ]
        , test "returns an empty array if start >= stop and step < 0" <|
            \() ->
                expectAll
                    [ VList.range 5 5 -2
                        |> Expect.equal []
                    , VList.range 5 6 -2
                        |> Expect.equal []
                    , VList.range 10 10 -1
                        |> Expect.equal []
                    , VList.range 10 10 -0.5
                        |> Expect.equal []
                    , VList.range 0 0 -1
                        |> Expect.equal []
                    , VList.range 0 0 -0.5
                        |> Expect.equal []
                    , VList.range 10 20 -2
                        |> Expect.equal []
                    , VList.range 10 20 -1
                        |> Expect.equal []
                    , VList.range 10 20 -0.5
                        |> Expect.equal []
                    ]
        , test "returns an empty array if step is zero" <|
            \() ->
                VList.range 0 5 0
                    |> Expect.equal []
        , fuzz2 (intRange -1000 1000) (intRange -1000 1000) "behaves the same as List.range for ints" <|
            \a b ->
                VList.range (min a b) (max a b) 1
                    -- account for List.range being inclusive
                    |> (flip List.append) [ max a b ]
                    |> Expect.equal (List.range (min a b) (max a b))

        -- These tests are waiting for the better implementation, but that will be a breaking change.
        -- , test "returns exactly [start + step * i, …] for fractional steps" <|
        --     \() ->
        --         expectAll
        --             [ VList.range 0 0.5 0.1
        --                 |> Expect.equal [ 0 + 0.1 * 0, 0 + 0.1 * 1, 0 + 0.1 * 2, 0 + 0.1 * 3, 0 + 0.1 * 4 ]
        --             , VList.range 0.5 0 -0.1
        --                 |> Expect.equal [ 0.5 - 0.1 * 0, 0.5 - 0.1 * 1, 0.5 - 0.1 * 2, 0.5 - 0.1 * 3, 0.5 - 0.1 * 4 ]
        --             , VList.range -2 -1.2 0.1
        --                 |> Expect.equal [ -2 + 0.1 * 0, -2 + 0.1 * 1, -2 + 0.1 * 2, -2 + 0.1 * 3, -2 + 0.1 * 4, -2 + 0.1 * 5, -2 + 0.1 * 6, -2 + 0.1 * 7 ]
        --             , VList.range -1.2 -2 -0.1
        --                 |> Expect.equal [ -1.2 - 0.1 * 0, -1.2 - 0.1 * 1, -1.2 - 0.1 * 2, -1.2 - 0.1 * 3, -1.2 - 0.1 * 4, -1.2 - 0.1 * 5, -1.2 - 0.1 * 6, -1.2 - 0.1 * 7 ]
        --             ]
        -- , test "returns exactly [start + step * i, …] for very small fractional steps" <|
        --     \() ->
        --         expectAll
        --             [ VList.range 2.1e-31 5.0e-31 1.1e-31
        --                 |> Expect.equal [ 2.1e-31 + 1.1e-31 * 0, 2.1e-31 + 1.1e-31 * 1, 2.1e-31 + 1.1e-31 * 2 ]
        --             , VList.range 5.0e-31 2.1e-31 -1.1e-31
        --                 |> Expect.equal [ 5.0e-31 - 1.1e-31 * 0, 5.0e-31 - 1.1e-31 * 1, 5.0e-31 - 1.1e-31 * 2 ]
        --             ]
        , test "returns exactly [start + step * i, …] for very large fractional steps" <|
            \() ->
                expectAll
                    [ VList.range 1.0e300 2.0e300 3.0e299
                        |> Expect.equal [ 1.0e300 + 3.0e299 * 0, 1.0e300 + 3.0e299 * 1, 1.0e300 + 3.0e299 * 2, 1.0e300 + 3.0e299 * 3 ]
                    , VList.range 2.0e300 1.0e300 -3.0e299
                        |> Expect.equal [ 2.0e300 - 3.0e299 * 0, 2.0e300 - 3.0e299 * 1, 2.0e300 - 3.0e299 * 2, 2.0e300 - 3.0e299 * 3 ]
                    ]
        ]
