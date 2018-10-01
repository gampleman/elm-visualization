module ShapeTests exposing (arcTest, centroidTest, pieTest)

import Expect
import Fuzz exposing (..)
import Helper exposing (expectAll, isAbout, isBetween, pathEqual)
import Shape exposing (defaultPieConfig)
import Test exposing (..)


defaultArc =
    { innerRadius = 0
    , outerRadius = 100
    , cornerRadius = 0
    , startAngle = 0
    , endAngle = 2 * pi
    , padAngle = 0
    , padRadius = 0
    }


withInner =
    { defaultArc | innerRadius = 50 }


infinity =
    1 / 0


myRound n =
    toFloat (round (n * 1.0e6)) / 1.0e6



-- This will go away in 0.18


tupleMap f ( a, b ) =
    ( f a, f b )


pieTest : Test
pieTest =
    describe "Pie"
        [ test "pie defaultPieConfig returns default pie data" <|
            \() ->
                Shape.pie defaultPieConfig [ 1, 2, 3 ]
                    |> Expect.equal
                        [ { defaultArc | startAngle = 0, endAngle = pi / 3 }
                        , { defaultArc | startAngle = pi / 3, endAngle = pi }
                        , { defaultArc | startAngle = pi, endAngle = 2 * pi }
                        ]
        , test "pie treats negative numbers as zero" <|
            \() ->
                Shape.pie Shape.defaultPieConfig [ 1, 0, -1 ]
                    |> Expect.equal
                        [ { defaultArc | startAngle = 0, endAngle = 2 * pi }
                        , { defaultArc | startAngle = 0, endAngle = 0 }
                        , { defaultArc | startAngle = 0, endAngle = 0 }
                        ]
        , test "restricts |endAngle - startAngle| to τ" <|
            \() ->
                expectAll
                    [ Shape.pie { defaultPieConfig | endAngle = 7 } [ 1, 2 ]
                        |> Expect.equal
                            [ { defaultArc | startAngle = 0, endAngle = 2 * pi / 3 }
                            , { defaultArc | startAngle = 2 * pi / 3, endAngle = 2 * pi }
                            ]
                    ]
        , test "padAngle = δ observes the specified pad angle" <|
            \() ->
                Shape.pie { defaultPieConfig | padAngle = 0.1 } [ 1, 2, 3 ]
                    |> Expect.equal
                        [ { defaultArc | startAngle = 0, endAngle = pi / 3 + 0.05, padAngle = 0.1 }
                        , { defaultArc | startAngle = pi / 3 + 0.05, endAngle = pi + 0.05 + 0.0000000000000004, padAngle = 0.1 }
                        , { defaultArc | startAngle = pi + 0.05 + 0.0000000000000004, endAngle = 2 * pi, padAngle = 0.1 }
                        ]
        ]


centroidTest : Test
centroidTest =
    describe "Centroid"
        [ test "centroid computes the midpoint of the center line of the arc" <|
            \() ->
                expectAll
                    [ { defaultArc | endAngle = pi }
                        |> Shape.centroid
                        |> tupleMap myRound
                        |> Expect.equal ( 50, 0 )
                    , { defaultArc | endAngle = pi / 2 }
                        |> Shape.centroid
                        |> tupleMap myRound
                        |> Expect.equal ( 35.355339, -35.355339 )
                    , { withInner | endAngle = -pi }
                        |> Shape.centroid
                        |> tupleMap myRound
                        |> Expect.equal ( -75, 0 )
                    , { withInner | endAngle = -pi / 2 }
                        |> Shape.centroid
                        |> tupleMap myRound
                        |> Expect.equal ( -53.033009, -53.033009 )
                    ]
        ]


arcTest : Test
arcTest =
    describe "Arc"
        [ test "arc {innerRadius = 0, outerRadius = 0} renders a point" <|
            \() ->
                expectAll
                    [ { defaultArc | innerRadius = 0, outerRadius = 0 }
                        |> Shape.arc
                        |> pathEqual "M0,0Z"
                    , { defaultArc | innerRadius = 0, outerRadius = 0, endAngle = 0 }
                        |> Shape.arc
                        |> pathEqual "M0,0Z"
                    ]
        , test "arc {innerRadius = 0, outerRadius = r, startAngle = θ₀, endAngle = θ₁ renders a clockwise circle if r > 0 and θ₁ - θ₀ ≥ τ" <|
            \() ->
                expectAll
                    [ defaultArc
                        |> Shape.arc
                        |> pathEqual "M0,-100A100,100,0,1,1,0,100A100,100,0,1,1,0,-100Z"
                    , { defaultArc | endAngle = 3 * pi }
                        |> Shape.arc
                        |> pathEqual "M0,-100A100,100,0,1,1,0,100A100,100,0,1,1,0,-100Z"
                    , { defaultArc | startAngle = -2 * pi, endAngle = 0 }
                        |> Shape.arc
                        |> pathEqual "M0,-100A100,100,0,1,1,0,100A100,100,0,1,1,0,-100Z"
                    , { defaultArc | startAngle = -pi, endAngle = pi }
                        |> Shape.arc
                        |> pathEqual "M0,100A100,100,0,1,1,0,-100A100,100,0,1,1,0,100Z"
                    , { defaultArc | startAngle = -3 * pi, endAngle = 0 }
                        |> Shape.arc
                        |> pathEqual "M0,100A100,100,0,1,1,0,-100A100,100,0,1,1,0,100Z"
                    ]
        , test "arc {innerRadius = 0, outerRadius = r, startAngle = θ₀, endAngle = θ₁ renders an anticlockwise circle if r > 0 and θ₀ - θ₁ ≥ τ" <|
            \() ->
                expectAll
                    [ { defaultArc | endAngle = -2 * pi }
                        |> Shape.arc
                        |> pathEqual "M0,-100A100,100,0,1,0,0,100A100,100,0,1,0,0,-100Z"
                    , { defaultArc | endAngle = -3 * pi }
                        |> Shape.arc
                        |> pathEqual "M0,-100A100,100,0,1,0,0,100A100,100,0,1,0,0,-100Z"
                    , { defaultArc | startAngle = 2 * pi, endAngle = 0 }
                        |> Shape.arc
                        |> pathEqual "M0,-100A100,100,0,1,0,0,100A100,100,0,1,0,0,-100Z"
                    , { defaultArc | startAngle = pi, endAngle = -pi }
                        |> Shape.arc
                        |> pathEqual "M0,100A100,100,0,1,0,0,-100A100,100,0,1,0,0,100Z"
                    , { defaultArc | startAngle = 3 * pi, endAngle = 0 }
                        |> Shape.arc
                        |> pathEqual "M0,100A100,100,0,1,0,0,-100A100,100,0,1,0,0,100Z"
                    ]

        -- Note: The outer ring starts and ends at θ₀, but the inner ring starts and ends at θ₁.
        -- Note: The outer ring is clockwise, but the inner ring is anticlockwise.
        , test "arc {innerRadius = r₀, outerRadius = r₁, startAngle = θ₀, endAngle = θ₁} renders a clockwise annulus if r₀ > 0, r₁ > 0 and θ₀ - θ₁ ≥ τ" <|
            \() ->
                expectAll
                    [ withInner
                        |> Shape.arc
                        |> pathEqual "M0,-100A100,100,0,1,1,0,100A100,100,0,1,1,0,-100M0,-50A50,50,0,1,0,0,50A50,50,0,1,0,0,-50Z"
                    , { withInner | endAngle = 3 * pi }
                        |> Shape.arc
                        |> pathEqual "M0,-100A100,100,0,1,1,0,100A100,100,0,1,1,0,-100M0,50A50,50,0,1,0,0,-50A50,50,0,1,0,0,50Z"
                    , { withInner | startAngle = -2 * pi, endAngle = 0 }
                        |> Shape.arc
                        |> pathEqual "M0,-100A100,100,0,1,1,0,100A100,100,0,1,1,0,-100M0,-50A50,50,0,1,0,0,50A50,50,0,1,0,0,-50Z"
                    , { withInner | startAngle = -pi, endAngle = pi }
                        |> Shape.arc
                        |> pathEqual "M0,100A100,100,0,1,1,0,-100A100,100,0,1,1,0,100M0,50A50,50,0,1,0,0,-50A50,50,0,1,0,0,50Z"
                    , { withInner | startAngle = -3 * pi, endAngle = 0 }
                        |> Shape.arc
                        |> pathEqual "M0,100A100,100,0,1,1,0,-100A100,100,0,1,1,0,100M0,-50A50,50,0,1,0,0,50A50,50,0,1,0,0,-50Z"
                    ]

        -- Note: The outer ring starts and ends at θ₀, but the inner ring starts and ends at θ₁.
        -- Note: The outer ring is anticlockwise, but the inner ring is clockwise.
        , test "arc {innerRadius = r₀, outerRadius = r₁, startAngle = θ₀, endAngle = θ₁} renders an anticlockwise annulus if r₀ > 0, r₁ > 0 and θ₁ - θ₀ ≥ τ" <|
            \() ->
                expectAll
                    [ { withInner | endAngle = -2 * pi }
                        |> Shape.arc
                        |> pathEqual "M0,-100A100,100,0,1,0,0,100A100,100,0,1,0,0,-100M0,-50A50,50,0,1,1,0,50A50,50,0,1,1,0,-50Z"
                    , { withInner | endAngle = -3 * pi }
                        |> Shape.arc
                        |> pathEqual "M0,-100A100,100,0,1,0,0,100A100,100,0,1,0,0,-100M0,50A50,50,0,1,1,0,-50A50,50,0,1,1,0,50Z"
                    , { withInner | startAngle = 2 * pi, endAngle = 0 }
                        |> Shape.arc
                        |> pathEqual "M0,-100A100,100,0,1,0,0,100A100,100,0,1,0,0,-100M0,-50A50,50,0,1,1,0,50A50,50,0,1,1,0,-50Z"
                    , { withInner | startAngle = pi, endAngle = -pi }
                        |> Shape.arc
                        |> pathEqual "M0,100A100,100,0,1,0,0,-100A100,100,0,1,0,0,100M0,50A50,50,0,1,1,0,-50A50,50,0,1,1,0,50Z"
                    , { withInner | startAngle = 3 * pi, endAngle = 0 }
                        |> Shape.arc
                        |> pathEqual "M0,100A100,100,0,1,0,0,-100A100,100,0,1,0,0,100M0,-50A50,50,0,1,1,0,50A50,50,0,1,1,0,-50Z"
                    ]
        , test "arc {innerRadius = 0, outerRadius = r, startAngle = θ₀, endAngle = θ₁} renders a small clockwise sector if r > 0 and π > θ₁ - θ₀ ≥ 0" <|
            \() ->
                expectAll
                    [ { defaultArc | endAngle = pi / 2 }
                        |> Shape.arc
                        |> pathEqual "M0,-100A100,100,0,0,1,100,0L0,0Z"
                    , { defaultArc | startAngle = 2 * pi, endAngle = 5 * pi / 2 }
                        |> Shape.arc
                        |> pathEqual "M0,-100A100,100,0,0,1,100,0L0,0Z"
                    , { defaultArc | startAngle = -pi, endAngle = -pi / 2 }
                        |> Shape.arc
                        |> pathEqual "M0,100A100,100,0,0,1,-100,0L0,0Z"
                    ]
        , test "arc {innerRadius = 0, outerRadius = r, startAngle = θ₀, endAngle = θ₁} renders a small anticlockwise sector if r > 0 and π > θ₀ - θ₁ ≥ 0" <|
            \() ->
                let
                    param =
                        { defaultArc | outerRadius = 100 }
                in
                expectAll
                    [ { param | startAngle = 0, endAngle = -pi / 2 } |> Shape.arc |> pathEqual "M0,-100A100,100,0,0,0,-100,0L0,0Z"
                    , { param | startAngle = -2 * pi, endAngle = -5 * pi / 2 } |> Shape.arc |> pathEqual "M0,-100A100,100,0,0,0,-100,0L0,0Z"
                    , { param | startAngle = pi, endAngle = pi / 2 } |> Shape.arc |> pathEqual "M0,100A100,100,0,0,0,100,0L0,0Z"
                    ]
        , test "arc {innerRadius = 0, outerRadius = r, startAngle = θ₀, endAngle = θ₁} renders a large clockwise sector if r > 0 and τ > θ₁ - θ₀ ≥ π" <|
            \() ->
                let
                    param =
                        { defaultArc | outerRadius = 100 }
                in
                expectAll
                    [ { param | startAngle = 0, endAngle = 3 * pi / 2 } |> Shape.arc |> pathEqual "M0,-100A100,100,0,1,1,-100,0L0,0Z"
                    , { param | startAngle = 2 * pi, endAngle = 7 * pi / 2 } |> Shape.arc |> pathEqual "M0,-100A100,100,0,1,1,-100,0L0,0Z"
                    , { param | startAngle = -pi, endAngle = pi / 2 } |> Shape.arc |> pathEqual "M0,100A100,100,0,1,1,100,0L0,0Z"
                    ]
        , test "arc {innerRadius = 0, outerRadius = r, startAngle = θ₀, endAngle = θ₁} renders a large anticlockwise sector if r > 0 and τ > θ₀ - θ₁ ≥ π" <|
            \() ->
                let
                    param =
                        { defaultArc | outerRadius = 100 }
                in
                expectAll
                    [ { param | startAngle = 0, endAngle = -3 * pi / 2 } |> Shape.arc |> pathEqual "M0,-100A100,100,0,1,0,100,0L0,0Z"
                    , { param | startAngle = -2 * pi, endAngle = -7 * pi / 2 } |> Shape.arc |> pathEqual "M0,-100A100,100,0,1,0,100,0L0,0Z"
                    , { param | startAngle = pi, endAngle = -pi / 2 } |> Shape.arc |> pathEqual "M0,100A100,100,0,1,0,-100,0L0,0Z"
                    ]

        -- Note: The outer ring is clockwise, but the inner ring is anticlockwise.
        , test "arc {innerRadius = r₀, outerRadius = r₁, startAngle = θ₀, endAngle = θ₁} renders a small clockwise annular sector if r₀ > 0, r₁ > 0 and π > θ₁ - θ₀ ≥ 0" <|
            \() ->
                let
                    param =
                        { withInner | outerRadius = 100 }
                in
                expectAll
                    [ { param | startAngle = 0, endAngle = pi / 2 } |> Shape.arc |> pathEqual "M0,-100A100,100,0,0,1,100,0L50,0A50,50,0,0,0,0,-50Z"
                    , { param | startAngle = 2 * pi, endAngle = 5 * pi / 2 } |> Shape.arc |> pathEqual "M0,-100A100,100,0,0,1,100,0L50,0A50,50,0,0,0,0,-50Z"
                    , { param | startAngle = -pi, endAngle = -pi / 2 } |> Shape.arc |> pathEqual "M0,100A100,100,0,0,1,-100,0L-50,0A50,50,0,0,0,0,50Z"
                    ]

        -- Note: The outer ring is anticlockwise, but the inner ring is clockwise.
        , test "arc {innerRadius = r₀, outerRadius = r₁, startAngle = θ₀, endAngle = θ₁} renders a small anticlockwise annular sector if r₀ > 0, r₁ > 0 and π > θ₀ - θ₁ ≥ 0" <|
            \() ->
                let
                    param =
                        { withInner | outerRadius = 100 }
                in
                expectAll
                    [ { param | startAngle = 0, endAngle = -pi / 2 } |> Shape.arc |> pathEqual "M0,-100A100,100,0,0,0,-100,0L-50,0A50,50,0,0,1,0,-50Z"
                    , { param | startAngle = -2 * pi, endAngle = -5 * pi / 2 } |> Shape.arc |> pathEqual "M0,-100A100,100,0,0,0,-100,0L-50,0A50,50,0,0,1,0,-50Z"
                    , { param | startAngle = pi, endAngle = pi / 2 } |> Shape.arc |> pathEqual "M0,100A100,100,0,0,0,100,0L50,0A50,50,0,0,1,0,50Z"
                    ]

        -- Note: The outer ring is clockwise, but the inner ring is anticlockwise.
        , test "arc {innerRadius = r₀, outerRadius = r₁, startAngle = θ₀, endAngle = θ₁} renders a large clockwise annular sector if r₀ > 0, r₁ > 0 and τ > θ₁ - θ₀ ≥ π" <|
            \() ->
                let
                    param =
                        { withInner | outerRadius = 100 }
                in
                expectAll
                    [ { param | startAngle = 0, endAngle = 3 * pi / 2 } |> Shape.arc |> pathEqual "M0,-100A100,100,0,1,1,-100,0L-50,0A50,50,0,1,0,0,-50Z"
                    , { param | startAngle = 2 * pi, endAngle = 7 * pi / 2 } |> Shape.arc |> pathEqual "M0,-100A100,100,0,1,1,-100,0L-50,0A50,50,0,1,0,0,-50Z"
                    , { param | startAngle = -pi, endAngle = pi / 2 } |> Shape.arc |> pathEqual "M0,100A100,100,0,1,1,100,0L50,0A50,50,0,1,0,0,50Z"
                    ]

        -- Note: The outer ring is anticlockwise, but the inner ring is clockwise.
        , test "arc {innerRadius = r₀, outerRadius = r₁, startAngle = θ₀, endAngle = θ₁} renders a large anticlockwise annular sector if r₀ > 0, r₁ > 0 and τ > θ₀ - θ₁ ≥ π" <|
            \() ->
                let
                    param =
                        { withInner | outerRadius = 100 }
                in
                expectAll
                    [ { param | startAngle = 0, endAngle = -3 * pi / 2 } |> Shape.arc |> pathEqual "M0,-100A100,100,0,1,0,100,0L50,0A50,50,0,1,1,0,-50Z"
                    , { param | startAngle = -2 * pi, endAngle = -7 * pi / 2 } |> Shape.arc |> pathEqual "M0,-100A100,100,0,1,0,100,0L50,0A50,50,0,1,1,0,-50Z"
                    , { param | startAngle = pi, endAngle = -pi / 2 } |> Shape.arc |> pathEqual "M0,100A100,100,0,1,0,-100,0L-50,0A50,50,0,1,1,0,50Z"
                    ]
        , test "arc {innerRadius = 0, outerRadius = 0, cornerRadius = r} renders a point" <|
            \() ->
                let
                    param =
                        { defaultArc | outerRadius = 0, cornerRadius = 5 }
                in
                expectAll
                    [ { param | startAngle = 0, endAngle = 2 * pi } |> Shape.arc |> pathEqual "M0,0Z"
                    , { param | startAngle = 0, endAngle = 0 } |> Shape.arc |> pathEqual "M0,0Z"
                    ]
        , test "arc {innerRadius = 0, outerRadius = r, startAngle = θ₀, endAngle = θ₁, cornerRadius = rᵧ} renders a clockwise circle if r > 0 and θ₁ - θ₀ ≥ τ" <|
            \() ->
                let
                    param =
                        { defaultArc | outerRadius = 100, cornerRadius = 5 }
                in
                expectAll
                    [ { param | startAngle = 0, endAngle = 2 * pi } |> Shape.arc |> pathEqual "M0,-100A100,100,0,1,1,0,100A100,100,0,1,1,0,-100Z"
                    , { param | startAngle = 0, endAngle = 3 * pi } |> Shape.arc |> pathEqual "M0,-100A100,100,0,1,1,0,100A100,100,0,1,1,0,-100Z"
                    , { param | startAngle = -2 * pi, endAngle = 0 } |> Shape.arc |> pathEqual "M0,-100A100,100,0,1,1,0,100A100,100,0,1,1,0,-100Z"
                    , { param | startAngle = -pi, endAngle = pi } |> Shape.arc |> pathEqual "M0,100A100,100,0,1,1,0,-100A100,100,0,1,1,0,100Z"
                    , { param | startAngle = -3 * pi, endAngle = 0 } |> Shape.arc |> pathEqual "M0,100A100,100,0,1,1,0,-100A100,100,0,1,1,0,100Z"
                    ]
        , test "arc {innerRadius = 0, outerRadius = r, startAngle = θ₀, endAngle = θ₁, cornerRadius = rᵧ} renders an anticlockwise circle if r > 0 and θ₀ - θ₁ ≥ τ" <|
            \() ->
                let
                    param =
                        { defaultArc | outerRadius = 100, cornerRadius = 5 }
                in
                expectAll
                    [ { param | startAngle = 0, endAngle = -2 * pi } |> Shape.arc |> pathEqual "M0,-100A100,100,0,1,0,0,100A100,100,0,1,0,0,-100Z"
                    , { param | startAngle = 0, endAngle = -3 * pi } |> Shape.arc |> pathEqual "M0,-100A100,100,0,1,0,0,100A100,100,0,1,0,0,-100Z"
                    , { param | startAngle = 2 * pi, endAngle = 0 } |> Shape.arc |> pathEqual "M0,-100A100,100,0,1,0,0,100A100,100,0,1,0,0,-100Z"
                    , { param | startAngle = pi, endAngle = -pi } |> Shape.arc |> pathEqual "M0,100A100,100,0,1,0,0,-100A100,100,0,1,0,0,100Z"
                    , { param | startAngle = 3 * pi, endAngle = 0 } |> Shape.arc |> pathEqual "M0,100A100,100,0,1,0,0,-100A100,100,0,1,0,0,100Z"
                    ]

        -- Note: The outer ring starts and ends at θ₀, but the inner ring starts and ends at θ₁.
        -- Note: The outer ring is clockwise, but the inner ring is anticlockwise.
        , test "arc {innerRadius = r₀, outerRadius = r₁, startAngle = θ₀, endAngle = θ₁, cornerRadius = rᵧ} renders a clockwise annulus if r₀ > 0, r₁ > 0 and θ₀ - θ₁ ≥ τ" <|
            \() ->
                let
                    param =
                        { withInner | outerRadius = 100, cornerRadius = 5 }
                in
                expectAll
                    [ { param | startAngle = 0, endAngle = 2 * pi } |> Shape.arc |> pathEqual "M0,-100A100,100,0,1,1,0,100A100,100,0,1,1,0,-100M0,-50A50,50,0,1,0,0,50A50,50,0,1,0,0,-50Z"
                    , { param | startAngle = 0, endAngle = 3 * pi } |> Shape.arc |> pathEqual "M0,-100A100,100,0,1,1,0,100A100,100,0,1,1,0,-100M0,50A50,50,0,1,0,0,-50A50,50,0,1,0,0,50Z"
                    , { param | startAngle = -2 * pi, endAngle = 0 } |> Shape.arc |> pathEqual "M0,-100A100,100,0,1,1,0,100A100,100,0,1,1,0,-100M0,-50A50,50,0,1,0,0,50A50,50,0,1,0,0,-50Z"
                    , { param | startAngle = -pi, endAngle = pi } |> Shape.arc |> pathEqual "M0,100A100,100,0,1,1,0,-100A100,100,0,1,1,0,100M0,50A50,50,0,1,0,0,-50A50,50,0,1,0,0,50Z"
                    , { param | startAngle = -3 * pi, endAngle = 0 } |> Shape.arc |> pathEqual "M0,100A100,100,0,1,1,0,-100A100,100,0,1,1,0,100M0,-50A50,50,0,1,0,0,50A50,50,0,1,0,0,-50Z"
                    ]

        -- Note: The outer ring starts and ends at θ₀, but the inner ring starts and ends at θ₁.
        -- Note: The outer ring is anticlockwise, but the inner ring is clockwise.
        , test "arc {innerRadius = r₀, outerRadius = r₁, startAngle = θ₀, endAngle = θ₁, cornerRadius = rᵧ} renders an anticlockwise annulus if r₀ > 0, r₁ > 0 and θ₁ - θ₀ ≥ τ" <|
            \() ->
                let
                    param =
                        { withInner | outerRadius = 100, cornerRadius = 5 }
                in
                expectAll
                    [ { param | startAngle = 0, endAngle = -2 * pi } |> Shape.arc |> pathEqual "M0,-100A100,100,0,1,0,0,100A100,100,0,1,0,0,-100M0,-50A50,50,0,1,1,0,50A50,50,0,1,1,0,-50Z"
                    , { param | startAngle = 0, endAngle = -3 * pi } |> Shape.arc |> pathEqual "M0,-100A100,100,0,1,0,0,100A100,100,0,1,0,0,-100M0,50A50,50,0,1,1,0,-50A50,50,0,1,1,0,50Z"
                    , { param | startAngle = 2 * pi, endAngle = 0 } |> Shape.arc |> pathEqual "M0,-100A100,100,0,1,0,0,100A100,100,0,1,0,0,-100M0,-50A50,50,0,1,1,0,50A50,50,0,1,1,0,-50Z"
                    , { param | startAngle = pi, endAngle = -pi } |> Shape.arc |> pathEqual "M0,100A100,100,0,1,0,0,-100A100,100,0,1,0,0,100M0,50A50,50,0,1,1,0,-50A50,50,0,1,1,0,50Z"
                    , { param | startAngle = 3 * pi, endAngle = 0 } |> Shape.arc |> pathEqual "M0,100A100,100,0,1,0,0,-100A100,100,0,1,0,0,100M0,-50A50,50,0,1,1,0,50A50,50,0,1,1,0,-50Z"
                    ]
        , test "arc {innerRadius = 0, outerRadius = r, startAngle = θ₀, endAngle = θ₁, cornerRadius = rᵧ} renders a small clockwise sector if r > 0 and π > θ₁ - θ₀ ≥ 0" <|
            \() ->
                let
                    param =
                        { defaultArc | outerRadius = 100, cornerRadius = 5 }
                in
                expectAll
                    [ { param | startAngle = 0, endAngle = pi / 2 } |> Shape.arc |> pathEqual "M0,-94.868330A5,5,0,0,1,5.263158,-99.861400A100,100,0,0,1,99.861400,-5.263158A5,5,0,0,1,94.868330,0L0,0Z"
                    , { param | startAngle = 2 * pi, endAngle = 5 * pi / 2 } |> Shape.arc |> pathEqual "M0,-94.868330A5,5,0,0,1,5.263158,-99.861400A100,100,0,0,1,99.861400,-5.263158A5,5,0,0,1,94.868330,0L0,0Z"
                    , { param | startAngle = -pi, endAngle = -pi / 2 } |> Shape.arc |> pathEqual "M0,94.868330A5,5,0,0,1,-5.263158,99.861400A100,100,0,0,1,-99.861400,5.263158A5,5,0,0,1,-94.868330,0L0,0Z"
                    ]
        , test "arc {innerRadius = 0, outerRadius = r, startAngle = θ₀, endAngle = θ₁, cornerRadius = rᵧ} renders a small anticlockwise sector if r > 0 and π > θ₀ - θ₁ ≥ 0" <|
            \() ->
                let
                    param =
                        { defaultArc | outerRadius = 100, cornerRadius = 5 }
                in
                expectAll
                    [ { param | startAngle = 0, endAngle = -pi / 2 } |> Shape.arc |> pathEqual "M0,-94.868330A5,5,0,0,0,-5.263158,-99.861400A100,100,0,0,0,-99.861400,-5.263158A5,5,0,0,0,-94.868330,0L0,0Z"
                    , { param | startAngle = -2 * pi, endAngle = -5 * pi / 2 } |> Shape.arc |> pathEqual "M0,-94.868330A5,5,0,0,0,-5.263158,-99.861400A100,100,0,0,0,-99.861400,-5.263158A5,5,0,0,0,-94.868330,0L0,0Z"
                    , { param | startAngle = pi, endAngle = pi / 2 } |> Shape.arc |> pathEqual "M0,94.868330A5,5,0,0,0,5.263158,99.861400A100,100,0,0,0,99.861400,5.263158A5,5,0,0,0,94.868330,0L0,0Z"
                    ]
        , test "arc {innerRadius = 0, outerRadius = r, startAngle = θ₀, endAngle = θ₁, cornerRadius = rᵧ} renders a large clockwise sector if r > 0 and τ > θ₁ - θ₀ ≥ π" <|
            \() ->
                let
                    param =
                        { defaultArc | outerRadius = 100, cornerRadius = 5 }
                in
                expectAll
                    [ { param | startAngle = 0, endAngle = 3 * pi / 2 } |> Shape.arc |> pathEqual "M0,-94.868330A5,5,0,0,1,5.263158,-99.861400A100,100,0,1,1,-99.861400,5.263158A5,5,0,0,1,-94.868330,0L0,0Z"
                    , { param | startAngle = 2 * pi, endAngle = 7 * pi / 2 } |> Shape.arc |> pathEqual "M0,-94.868330A5,5,0,0,1,5.263158,-99.861400A100,100,0,1,1,-99.861400,5.263158A5,5,0,0,1,-94.868330,0L0,0Z"
                    , { param | startAngle = -pi, endAngle = pi / 2 } |> Shape.arc |> pathEqual "M0,94.868330A5,5,0,0,1,-5.263158,99.861400A100,100,0,1,1,99.861400,-5.263158A5,5,0,0,1,94.868330,0L0,0Z"
                    ]
        , test "arc {innerRadius = 0, outerRadius = r, startAngle = θ₀, endAngle = θ₁, cornerRadius = rᵧ} renders a large anticlockwise sector if r > 0 and τ > θ₀ - θ₁ ≥ π" <|
            \() ->
                let
                    param =
                        { defaultArc | outerRadius = 100, cornerRadius = 5 }
                in
                expectAll
                    [ { param | startAngle = 0, endAngle = -3 * pi / 2 } |> Shape.arc |> pathEqual "M0,-94.868330A5,5,0,0,0,-5.263158,-99.861400A100,100,0,1,0,99.861400,5.263158A5,5,0,0,0,94.868330,0L0,0Z"
                    , { param | startAngle = -2 * pi, endAngle = -7 * pi / 2 } |> Shape.arc |> pathEqual "M0,-94.868330A5,5,0,0,0,-5.263158,-99.861400A100,100,0,1,0,99.861400,5.263158A5,5,0,0,0,94.868330,0L0,0Z"
                    , { param | startAngle = pi, endAngle = -pi / 2 } |> Shape.arc |> pathEqual "M0,94.868330A5,5,0,0,0,5.263158,99.861400A100,100,0,1,0,-99.861400,-5.263158A5,5,0,0,0,-94.868330,0L0,0Z"
                    ]

        -- Note: The outer ring is clockwise, but the inner ring is anticlockwise.
        , test "arc {innerRadius = r₀, outerRadius = r₁, startAngle = θ₀, endAngle = θ₁, cornerRadius = rᵧ} renders a small clockwise annular sector if r₀ > 0, r₁ > 0 and π > θ₁ - θ₀ ≥ 0" <|
            \() ->
                let
                    param =
                        { withInner | outerRadius = 100, cornerRadius = 5 }
                in
                expectAll
                    [ { param | startAngle = 0, endAngle = pi / 2 } |> Shape.arc |> pathEqual "M0,-94.868330A5,5,0,0,1,5.263158,-99.861400A100,100,0,0,1,99.861400,-5.263158A5,5,0,0,1,94.868330,0L54.772256,0A5,5,0,0,1,49.792960,-4.545455A50,50,0,0,0,4.545455,-49.792960A5,5,0,0,1,0,-54.772256Z"
                    , { param | startAngle = 2 * pi, endAngle = 5 * pi / 2 } |> Shape.arc |> pathEqual "M0,-94.868330A5,5,0,0,1,5.263158,-99.861400A100,100,0,0,1,99.861400,-5.263158A5,5,0,0,1,94.868330,0L54.772256,0A5,5,0,0,1,49.792960,-4.545455A50,50,0,0,0,4.545455,-49.792960A5,5,0,0,1,0,-54.772256Z"
                    , { param | startAngle = -pi, endAngle = -pi / 2 } |> Shape.arc |> pathEqual "M0,94.868330A5,5,0,0,1,-5.263158,99.861400A100,100,0,0,1,-99.861400,5.263158A5,5,0,0,1,-94.868330,0L-54.772256,0A5,5,0,0,1,-49.792960,4.545455A50,50,0,0,0,-4.545455,49.792960A5,5,0,0,1,0,54.772256Z"
                    ]

        -- Note: The outer ring is anticlockwise, but the inner ring is clockwise.
        , test "arc {innerRadius = r₀, outerRadius = r₁, startAngle = θ₀, endAngle = θ₁, cornerRadius = rᵧ} renders a small anticlockwise annular sector if r₀ > 0, r₁ > 0 and π > θ₀ - θ₁ ≥ 0" <|
            \() ->
                let
                    param =
                        { withInner | outerRadius = 100, cornerRadius = 5 }
                in
                expectAll
                    [ { param | startAngle = 0, endAngle = -pi / 2 } |> Shape.arc |> pathEqual "M0,-94.868330A5,5,0,0,0,-5.263158,-99.861400A100,100,0,0,0,-99.861400,-5.263158A5,5,0,0,0,-94.868330,0L-54.772256,0A5,5,0,0,0,-49.792960,-4.545455A50,50,0,0,1,-4.545455,-49.792960A5,5,0,0,0,0,-54.772256Z"
                    , { param | startAngle = -2 * pi, endAngle = -5 * pi / 2 } |> Shape.arc |> pathEqual "M0,-94.868330A5,5,0,0,0,-5.263158,-99.861400A100,100,0,0,0,-99.861400,-5.263158A5,5,0,0,0,-94.868330,0L-54.772256,0A5,5,0,0,0,-49.792960,-4.545455A50,50,0,0,1,-4.545455,-49.792960A5,5,0,0,0,0,-54.772256Z"
                    , { param | startAngle = pi, endAngle = pi / 2 } |> Shape.arc |> pathEqual "M0,94.868330A5,5,0,0,0,5.263158,99.861400A100,100,0,0,0,99.861400,5.263158A5,5,0,0,0,94.868330,0L54.772256,0A5,5,0,0,0,49.792960,4.545455A50,50,0,0,1,4.545455,49.792960A5,5,0,0,0,0,54.772256Z"
                    ]

        -- Note: The outer ring is clockwise, but the inner ring is anticlockwise.
        , test "arc {innerRadius = r₀, outerRadius = r₁, startAngle = θ₀, endAngle = θ₁, cornerRadius = rᵧ} renders a large clockwise annular sector if r₀ > 0, r₁ > 0 and τ > θ₁ - θ₀ ≥ π" <|
            \() ->
                let
                    param =
                        { withInner | outerRadius = 100, cornerRadius = 5 }
                in
                expectAll
                    [ { param | startAngle = 0, endAngle = 3 * pi / 2 } |> Shape.arc |> pathEqual "M0,-94.868330A5,5,0,0,1,5.263158,-99.861400A100,100,0,1,1,-99.861400,5.263158A5,5,0,0,1,-94.868330,0L-54.772256,0A5,5,0,0,1,-49.792960,4.545455A50,50,0,1,0,4.545455,-49.792960A5,5,0,0,1,0,-54.772256Z"
                    , { param | startAngle = 2 * pi, endAngle = 7 * pi / 2 } |> Shape.arc |> pathEqual "M0,-94.868330A5,5,0,0,1,5.263158,-99.861400A100,100,0,1,1,-99.861400,5.263158A5,5,0,0,1,-94.868330,0L-54.772256,0A5,5,0,0,1,-49.792960,4.545455A50,50,0,1,0,4.545455,-49.792960A5,5,0,0,1,0,-54.772256Z"
                    , { param | startAngle = -pi, endAngle = pi / 2 } |> Shape.arc |> pathEqual "M0,94.868330A5,5,0,0,1,-5.263158,99.861400A100,100,0,1,1,99.861400,-5.263158A5,5,0,0,1,94.868330,0L54.772256,0A5,5,0,0,1,49.792960,-4.545455A50,50,0,1,0,-4.545455,49.792960A5,5,0,0,1,0,54.772256Z"
                    ]

        -- Note: The outer ring is anticlockwise, but the inner ring is clockwise.
        , test "arc {innerRadius = r₀, outerRadius = r₁, startAngle = θ₀, endAngle = θ₁, cornerRadius = rᵧ} renders a large anticlockwise annular sector if r₀ > 0, r₁ > 0 and τ > θ₀ - θ₁ ≥ π" <|
            \() ->
                let
                    param =
                        { withInner | outerRadius = 100, cornerRadius = 5 }
                in
                expectAll
                    [ { param | startAngle = 0, endAngle = -3 * pi / 2 } |> Shape.arc |> pathEqual "M0,-94.868330A5,5,0,0,0,-5.263158,-99.861400A100,100,0,1,0,99.861400,5.263158A5,5,0,0,0,94.868330,0L54.772256,0A5,5,0,0,0,49.792960,4.545455A50,50,0,1,1,-4.545455,-49.792960A5,5,0,0,0,0,-54.772256Z"
                    , { param | startAngle = -2 * pi, endAngle = -7 * pi / 2 } |> Shape.arc |> pathEqual "M0,-94.868330A5,5,0,0,0,-5.263158,-99.861400A100,100,0,1,0,99.861400,5.263158A5,5,0,0,0,94.868330,0L54.772256,0A5,5,0,0,0,49.792960,4.545455A50,50,0,1,1,-4.545455,-49.792960A5,5,0,0,0,0,-54.772256Z"
                    , { param | startAngle = pi, endAngle = -pi / 2 } |> Shape.arc |> pathEqual "M0,94.868330A5,5,0,0,0,5.263158,99.861400A100,100,0,1,0,-99.861400,-5.263158A5,5,0,0,0,-94.868330,0L-54.772256,0A5,5,0,0,0,-49.792960,-4.545455A50,50,0,1,1,4.545455,49.792960A5,5,0,0,0,0,54.772256Z"
                    ]
        , test "arc {innerRadius = r₀, outerRadius = r₁, cornerRadius = rᵧ} restricts rᵧ to |r₁ - r₀| / 2" <|
            \() ->
                let
                    param =
                        { defaultArc | cornerRadius = infinity, startAngle = 0, endAngle = pi / 2 }
                in
                expectAll
                    [ { param | innerRadius = 90, outerRadius = 100 } |> Shape.arc |> pathEqual "M0,-94.868330A5,5,0,0,1,5.263158,-99.861400A100,100,0,0,1,99.861400,-5.263158A5,5,0,0,1,94.868330,0L94.868330,0A5,5,0,0,1,89.875260,-4.736842A90,90,0,0,0,4.736842,-89.875260A5,5,0,0,1,0,-94.868330Z"
                    , { param | innerRadius = 100, outerRadius = 90 } |> Shape.arc |> pathEqual "M0,-94.868330A5,5,0,0,1,5.263158,-99.861400A100,100,0,0,1,99.861400,-5.263158A5,5,0,0,1,94.868330,0L94.868330,0A5,5,0,0,1,89.875260,-4.736842A90,90,0,0,0,4.736842,-89.875260A5,5,0,0,1,0,-94.868330Z"
                    ]
        , test "arc {innerRadius = r₀, outerRadius = r₁, cornerRadius = rᵧ} merges adjacent corners when rᵧ is relatively large" <|
            \() ->
                let
                    param =
                        { defaultArc | cornerRadius = infinity, startAngle = 0, endAngle = pi / 2 }
                in
                expectAll
                    [ { param | innerRadius = 10, outerRadius = 100 } |> Shape.arc |> pathEqual "M0,-41.421356A41.421356,41.421356,0,1,1,41.421356,0L24.142136,0A24.142136,24.142136,0,0,1,0,-24.142136Z"

                    --, { param | innerRadius = 100, outerRadius = 10 } |> Shape.arc |> pathEqual "M0,-41.421356A41.421356,41.421356,0,1,1,41.421356,0L24.142136,0A24.142136,24.142136,0,0,1,0,-24.142136Z"
                    ]
        , test "arc {innerRadius = 0, outerRadius = 0, startAngle = 0, endAngle = τ, padAngle = δ} does not pad a point" <|
            \() ->
                let
                    param =
                        { defaultArc | outerRadius = 0, startAngle = 0, endAngle = 2 * pi, padAngle = 0.1 }
                in
                param
                    |> Shape.arc
                    |> pathEqual "M0,0Z"
        , test "arc {innerRadius = 0, outerRadius = r, startAngle = 0, endAngle = τ, padAngle = δ} does not pad a circle" <|
            \() ->
                let
                    param =
                        { defaultArc | outerRadius = 100, startAngle = 0, endAngle = 2 * pi, padAngle = 0.1 }
                in
                param |> Shape.arc |> pathEqual "M0,-100A100,100,0,1,1,0,100A100,100,0,1,1,0,-100Z"
        , test "arc {innerRadius = r₀, outerRadius = r₁, startAngle = 0, endAngle = τ, padAngle = δ} does not pad an annulus" <|
            \() ->
                let
                    param =
                        { withInner | outerRadius = 100, startAngle = 0, endAngle = 2 * pi, padAngle = 0.1 }
                in
                param |> Shape.arc |> pathEqual "M0,-100A100,100,0,1,1,0,100A100,100,0,1,1,0,-100M0,-50A50,50,0,1,0,0,50A50,50,0,1,0,0,-50Z"
        , test "arc {innerRadius = 0, outerRadius = r, startAngle = θ₀, endAngle = θ₁, padAngle = δ} pads the outside of a circular sector" <|
            \() ->
                let
                    param =
                        { defaultArc | outerRadius = 100, startAngle = 0, endAngle = pi / 2, padAngle = 0.1 }
                in
                param |> Shape.arc |> pathEqual "M4.997917,-99.875026A100,100,0,0,1,99.875026,-4.997917L0,0Z"
        , test "arc {innerRadius = r₀, outerRadius = r₁, startAngle = θ₀, endAngle = θ₁, padAngle = δ} pads an annular sector" <|
            \() ->
                let
                    param =
                        { withInner | outerRadius = 100, startAngle = 0, endAngle = pi / 2, padAngle = 0.1 }
                in
                param |> Shape.arc |> pathEqual "M5.587841,-99.843758A100,100,0,0,1,99.843758,-5.587841L49.686779,-5.587841A50,50,0,0,0,5.587841,-49.686779Z"
        , test "arc {innerRadius = r₀, outerRadius = r₁, startAngle = θ₀, endAngle = θ₁, padAngle = δ} may collapse the inside of an annular sector" <|
            \() ->
                { defaultArc | innerRadius = 10, outerRadius = 100, startAngle = 0, endAngle = pi / 2, padAngle = 0.2 } |> Shape.arc |> pathEqual "M10.033134,-99.495408A100,100,0,0,1,99.495408,-10.033134L7.071068,-7.071068Z"
        , test "arc {innerRadius = 0, outerRadius = r, startAngle = θ₀, endAngle = θ₁, padAngle = δ, cornerRadius = rᵧ} rounds and pads a circular sector" <|
            \() ->
                let
                    param =
                        { defaultArc | outerRadius = 100, startAngle = 0, endAngle = pi / 2, padAngle = 0.1, cornerRadius = 10 }
                in
                param |> Shape.arc |> pathEqual "M4.470273,-89.330939A10,10,0,0,1,16.064195,-98.701275A100,100,0,0,1,98.701275,-16.064195A10,10,0,0,1,89.330939,-4.470273L0,0Z"
        , test "arc {innerRadius = r₀, outerRadius = r₁, startAngle = θ₀, endAngle = θ₁, padAngle = δ, cornerRadius = rᵧ} rounds and pads an annular sector" <|
            \() ->
                let
                    param =
                        { withInner | outerRadius = 100, startAngle = 0, endAngle = pi / 2, padAngle = 0.1, cornerRadius = 10 }
                in
                param |> Shape.arc |> pathEqual "M5.587841,-88.639829A10,10,0,0,1,17.319823,-98.488698A100,100,0,0,1,98.488698,-17.319823A10,10,0,0,1,88.639829,-5.587841L57.939790,-5.587841A10,10,0,0,1,48.283158,-12.989867A50,50,0,0,0,12.989867,-48.283158A10,10,0,0,1,5.587841,-57.939790Z"
        , test "arc {innerRadius = r₀, outerRadius = r₁, startAngle = θ₀, endAngle = θ₁, padAngle = δ, cornerRadius = rᵧ} rounds and pads a collapsed annular sector" <|
            \() ->
                { defaultArc | innerRadius = 10, endAngle = pi / 2, padAngle = 0.2, cornerRadius = 10 } |> Shape.arc |> pathEqual "M9.669396,-88.145811A10,10,0,0,1,21.849183,-97.583878A100,100,0,0,1,97.583878,-21.849183A10,10,0,0,1,88.145811,-9.669396L7.071068,-7.071068Z"
        ]
