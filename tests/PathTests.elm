module PathTests exposing (all)

import Test exposing (..)
import Expect
import Visualization.Path as Path exposing (..)
import Fuzz exposing (..)
import Helper exposing (isAbout, isBetween, expectAll)
import Regex exposing (HowMany(..))
import String
import Result


pathEqual a b =
    Helper.pathEqual a (toAttrString b)


all : Test
all =
    describe "path "
        [ test "moveTo appends a M command" <|
            \() ->
                expectAll
                    [ begin
                        |> moveTo 150 50
                        |> pathEqual "M150,50"
                    , begin
                        |> moveTo 150 50
                        |> lineTo 200 100
                        |> moveTo 100 50
                        |> pathEqual "M150,50L200,100M100,50"
                    ]
        , test "close appends a Z command" <|
            \() ->
                expectAll
                    [ begin
                        |> moveTo 150 50
                        |> close
                        |> pathEqual "M150,50Z"
                    , begin
                        |> moveTo 150 50
                        |> close
                        |> close
                        |> pathEqual "M150,50ZZ"
                    ]
        , test "close does nothing if path is empty" <|
            \() ->
                expectAll
                    [ begin
                        |> pathEqual ""
                    , begin
                        |> close
                        |> pathEqual ""
                    ]
        , test "lineTo appends an L command" <|
            \() ->
                expectAll
                    [ begin
                        |> moveTo 150 50
                        |> lineTo 200 100
                        |> pathEqual "M150,50L200,100"
                    ]
        , test "quadraticCurveTo appends a Q command" <|
            \() ->
                begin
                    |> moveTo 150 50
                    |> quadraticCurveTo 100 50 200 100
                    |> pathEqual "M150,50Q100,50,200,100"
        , test "bezierCurveTo appends a C command" <|
            \() ->
                begin
                    |> moveTo 150 50
                    |> bezierCurveTo 100 50 0 24 200 100
                    |> pathEqual "M150,50C100,50,0,24,200,100"
        , test "arc may append only an M command if the radius is zero" <|
            \() ->
                begin
                    |> arc 100 100 0 0 (pi / 2) False
                    |> pathEqual "M100,100"
        , test "arc may append only an L command if the radius is zero" <|
            \() ->
                begin
                    |> moveTo 0 0
                    |> arc 100 100 0 0 (pi / 2) False
                    |> pathEqual "M0,0L100,100"
        , test "arc may append an M command if the path was empty" <|
            \() ->
                expectAll
                    [ begin
                        |> arc 100 100 50 0 (pi * 2) False
                        |> pathEqual "M150,100A50,50,0,1,1,50,100A50,50,0,1,1,150,100"
                    , begin
                        |> arc 0 50 50 (-pi / 2) 0 False
                        |> pathEqual "M0,0A50,50,0,0,1,50,50"
                    ]
        , test "arc may append an L command if the arc doesn’t start at the current point" <|
            \() ->
                begin
                    |> moveTo 100 100
                    |> arc 100 100 50 0 (pi * 2) False
                    |> pathEqual "M100,100L150,100A50,50,0,1,1,50,100A50,50,0,1,1,150,100"
        , test "arc appends a single A command if the angle is less than π" <|
            \() ->
                begin
                    |> moveTo 150 100
                    |> arc 100 100 50 0 (pi / 2) False
                    |> pathEqual "M150,100A50,50,0,0,1,100,150"
        , test "arc appends a single A command if the angle is less than τ" <|
            \() ->
                begin
                    |> moveTo 150 100
                    |> arc 100 100 50 0 pi False
                    |> pathEqual "M150,100A50,50,0,1,1,50,100"
        , test "arc appends two A commands if the angle is greater than τ" <|
            \() ->
                begin
                    |> moveTo 150 100
                    |> arc 100 100 50 0 (pi * 2) False
                    |> pathEqual "M150,100A50,50,0,1,1,50,100A50,50,0,1,1,150,100"
        , test "arc 0,π/2 draws a small clockwise arc" <|
            \() ->
                begin
                    |> moveTo 150 100
                    |> arc 100 100 50 0 (pi / 2) False
                    |> pathEqual "M150,100A50,50,0,0,1,100,150"
        , test "arc -π/2, 0 draws a small clockwise arc" <|
            \() ->
                begin
                    |> moveTo 100 50
                    |> arc 100 100 50 (-pi / 2) 0 False
                    |> pathEqual "M100,50A50,50,0,0,1,150,100"
        , test "arc 0, 13π/2 draws a clockwise circle" <|
            \() ->
                begin
                    |> moveTo 150 100
                    |> arc 100 100 50 0 (13 * pi / 2) False
                    |> pathEqual "M150,100A50,50,0,1,1,50,100A50,50,0,1,1,150,100"
        , test "arc 13π/2, 0 draws a big clockwise arc" <|
            \() ->
                begin
                    |> moveTo 100 150
                    |> arc 100 100 50 (13 * pi / 2) 0 False
                    |> pathEqual "M100,150A50,50,0,1,1,150,100"
        , test "arc π/2, 0 draws a big clockwise arc" <|
            \() ->
                begin
                    |> moveTo 100 150
                    |> arc 100 100 50 (pi / 2) 0 False
                    |> pathEqual "M100,150A50,50,0,1,1,150,100"
        , test "arc 3π/2, 0 draws a small clockwise arc" <|
            \() ->
                begin
                    |> moveTo 100 50
                    |> arc 100 100 50 (3 * pi / 2) 0 False
                    |> pathEqual "M100,50A50,50,0,0,1,150,100"
        , test "arc 15π/2, 0 draws a small clockwise arc" <|
            \() ->
                begin
                    |> moveTo 100 50
                    |> arc 100 100 50 (15 * pi / 2) 0 False
                    |> pathEqual "M100,50A50,50,0,0,1,150,100"
        , test "arc 0, π/2 draws a big anticlockwise arc" <|
            \() ->
                begin
                    |> moveTo 150 100
                    |> arc 100 100 50 0 (pi / 2) True
                    |> pathEqual "M150,100A50,50,0,1,0,100,150"
        , test "arc -π/2, 0  draws a big anticlockwise arc" <|
            \() ->
                begin
                    |> moveTo 100 50
                    |> arc 100 100 50 (-pi / 2) 0 True
                    |> pathEqual "M100,50A50,50,0,1,0,150,100"
        , test "arc -13π/2, 0 draws a big anticlockwise arc" <|
            \() ->
                begin
                    |> moveTo 100 50
                    |> arc 100 100 50 (-13 * pi / 2) 0 True
                    |> pathEqual "M100,50A50,50,0,1,0,150,100"
        , test "arc 13π/2 draws a big anticlockwise arc" <|
            \() ->
                begin
                    |> moveTo 150 100
                    |> arc 100 100 50 0 (13 * pi / 2) True
                    |> pathEqual "M150,100A50,50,0,1,0,100,150"
        , test "arc π/2, 0 draws a small anticlockwise arc" <|
            \() ->
                begin
                    |> moveTo 100 150
                    |> arc 100 100 50 (pi / 2) 0 True
                    |> pathEqual "M100,150A50,50,0,0,0,150,100"
        , test "arc 3π/2, 0  draws a big anticlockwise arc" <|
            \() ->
                begin
                    |> moveTo 100 50
                    |> arc 100 100 50 (3 * pi / 2) 0 True
                    |> pathEqual "M100,50A50,50,0,1,0,150,100"
        , test "arcTo appends an M command if the path was empty" <|
            \() ->
                begin
                    |> arcTo 270 39 163 100 53
                    |> pathEqual "M270,39"
        , test "arcTo does nothing if the previous point was ⟨x1,y1⟩" <|
            \() ->
                begin
                    |> moveTo 270 39
                    |> arcTo 270 39 163 100 53
                    |> pathEqual "M270,39"
        , test "arcTo appends an L command if the previous point, ⟨x1,y1⟩ and ⟨x2,y2⟩ are collinear" <|
            \() ->
                begin
                    |> moveTo 100 50
                    |> arcTo 101 51 102 52 10
                    |> pathEqual "M100,50L101,51"
        , test "arcTo appends an L command if ⟨x1,y1⟩ and ⟨x2,y2⟩ are coincident" <|
            \() ->
                begin
                    |> moveTo 100 50
                    |> arcTo 101 51 101 51 10
                    |> pathEqual "M100,50L101,51"
        , test "arcTo appends an L command if the radius is zero" <|
            \() ->
                begin
                    |> moveTo 270 182
                    |> arcTo 270 39 163 100 0
                    |> pathEqual "M270,182L270,39"
        , test "arcTo appends L and A commands if the arc does not start at the current point" <|
            \() ->
                begin
                    |> moveTo 270 182
                    |> arcTo 270 39 163 100 53
                    |> pathEqual "M270,182L270,130.222686A53,53,0,0,0,190.750991,84.179342"
        , test "arcTo appends only an A command if the arc starts at the current point" <|
            \() ->
                begin
                    |> moveTo 100 100
                    |> arcTo 200 100 200 200 100
                    |> pathEqual "M100,100A100,100,0,0,1,200,200"
        , test "arcTo sets the last point to be the end tangent of the arc" <|
            \() ->
                begin
                    |> moveTo 100 100
                    |> arcTo 200 100 200 200 50
                    |> arc 150 150 50 0 pi False
                    |> pathEqual "M100,100L150,100A50,50,0,0,1,200,150A50,50,0,1,1,100,150"
        , test "rect appends M, h, v, h, and Z commands" <|
            \() ->
                begin
                    |> moveTo 150 100
                    |> rect 100 200 50 25
                    |> pathEqual "M150,100M100,200H150V225H100Z"
        , test "DSL and ADT definitions are equivalent" <|
            \() ->
                begin
                    |> moveTo 120 43
                    |> lineTo 34 45
                    |> arcTo 200 100 200 200 50
                    |> close
                    |> pathEqual (toAttrString [ Move ( 120, 43 ), Line ( 34, 45 ), Arc ( 200, 100 ) ( 200, 200 ) 50, Close ])
        ]
