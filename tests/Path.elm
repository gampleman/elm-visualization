module Path exposing (all)

import Test exposing (..)
import Expect
import Visualization.Path as Path exposing (..)
import Fuzz exposing (..)
import Helper exposing (isAbout, isBetween, expectAll)


all : Test
all =
    describe "Path"
        [ test "moveTo appends a M command" <|
            \() ->
                expectAll
                    [ path
                        |> moveTo 150 50
                        |> toAttrString
                        |> Expect.equal "M150,50"
                    , path
                        |> moveTo 150 50
                        |> lineTo 200 100
                        |> moveTo 100 50
                        |> toAttrString
                        |> Expect.equal "M150,50L200,100M100,50"
                    ]
        , test "close appends a Z command" <|
            \() ->
                expectAll
                    [ path
                        |> moveTo 150 50
                        |> close
                        |> toAttrString
                        |> Expect.equal "M150,50Z"
                    , path
                        |> moveTo 150 50
                        |> close
                        |> close
                        |> toAttrString
                        |> Expect.equal "M150,50ZZ"
                    ]
        , test "close does nothing if path is empty" <|
            \() ->
                expectAll
                    [ path
                        |> toAttrString
                        |> Expect.equal ""
                    , path
                        |> close
                        |> toAttrString
                        |> Expect.equal ""
                    ]
        , test "lineTo appends an L command" <|
            \() ->
                expectAll
                    [ path
                        |> moveTo 150 50
                        |> lineTo 200 100
                        |> toAttrString
                        |> Expect.equal "M150,50L200,100"
                    ]
          -- ....
        , test "arcTo appends an M command if the path was empty" <|
            \() ->
                path
                    |> arcTo 270 39 163 100 53
                    |> toAttrString
                    |> Expect.equal "M270,39"
        , test "arcTo does nothing if the previous point was ⟨x1,y1⟩" <|
            \() ->
                path
                    |> moveTo 270 39
                    |> arcTo 270 39 163 100 53
                    |> toAttrString
                    |> Expect.equal "M270,39"
        , test "arcTo appends an L command if the previous point, ⟨x1,y1⟩ and ⟨x2,y2⟩ are collinear" <|
            \() ->
                path
                    |> moveTo 100 50
                    |> arcTo 101 51 102 52 10
                    |> toAttrString
                    |> Expect.equal "M100,50L101,51"
        , test "arcTo appends an L command if ⟨x1,y1⟩ and ⟨x2,y2⟩ are coincident" <|
            \() ->
                path
                    |> moveTo 100 50
                    |> arcTo 101 51 101 51 10
                    |> toAttrString
                    |> Expect.equal "M100,50L101,51"
        , test "arcTo appends an L command if the radius is zero" <|
            \() ->
                path
                    |> moveTo 270 182
                    |> arcTo 270 39 163 100 0
                    |> toAttrString
                    |> Expect.equal "M270,182L270,39"
        , test "arcTo appends L and A commands if the arc does not start at the current point" <|
            \() ->
                path
                    |> moveTo 270 182
                    |> arcTo 270 39 163 100 53
                    |> toAttrString
                    |> Expect.equal "M270,182L270,130.222686A53,53,0,0,0,190.750991,84.179342"
        , test "arcTo appends only an A command if the arc starts at the current point" <|
            \() ->
                path
                    |> moveTo 100 100
                    |> arcTo 200 100 200 200 100
                    |> toAttrString
                    |> Expect.equal "M100,100A100,100,0,0,1,200,200"
        , test "arcTo sets the last point to be the end tangent of the arc" <|
            \() ->
                path
                    |> moveTo 100 100
                    |> arcTo 200 100 200 200 50
                    |> arc 150 150 50 0 pi False
                    |> toAttrString
                    |> Expect.equal "M100,100L150,100A50,50,0,0,1,200,150A50,50,0,1,1,100,150"
        , test "rect appends M, h, v, h, and Z commands" <|
            \() ->
                path
                    |> moveTo 150 100
                    |> rect 100 200 50 25
                    |> toAttrString
                    |> Expect.equal "M150,100M100,200h50v25h-50Z"
        ]
