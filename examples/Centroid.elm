module Centroid exposing (main)

{-| The black dots show the midpoint computed by `centroid`. Note that this is
not the geometric center of the arc, which may be outside the arc; this method
is merely a convenience for positioning labels.
-}

import Array exposing (Array)
import LowLevel.Command exposing (arcTo, clockwise, largestArc, moveTo)
import Path
import SubPath exposing (SubPath)
import Svg.Attributes exposing (fill, stroke)
import TypedSvg exposing (circle, g, svg)
import TypedSvg.Attributes exposing (transform)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, width)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Transform(..))
import Visualization.Shape as Shape exposing (Arc, defaultPieConfig)


w : Float
w =
    990


h : Float
h =
    504


colors : Array String
colors =
    Array.fromList
        [ "rgba(31, 119, 180, 0.5)"
        , "rgba(255, 127, 14, 0.5)"
        , "rgba(44, 159, 44, 0.5)"
        , "rgba(214, 39, 40, 0.5)"
        , "rgba(148, 103, 189, 0.5)"
        , "rgba(140, 86, 75, 0.5)"
        , "rgba(227, 119, 194, 0.5)"
        , "rgba(128, 128, 128, 0.5)"
        , "rgba(188, 189, 34, 0.5)"
        , "rgba(23, 190, 207, 0.5)"
        ]


radius : Float
radius =
    min (w / 2) h / 2 - 10


dot : SubPath
dot =
    SubPath.with (moveTo ( 5, 5 ))
        [ arcTo
            [ { radii = ( 5, 5 )
              , xAxisRotate = 0
              , arcFlag = largestArc
              , direction = clockwise
              , target = ( 5, 5 )
              }
            ]
        ]


circular : List Arc -> Svg msg
circular arcs =
    let
        makeSlice index datum =
            Path.element (Shape.arc datum) [ fill (Maybe.withDefault "#000" <| Array.get index colors), stroke "#000" ]

        makeDot datum =
            let
                ( x, y ) =
                    Shape.centroid datum
            in
            circle [ cx x, cy y, r 5 ] []
    in
    g [ transform [ Translate radius radius ] ]
        [ g [] <| List.indexedMap makeSlice arcs
        , g [] <| List.map makeDot arcs
        ]


annular : List Arc -> Svg msg
annular arcs =
    let
        makeSlice index datum =
            Path.element (Shape.arc { datum | innerRadius = radius - 60 }) [ fill (Maybe.withDefault "#000" <| Array.get index colors), stroke "#000" ]

        makeDot datum =
            let
                ( x, y ) =
                    Shape.centroid { datum | innerRadius = radius - 60 }
            in
            circle [ cx x, cy y, r 5 ] []
    in
    g [ transform [ Translate (3 * radius + 20) radius ] ]
        [ g [] <| List.indexedMap makeSlice arcs
        , g [] <| List.map makeDot arcs
        ]


view : List Float -> Svg msg
view model =
    let
        pieData =
            model |> Shape.pie { defaultPieConfig | outerRadius = radius }
    in
    svg [ width w, height h ]
        [ circular pieData
        , annular pieData
        ]


data : List Float
data =
    [ 1, 1, 2, 3, 5, 8, 13 ]


main : Svg msg
main =
    view data
