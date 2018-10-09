module Centroid exposing (main)

{-| The black dots show the midpoint computed by `centroid`. Note that this is
not the geometric center of the arc, which may be outside the arc; this method
is merely a convenience for positioning labels.
-}

import Array exposing (Array)
import Color exposing (Color, fromRgba)
import LowLevel.Command exposing (arcTo, clockwise, largestArc, moveTo)
import Path
import Shape exposing (Arc, defaultPieConfig)
import SubPath exposing (SubPath)
import TypedSvg exposing (circle, g, svg)
import TypedSvg.Attributes exposing (fill, stroke, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, r)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Fill(..), Transform(..))


w : Float
w =
    990


h : Float
h =
    504


rgba255 : Int -> Int -> Int -> Float -> Color
rgba255 r g b a =
    Color.fromRgba { red = toFloat r / 255, green = toFloat g / 255, blue = toFloat b / 255, alpha = a }


colors : Array Color
colors =
    Array.fromList
        [ rgba255 31 119 180 0.5
        , rgba255 255 127 14 0.5
        , rgba255 44 159 44 0.5
        , rgba255 214 39 40 0.5
        , rgba255 148 103 189 0.5
        , rgba255 140 86 75 0.5
        , rgba255 227 119 194 0.5
        , rgba255 128 128 128 0.5
        , rgba255 188 189 34 0.5
        , rgba255 23 190 207 0.5
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
            Path.element (Shape.arc datum)
                [ fill <| Fill <| Maybe.withDefault Color.black <| Array.get index colors
                , stroke Color.black
                ]

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
            Path.element (Shape.arc { datum | innerRadius = radius - 60 })
                [ fill <| Fill <| Maybe.withDefault Color.black <| Array.get index colors
                , stroke Color.black
                ]

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
    svg [ viewBox 0 0 w h ]
        [ circular pieData
        , annular pieData
        ]


data : List Float
data =
    [ 1, 1, 2, 3, 5, 8, 13 ]


main : Svg msg
main =
    view data
