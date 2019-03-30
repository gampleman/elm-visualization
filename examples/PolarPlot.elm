module PolarPlot exposing (main)

{-| A polar plot of `sin(2x)cos(2x)`.
-}

import Path
import Scale exposing (ContinuousScale)
import Shape
import Statistics
import TypedSvg exposing (circle, g, line, style, svg, text_)
import TypedSvg.Attributes exposing (class, dy, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (fontSize, r, x, x2, y)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Transform(..), em)


w : Float
w =
    900


h : Float
h =
    450


padding : Float
padding =
    30


mainRadius : Float
mainRadius =
    Basics.min w h / 2 - padding


radiusScale : ContinuousScale Float
radiusScale =
    Scale.linear ( 0, mainRadius ) ( 0, 0.5 )


fn : Float -> Float
fn t =
    sin (2 * t) * cos (2 * t)


data : List (Maybe ( Float, Float ))
data =
    Statistics.range 0 (2 * pi) 0.01
        |> List.map (\t -> Just ( -t + pi / 2, Scale.convert radiusScale (fn t) ))


spoke : Float -> Svg msg
spoke angle =
    g [ transform [ Rotate -angle 0 0 ] ]
        [ line [ x2 mainRadius ] []
        , text_
            [ x (mainRadius + 6)
            , dy (em 0.35)
            , textAnchor
                (if angle < 270 && angle > 90 then
                    AnchorEnd

                 else
                    AnchorInherit
                )
            , transform
                (if angle < 270 && angle > 90 then
                    [ Rotate 180 (mainRadius + 6) 0 ]

                 else
                    []
                )
            ]
            [ text (String.fromFloat angle ++ "°") ]
        ]


radialAxis : Float -> Svg msg
radialAxis radius =
    g []
        [ circle [ r <| Scale.convert radiusScale radius ] []
        , text_ [ y <| Scale.convert radiusScale radius * -1 - 4, transform [ Rotate 15 0 0 ], textAnchor AnchorMiddle ]
            [ radius |> Scale.tickFormat radiusScale 5 |> text ]
        ]


css : String
css =
    """
    .frame {
      fill: none;
      stroke: #000;
    }

    .axis text {
      font: 10px sans-serif;
    }

    .axis line,
    .axis circle {
      fill: none;
      stroke: #777;
      stroke-dasharray: 1,4;
    }

    .axis :last-of-type circle {
      stroke: #333;
      stroke-dasharray: none;
    }

    .line {
      fill: none;
      stroke: red;
      stroke-width: 1.5px;
    }

    .label {
      font-family:  sans-serif;
    }
  """


main : Svg msg
main =
    svg [ viewBox 0 0 w h ]
        [ style [] [ text css ]
        , g [ class [ "label" ], transform [ Translate (padding * 2) (h / 2) ] ]
            [ text_ [ fontSize 20 ] [ text "sin(2x)cos(2x)" ]
            , text_ [ fontSize 12, y 20 ] [ text "A polar plot." ]
            ]
        , g [ transform [ Translate (w / 2 + mainRadius) (h / 2) ] ]
            [ Scale.ticks radiusScale 5
                |> List.drop 1
                |> List.map radialAxis
                |> g [ class [ "r", "axis" ] ]
            , Statistics.range 0 360 30
                |> List.map spoke
                |> g [ class [ "a", "axis" ] ]
            , Path.element (Shape.lineRadial Shape.linearCurve data) [ class [ "line" ] ]
            ]
        ]
