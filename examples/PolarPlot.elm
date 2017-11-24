module PolarPlot exposing (main)

{-| A polar plot of `sin(2x)cos(2x)`.
-}

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Visualization.List exposing (range)
import Visualization.Scale as Scale exposing (ContinuousScale)
import Visualization.Shape as Shape


w : Float
w =
    900


h : Float
h =
    450


padding : Float
padding =
    30


radius : Float
radius =
    Basics.min w h / 2 - padding


radiusScale : ContinuousScale
radiusScale =
    Scale.linear ( 0, 0.5 ) ( 0, radius )


fn : Float -> Float
fn t =
    sin (2 * t) * cos (2 * t)


data : List (Maybe ( Float, Float ))
data =
    range 0 (2 * pi) 0.01
        |> List.map (\t -> Just ( -t + pi / 2, Scale.convert radiusScale (fn t) ))


spoke : Float -> Svg msg
spoke angle =
    g [ transform ("rotate(" ++ toString -angle ++ ")") ]
        [ line [ x2 (toString radius) ] []
        , text_
            [ radius + 6 |> toString |> x
            , dy ".35em"
            , textAnchor
                (if angle < 270 && angle > 90 then
                    "end"
                 else
                    "inherit"
                )
            , transform
                ((if angle < 270 && angle > 90 then
                    "rotate(180 " ++ toString (radius + 6) ++ ",0)"
                  else
                    ""
                 )
                )
            ]
            [ text (toString angle ++ "°") ]
        ]


radialAxis : Float -> Svg msg
radialAxis radius =
    g []
        [ circle [ Scale.convert radiusScale radius |> toString |> r ] []
        , text_ [ Scale.convert radiusScale radius * -1 - 4 |> toString |> y, transform "rotate(15)", textAnchor "middle" ]
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
    svg [ width (toString w ++ "px"), height (toString h ++ "px") ]
        [ Svg.style [] [ text css ]
        , g [ class "label", transform ("translate" ++ toString ( padding * 2, h / 2 )) ]
            [ text_ [ fontSize "20" ] [ text "sin(2x)cos(2x)" ]
            , text_ [ fontSize "12", y "20" ] [ text "A polar plot." ]
            ]
        , g [ transform ("translate" ++ toString ( w / 2 + radius, h / 2 )) ]
            [ Scale.ticks radiusScale 5
                |> List.drop 1
                |> List.map radialAxis
                |> g [ class "r axis" ]
            , range 0 360 30
                |> List.map spoke
                |> g [ class "a axis" ]
            , Svg.path [ d <| Shape.lineRadial Shape.linearCurve data, class "line" ] []
            ]
        ]
