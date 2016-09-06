module Cross exposing (main)

import Visualization.Path exposing (begin, moveTo, lineTo, close, toAttrString)
import Svg exposing (svg, g, path)
import Svg.Attributes exposing (transform, d, stroke, fill, strokeLinejoin, strokeWidth)


cross =
    begin
        |> moveTo -60 -20
        |> lineTo -20 -20
        |> lineTo -20 -60
        |> lineTo 20 -60
        |> lineTo 20 -20
        |> lineTo 60 -20
        |> lineTo 60 20
        |> lineTo 20 20
        |> lineTo 20 60
        |> lineTo -20 60
        |> lineTo -20 20
        |> lineTo -60 20
        |> close
        |> toAttrString


main =
    svg []
        [ g [ transform "translate(70,70)" ]
            [ path [ d cross, stroke "#000", fill "none", strokeLinejoin "round", strokeWidth "10" ] [] ]
        ]
