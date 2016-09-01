module Cross exposing (main)

import Visualization.Path exposing (..)
import Svg
import Svg.Attributes exposing (d, fill, transform)


cross =
    path
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
    Svg.svg []
        [ Svg.g [ transform "translate(70,70)" ] [ Svg.path [ d cross, fill "#000" ] [] ]
        ]
