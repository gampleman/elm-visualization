module Cross exposing (main)

{-| This module illustrates drawing a _complex shape_ with a more semantic API
than raw SVG.
-}

import Html.Attributes exposing (style)
import Svg exposing (g, path, svg)
import Svg.Attributes exposing (d, fill, stroke, strokeLinejoin, strokeWidth, transform)
import Visualization.Path exposing (begin, close, lineTo, moveTo, toAttrString)


cross size =
    let
        inner =
            size

        outer =
            size * 3
    in
    begin
        |> moveTo -outer -inner
        |> lineTo -inner -inner
        |> lineTo -inner -outer
        |> lineTo inner -outer
        |> lineTo inner -inner
        |> lineTo outer -inner
        |> lineTo outer inner
        |> lineTo inner inner
        |> lineTo inner outer
        |> lineTo -inner outer
        |> lineTo -inner inner
        |> lineTo -outer inner
        |> close
        |> toAttrString


main =
    svg [ style "width" "900px", style "height" "500px" ]
        [ g [ transform "translate(420,250)" ]
            [ path [ d (cross 70), stroke "#000", fill "none", strokeLinejoin "round", strokeWidth "40" ] [] ]
        ]
