module Cross exposing (main)

{-| This module illustrates drawing a *complex shape* with a more semantic API
than raw SVG.
-}

import Visualization.Path exposing (begin, moveTo, lineTo, close, toAttrString)
import Svg exposing (svg, g, path)
import Svg.Attributes exposing (transform, d, stroke, fill, strokeLinejoin, strokeWidth)
import Html.Attributes exposing (style)


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
    svg [ style [ ( "width", "900px" ), ( "height", "500px" ) ] ]
        [ g [ transform "translate(420,250)" ]
            [ path [ d (cross 70), stroke "#000", fill "none", strokeLinejoin "round", strokeWidth "40" ] [] ]
        ]
