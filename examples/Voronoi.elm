module Voronoi exposing (..)

import Array.Hamt as Array
import Html exposing (li, text)
import Html.Events exposing (on)
import IntDict
import Json.Decode exposing (Decoder)
import Svg exposing (circle, path, svg)
import Svg.Attributes exposing (cx, cy, d, fill, height, r, stroke, viewBox, width)
import Visualization.Path exposing (..)
import Visualization.Voronoi.Delaunator exposing (delaunator)
import Visualization.Voronoi.Voronator exposing (render, voronoi)


model =
    Array.fromList [ 10, 10, 101, 11, 12, 100, 90, 102, 40, 130 ]


unsageAGet k s =
    case Array.get k s of
        Just v ->
            v

        Nothing ->
            -1


renderablePoints points scale =
    List.range 0 (Array.length points // 2 - 1)
        |> List.map (\index -> ( unsageAGet (Debug.log "index" index * 2) points * scale, unsageAGet (index * 2 + 1) points * scale ))


getPath points =
    case delaunator points of
        Ok res ->
            let
                _ =
                    Debug.log "points" (Array.toList points)
            in
                render (voronoi res) { xmin = 0, ymin = 0, xmax = 680, ymax = 480 }
                    |> toAttrString

        Err a ->
            ""


type Msg
    = PointAdded ( Float, Float )


mouseEventDecoder : Decoder ( Float, Float )
mouseEventDecoder =
    Json.Decode.map2
        (,)
        (Json.Decode.field "clientX" Json.Decode.float)
        (Json.Decode.field "clientY" Json.Decode.float)


onClick tagger =
    on "click" (Json.Decode.map tagger mouseEventDecoder)


view points =
    svg [ width "680", height "480", onClick (Debug.log "click" >> PointAdded) ] <|
        Svg.rect [ width "680", height "480", fill "transparent" ] []
            :: path [ d (getPath points), stroke "black" ] []
            :: List.map
                (\( x, y ) -> circle [ cx (toString x), cy (toString y), r "2" ] [])
                (renderablePoints points 1)


update msg model =
    case msg of
        PointAdded ( x, y ) ->
            Array.append model (Array.fromList [ x, y ])


main =
    Html.beginnerProgram { model = model, view = view, update = update }
