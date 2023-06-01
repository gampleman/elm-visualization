module TidyTree exposing (main)


import Hierarchy exposing (Hierarchy(..))
import Hierarchy.Tidy
import TypedSvg exposing (g, rect, svg)
import TypedSvg.Attributes exposing (class, fill, transform, viewBox, stroke)
import TypedSvg.Attributes.InPx exposing (height, width, x, y)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Paint(..), Transform(..))
import Shape
import Path
import Color

tree = 
    Hierarchy (0, 80,70) [
        Hierarchy (1,30,90) [
            Hierarchy (10, 30, 80) []
            , Hierarchy (10, 50, 50) []
            , Hierarchy (10, 60, 80) []
        ]
        , Hierarchy (3, 10, 10) [ Hierarchy (10, 50, 50) [] ]
    ]

layedOut = Hierarchy.Tidy.layout { width = (\(_, w, _) -> w), height = (\(_, _, h) -> h), layered = True, parentChildMargin = 10, peerMargin = 10 } tree

main = 
    svg [ viewBox -200 0 400 300] [
        layedOut
        |> Hierarchy.toList 
        |> List.map (\item -> 
            rect [ x (item.x - item.width / 2), y (item.y ), width item.width, height item.height ] [
                TypedSvg.text_ [] [TypedSvg.Core.text ((\(a, _, _) -> String.fromInt a) item.value)]
            ]
        )
        |> g []
        , layedOut 
        |> Hierarchy.Tidy.links 
        |> List.map (\(from, to) -> Shape.bumpYCurve [ (from.x, from.y + from.height), (to.x, to.y)])
        |> (\p -> Path.element p [ fill PaintNone, stroke (Paint Color.black)]) 
    ]
