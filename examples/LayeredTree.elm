module LayeredTree exposing (main)

{-| Shows the behavior of the `Hierarchy.layered` attribute: when present, each level of depth of the tree is rendered at the same y offset. When this is absent, the tree is free to use up space much more efficiently rendereing a potentially significantly smaller diagram.

@category Reference

-}

import Color
import Hierarchy
import Path
import Shape
import Tree exposing (Tree)
import TypedSvg exposing (g, rect, svg)
import TypedSvg.Attributes exposing (dy, fill, pointerEvents, stroke, style, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (fontSize, height, rx, width, x, y)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Paint(..), Transform(..), em)


w : Float
w =
    990


h : Float
h =
    504


padding : Float
padding =
    30


layout : Bool -> Tree { x : Float, y : Float, width : Float, height : Float, node : { height : Float, width : Float } }
layout layered =
    Hierarchy.tidy
        [ Hierarchy.nodeSize
            (\{ width, height } ->
                ( width, height )
            )
        , Hierarchy.parentChildMargin 20
        , Hierarchy.peerMargin 20
        , if layered then
            Hierarchy.layered

          else
            Hierarchy.none
        ]
        tree


main =
    svg [ viewBox 0 0 w h ]
        [ g []
            [ TypedSvg.text_ [ x padding, y padding, dy (em 1), fontSize 18, TypedSvg.Attributes.fontFamily [ "sans-serif" ] ] [ TypedSvg.Core.text "Non-layered" ]
            , view (layout False)
            ]
        , g [ transform [ Translate (w / 2) 0 ] ]
            [ TypedSvg.text_ [ x padding, y padding, dy (em 1), fontSize 18, TypedSvg.Attributes.fontFamily [ "sans-serif" ] ] [ TypedSvg.Core.text "Layered" ]
            , view (layout True)
            ]
        ]


view : Tree { x : Float, y : Float, width : Float, height : Float, node : { height : Float, width : Float } } -> Svg msg
view layedOut =
    g [ transform [ Translate (padding + w / 4) padding ] ]
        [ layedOut
            |> Tree.links
            |> List.map
                (\( from, to ) ->
                    Shape.bumpYCurve
                        [ ( from.x + from.width / 2, from.y + from.height )
                        , ( to.x + to.width / 2, to.y )
                        ]
                )
            |> (\p ->
                    Path.element p
                        [ fill PaintNone
                        , stroke (Paint (Color.rgb 0.3 0.3 0.3))
                        , style "vector-effect: non-scaling-stroke"
                        , pointerEvents "none"
                        ]
               )
        , layedOut
            |> Tree.toList
            |> List.map
                (\item ->
                    rect [ rx 5, width item.width, height item.height, x item.x, y item.y, fill (Paint (Color.rgba 0.1 0.1 0.8 0.3)), stroke (Paint (Color.rgba 0.1 0.1 0.8 1)) ] []
                )
            |> g []
        ]


tree : Tree { width : Float, height : Float }
tree =
    Tree.tree { width = 100, height = 50 }
        [ Tree.tree { width = 60, height = 140 }
            [ Tree.singleton { width = 60, height = 160 }
            , Tree.tree { width = 100, height = 100 }
                [ Tree.singleton { width = 40, height = 40 }
                , Tree.singleton { width = 40, height = 40 }
                ]
            , Tree.singleton { width = 120, height = 160 }
            ]
        , Tree.tree { width = 20, height = 20 }
            [ Tree.singleton { width = 100, height = 100 }
            ]
        ]
