module Hierarchy.Partition exposing (layout)

import Hierarchy.Treemap as Treemap
import Tree exposing (Tree)


layout :
    { padding : a -> Float, value : a -> Float, size : ( Float, Float ) }
    -> Tree a
    -> Tree { x : Float, y : Float, width : Float, height : Float, value : Float, node : a }
layout opts t =
    let
        ( dx, dy ) =
            opts.size

        n =
            toFloat (Tree.depth t) + 1
    in
    t
        |> Tree.map
            (\node ->
                { bbox =
                    { x0 = opts.padding node
                    , y0 = opts.padding node
                    , x1 = dx
                    , y1 = dy / n
                    }
                , value = opts.value node
                , node = node
                }
            )
        |> Tree.depthFirstTraversal
            (\s a l c ->
                let
                    depth =
                        List.length a

                    children =
                        List.map2
                            (\bb ->
                                Tree.updateLabel (\cn -> { cn | bbox = bb })
                            )
                            (Treemap.dice depth { x0 = l.bbox.x0, y0 = dy * (toFloat depth + 1) / n, x1 = l.bbox.x1, y1 = dy * (toFloat depth + 2) / n } l.value (List.map (\child -> .value (Tree.label child)) c))
                            c

                    p =
                        opts.padding l.node

                    bbox0 =
                        { x0 = l.bbox.x0
                        , y0 = l.bbox.y0
                        , x1 = l.bbox.x1 - p
                        , y1 = l.bbox.y1 - p
                        }

                    bbox1 =
                        if bbox0.x1 < bbox0.x0 then
                            { bbox0 | x0 = (bbox0.x0 + bbox0.x1) / 2, x1 = (bbox0.x0 + bbox0.x1) / 2 }

                        else
                            bbox0

                    bbox2 =
                        if bbox0.y1 < bbox0.y0 then
                            { bbox1 | y0 = (bbox0.y0 + bbox0.y1) / 2, y1 = (bbox0.y0 + bbox0.y1) / 2 }

                        else
                            bbox1
                in
                ( s
                , { x = bbox2.x0
                  , y = bbox2.y0
                  , width = bbox2.x1 - bbox2.x0
                  , height = bbox2.y1 - bbox2.y0
                  , value = l.value
                  , node = l.node
                  }
                , children
                )
            )
            (\s _ l c -> ( s, Tree.tree l c ))
            ()
        |> Tuple.second
