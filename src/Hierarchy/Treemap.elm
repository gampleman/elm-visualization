module Hierarchy.Treemap exposing (..)

import Hierarchy.Tree as Tree exposing (Tree(..))


type alias BBox =
    { x0 : Float, x1 : Float, y0 : Float, y1 : Float }



-- Main function


layout :
    { paddingInner : a -> Float
    , paddingLeft : a -> Float
    , paddingRight : a -> Float
    , paddingTop : a -> Float
    , paddingBottom : a -> Float
    , tile : Int -> BBox -> Float -> List Float -> List BBox
    , value : a -> Float
    , dimensions : ( Float, Float )
    }
    -> Tree a
    -> Tree { x : Float, y : Float, width : Float, height : Float, value : Float, node : a }
layout opts =
    let
        go depth p parentBBox tree =
            let
                bbox0 =
                    { x0 = parentBBox.x0 + p
                    , y0 = parentBBox.y0 + p
                    , x1 = parentBBox.x1 - p
                    , y1 = parentBBox.y1 - p
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

                node =
                    Tree.label tree

                childPadding =
                    opts.paddingInner node

                childBBox0 =
                    { x0 = bbox2.x0 + opts.paddingLeft node - childPadding
                    , y0 = bbox2.y0 + opts.paddingTop node - childPadding
                    , x1 = bbox2.x1 - opts.paddingRight node - childPadding
                    , y1 = bbox2.y1 - opts.paddingBottom node - childPadding
                    }

                childBBox1 =
                    if childBBox0.x1 < childBBox0.x0 then
                        { childBBox0 | x0 = (childBBox0.x0 + childBBox0.x1) / 2, x1 = (childBBox0.x0 + childBBox0.x1) / 2 }

                    else
                        childBBox0

                childBBox2 =
                    if childBBox1.y1 < childBBox1.y0 then
                        { childBBox1 | y0 = (childBBox1.y0 + childBBox1.y1) / 2, y1 = (childBBox1.y0 + childBBox1.y1) / 2 }

                    else
                        childBBox1

                value =
                    opts.value node

                childrenBBoxes =
                    opts.tile depth childBBox2 (opts.value node) (List.map (\child -> opts.value (Tree.label child)) (Tree.children tree))
            in
            Tree.tree
                { x = bbox2.x0
                , y = bbox2.y0
                , width = bbox2.x1 - bbox2.x0
                , height = bbox2.y1 - bbox2.y0
                , value = value
                , node = node
                }
                (List.map2 (go (depth + 1) childPadding) childrenBBoxes (Tree.children tree))
    in
    go 0 0 { x0 = 0, y0 = 0, x1 = Tuple.first opts.dimensions, y1 = Tuple.second opts.dimensions }



-- Tiling methods


slice : Int -> BBox -> Float -> List Float -> List BBox
slice _ { x0, x1, y0, y1 } value children =
    let
        k =
            if value == 0 then
                0

            else
                (y1 - y0) / value
    in
    List.foldl
        (\node ( prevY, lst ) ->
            let
                nextY =
                    prevY + node * k
            in
            ( nextY, { x0 = x0, x1 = x1, y0 = prevY, y1 = nextY } :: lst )
        )
        ( y0, [] )
        children
        |> Tuple.second
        |> List.reverse


dice : Int -> BBox -> Float -> List Float -> List BBox
dice _ { x0, x1, y0, y1 } value children =
    let
        k =
            if value == 0 then
                0

            else
                (x1 - x0) / value
    in
    List.foldl
        (\node ( prevX, lst ) ->
            let
                nextX =
                    prevX + node * k
            in
            ( nextX, { x0 = prevX, x1 = nextX, y0 = y0, y1 = y1 } :: lst )
        )
        ( x0, [] )
        children
        |> Tuple.second
        |> List.reverse


sliceDice : Int -> BBox -> Float -> List Float -> List BBox
sliceDice depth =
    if modBy 2 depth == 1 then
        slice depth

    else
        dice depth
