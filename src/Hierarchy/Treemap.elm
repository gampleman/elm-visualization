module Hierarchy.Treemap exposing (BBox, dice, layout, slice, sliceDice, squarify, squarifyRatio)

import List.Extra
import Tree exposing (Tree)


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
    , size : ( Float, Float )
    }
    -> Tree a
    -> Tree { x : Float, y : Float, width : Float, height : Float, value : Float, node : a }
layout opts =
    Tree.map
        (\node ->
            { bbox =
                { x0 = 0
                , y0 = 0
                , x1 = Tuple.first opts.size
                , y1 = Tuple.second opts.size
                }
            , value = opts.value node
            , node = node
            }
        )
        >> Tree.depthFirstTraversal
            (\p a l c ->
                let
                    depth =
                        List.length a

                    bbox0 =
                        { x0 = l.bbox.x0 + p
                        , y0 = l.bbox.y0 + p
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

                    node =
                        l.node

                    childPadding =
                        opts.paddingInner node / 2

                    childBBox0 =
                        { x0 = bbox2.x0 + opts.paddingLeft node - childPadding
                        , y0 = bbox2.y0 + opts.paddingTop node - childPadding
                        , x1 = bbox2.x1 - (opts.paddingRight node - childPadding)
                        , y1 = bbox2.y1 - (opts.paddingBottom node - childPadding)
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

                    children =
                        List.map2
                            (\bb ->
                                Tree.updateLabel (\cn -> { cn | bbox = bb })
                            )
                            (opts.tile depth childBBox2 l.value (List.map (\child -> .value (Tree.label child)) c))
                            c
                in
                ( childPadding
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
            0
        >> Tuple.second



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


phi : Float
phi =
    (1 + sqrt 5) / 2


squarify : Int -> BBox -> Float -> List Float -> List BBox
squarify =
    squarifyRatio phi


squarifyRatio : Float -> Int -> BBox -> Float -> List Float -> List BBox
squarifyRatio x =
    let
        ratio =
            if x > 1 then
                x

            else
                1
    in
    \depth { x0, x1, y0, y1 } value_ children ->
        let
            keepAddingWhileRatioImproves sumValue_ minValue_ maxValue_ minRatio alpha soFar nodes =
                case nodes of
                    [] ->
                        ( sumValue_, List.reverse soFar, [] )

                    nodeValue :: rest ->
                        let
                            sumValue =
                                sumValue_ + nodeValue

                            minValue =
                                if nodeValue < minValue_ then
                                    nodeValue

                                else
                                    minValue_

                            maxValue =
                                if nodeValue > maxValue_ then
                                    nodeValue

                                else
                                    maxValue_

                            beta =
                                sumValue * sumValue * alpha

                            newRatio =
                                max (maxValue / beta) (beta / minValue)
                        in
                        if newRatio > minRatio then
                            ( sumValue - nodeValue, List.reverse soFar, nodes )

                        else
                            keepAddingWhileRatioImproves sumValue minValue maxValue newRatio alpha (nodeValue :: soFar) rest

            go value x0_ y0_ nodes soFar =
                case List.Extra.splitWhen (\n -> n > 0) nodes of
                    Just ( row, nodeValue :: tail ) ->
                        let
                            dx =
                                x1 - x0_

                            dy =
                                y1 - y0_

                            minValue =
                                nodeValue

                            maxValue =
                                nodeValue

                            alpha =
                                max (dy / dx) (dx / dy) / (value * ratio)

                            beta =
                                nodeValue ^ 2 * alpha

                            minRatio =
                                max (maxValue / beta) (beta / minValue)

                            ( sumValue, toAdd, rest ) =
                                keepAddingWhileRatioImproves nodeValue minValue maxValue minRatio alpha [ nodeValue ] tail
                        in
                        if dx < dy then
                            if value > 0 then
                                go (value - sumValue) x0_ (y0_ + dy * sumValue / value) rest (dice depth { x0 = x0_, x1 = x1, y0 = y0_, y1 = y0_ + dy * sumValue / value } sumValue (row ++ toAdd) :: soFar)

                            else
                                go (value - sumValue) x0_ y0_ rest (dice depth { x0 = x0_, x1 = x1, y0 = y0_, y1 = y1 } sumValue (row ++ toAdd) :: soFar)

                        else if value > 0 then
                            go (value - sumValue) (x0_ + dx * sumValue / value) y0_ rest (slice depth { x0 = x0_, x1 = x0_ + dx * sumValue / value, y0 = y0_, y1 = y1 } sumValue (row ++ toAdd) :: soFar)

                        else
                            go (value - sumValue) x0_ y0_ rest (slice depth { x0 = x0_, x1 = x1, y0 = y0_, y1 = y1 } sumValue (row ++ toAdd) :: soFar)

                    _ ->
                        List.concat (List.reverse soFar)
        in
        go value_ x0 y0 children []
