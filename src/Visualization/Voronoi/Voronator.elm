module Visualization.Voronoi.Voronator exposing (..)

import Array.Hamt as Array exposing (Array)
import IntDict exposing (IntDict)
import Visualization.Voronoi.Zipper as Zipper


voronoi { triangles, halfedges, points, hull } =
    let
        computeCellTopology i _ { index, edges } =
            let
                t =
                    unsafeAGet i triangles

                e0 =
                    IntDict.get (t * 2) index |> Maybe.withDefault 0

                walkForward ( j, e, edges ) =
                    let
                        j0 =
                            IntDict.get j halfedges

                        edges0 =
                            IntDict.insert e (floor (toFloat j / 3)) edges
                    in
                        case j0 of
                            Just j1 ->
                                let
                                    j2 =
                                        if j1 % 3 == 2 then
                                            j1 - 2
                                        else
                                            j1 + 1
                                in
                                    if Array.get (j2) triangles == Just t || j2 == i then
                                        Break ( j2, e + 1, edges0 )
                                    else
                                        Continue ( j2, e + 1, edges0 )

                            Nothing ->
                                Break ( -1, e + 1, edges0 )

                walkBackward ( j, e, edges ) =
                    let
                        ji =
                            if j % 3 == 0 then
                                j + 2
                            else
                                j - 1

                        j0 =
                            IntDict.get ji halfedges |> Maybe.withDefault -1
                    in
                        if j0 == -1 || Array.get j0 triangles /= Just t then
                            Break ( j0, e, edges )
                        else
                            Continue ( j0, e + 1, IntDict.insert e (floor (toFloat j / 3)) edges )
            in
                if IntDict.get (t * 2) index == IntDict.get (t * 2 + 1) index then
                    let
                        ( j, e1, edges0 ) =
                            do walkForward ( i, e0, edges )

                        ( _, e2, edges1 ) =
                            if j /= i then
                                do walkBackward ( i, e1, edges0 )
                            else
                                ( j, e1, edges0 )

                        edges2 =
                            if e1 < e2 then
                                reverseSub e0 e2 (reverseSub e0 e1 edges1)
                            else
                                edges1
                    in
                        { index = IntDict.insert (t * 2 + 1) e0 index, edges = edges2 }
                else
                    { index = index, edges = edges }

        computeCircumcenter ( t1, t2, t3 ) =
            let
                x1 =
                    unsafeAGet (t1) points

                y1 =
                    unsafeAGet (t1 + 1) points

                x2 =
                    unsafeAGet (t2) points

                y2 =
                    unsafeAGet (t2 + 1) points

                x3 =
                    unsafeAGet (t3) points

                y3 =
                    unsafeAGet (t3 + 1) points

                a2 =
                    x1 - x2

                a3 =
                    x1 - x3

                b2 =
                    y1 - y2

                b3 =
                    y1 - y3

                d1 =
                    x1 * x1 + y1 * y1

                d2 =
                    d1 - x2 * x2 - y2 * y2

                d3 =
                    d1 - x3 * x3 - y3 * y3

                ab =
                    (a3 * b2 - a2 * b3) * 2
            in
                [ (b2 * d3 - b3 * d2) / ab, (a3 * d2 - a2 * d3) / ab ]

        computeExteriorCellRays hullPoint { p0, x0, y0, vectors } =
            let
                p1 =
                    unsafeAGet hullPoint triangles * 2

                x1 =
                    unsafeAGet p1 points

                y1 =
                    unsafeAGet (p1 + 1) points
            in
                { vectors =
                    vectors
                        |> IntDict.insert (p0 * 2 + 2) (y0 - y1)
                        |> IntDict.insert (p1 * 2) (y0 - y1)
                        |> IntDict.insert (p0 * 2 + 3) (x1 - x0)
                        |> IntDict.insert (p1 * 2 + 1) (y0 - y1)
                , p0 = p1
                , x0 = x1
                , y0 = y1
                }

        { index, edges } =
            IntDict.foldl computeCellTopology { index = IntDict.empty, edges = IntDict.empty } halfedges

        circumcenters =
            List.range 0 (Array.length triangles // 3 - 1)
                |> List.concatMap
                    (\index ->
                        computeCircumcenter
                            ( unsafeAGet index triangles
                            , unsafeAGet (index + 1) triangles
                            , unsafeAGet (index + 2) triangles
                            )
                    )
                |> Array.fromList

        hullArray =
            Zipper.toList hull |> List.map .t |> Array.fromList

        p1 =
            unsafeAGet (unsafeAGet (Array.length hullArray - 1) hullArray) triangles * 2

        { vectors } =
            Array.foldr computeExteriorCellRays { vectors = IntDict.empty, p0 = p1, x0 = unsafeAGet p1 points, y0 = unsafeAGet (p1 + 1) points } hullArray
    in
        { points = points, edges = edges, index = index, vectors = vectors, circumcenters = circumcenters }


reverseSub : Int -> Int -> IntDict a -> IntDict a
reverseSub a b dict =
    List.range a (b - 1)
        |> List.map (\index -> ( index, IntDict.get index dict ))
        |> List.foldr
            (\( index, value ) d ->
                case value of
                    Just v ->
                        IntDict.insert (b - (index - a)) v d

                    Nothing ->
                        d
            )
            dict


type Termination a
    = Break a
    | Continue a


do : (a -> Termination a) -> a -> a
do fn initial =
    case fn initial of
        Break result ->
            result

        Continue intermediate ->
            do fn intermediate


usafeIDGet k s =
    case IntDict.get k s of
        Just v ->
            v

        Nothing ->
            0


unsafeAGet k s =
    case Array.get k s of
        Just v ->
            v

        Nothing ->
            -1
