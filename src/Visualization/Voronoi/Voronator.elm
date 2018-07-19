module Visualization.Voronoi.Voronator exposing (..)

import Array.Hamt as Array exposing (Array)
import Bitwise
import IntDict
import Visualization.Path as Path
import Visualization.Voronoi.Delaunator
import Visualization.Voronoi.Zipper as Zipper


voronoi { triangles, halfedges, points, hull } =
    let
        computeCellTopology i he { index, edges } =
            let
                t =
                    unsafeAGet i triangles

                _ =
                    Debug.log "t" ( i, t, he )

                e0 =
                    Array.get (t * 2) index |> Maybe.withDefault 0

                walkForward ( j, e, edges ) =
                    let
                        j0 =
                            IntDict.get j halfedges

                        edges0 =
                            Array.set e (floor (toFloat j / 3)) edges
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
                                do walkBackward ( i, e, edges0 )
                                    |> reverseEdgesIfNecessary e
                                    |> Break

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
                            Continue ( j0, e + 1, Array.set e (floor (toFloat j / 3)) edges )

                reverseEdgesIfNecessary e1 ( j, e, edges ) =
                    if e1 < e then
                        ( j, e, reverseSub e0 e (reverseSub e0 e edges) )
                    else
                        ( j, e, edges )
            in
                if Array.get (t * 2) index == Array.get (t * 2 + 1) index then
                    let
                        ( j, e1, edges0 ) =
                            do walkForward ( i, e0, edges )
                    in
                        { index = Array.set (t * 2 + 1) e0 index, edges = edges0 }
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

                _ =
                    Debug.log "computeCircumcenter" ( t1, t2, t3, ( x1, y1 ), ( x2, y2 ), ( x3, y3 ) )

                a2 =
                    Debug.log "a2" <| x1 - x2

                a3 =
                    Debug.log "a3" <| x1 - x3

                b2 =
                    Debug.log "b2" <| y1 - y2

                b3 =
                    Debug.log "b3" <| y1 - y3

                d1 =
                    Debug.log "d1" <| x1 * x1 + y1 * y1

                d2 =
                    Debug.log "d2" <| d1 - x2 * x2 - y2 * y2

                d3 =
                    Debug.log "d3" <| d1 - x3 * x3 - y3 * y3

                ab =
                    Debug.log "ab" <| (a3 * b2 - a2 * b3) * 2

                result =
                    Debug.log "resulting" [ (b2 * d3 - b3 * d2) / ab, (a3 * d2 - a2 * d3) / ab ]
            in
                result

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
                        |> Array.set (p0 * 2 + 2) (y0 - y1)
                        |> Array.set (p1 * 2) (y0 - y1)
                        |> Array.set (p0 * 2 + 3) (x1 - x0)
                        |> Array.set (p1 * 2 + 1) (x1 - x0)
                , p0 = p1
                , x0 = x1
                , y0 = y1
                }

        { index, edges } =
            IntDict.foldl computeCellTopology { index = Array.repeat (Array.length points) 0, edges = Array.repeat (IntDict.size halfedges) 0 } halfedges

        _ =
            Debug.log "traingles" (Array.toList triangles)

        circumcenters =
            List.range 0 (Array.length triangles // 3 - 1)
                |> List.map (\a -> a * 3)
                |> List.concatMap
                    (\index ->
                        computeCircumcenter
                            ( unsafeAGet index triangles * 2
                            , unsafeAGet (index + 1) triangles * 2
                            , unsafeAGet (index + 2) triangles * 2
                            )
                    )
                |> Array.fromList

        hullArray =
            Zipper.toList hull |> List.map .t |> Debug.log "hull" |> Array.fromList

        p1 =
            unsafeAGet (unsafeAGet (Array.length hullArray - 1) hullArray) triangles
                * 2

        { vectors } =
            Array.foldl computeExteriorCellRays { vectors = Array.repeat (Array.length points * 2) 0, p0 = p1, x0 = unsafeAGet p1 points, y0 = unsafeAGet (p1 + 1) points } hullArray

        _ =
            Debug.log "halfedges" (IntDict.toList halfedges)

        _ =
            Debug.log "vectors" (Array.toList vectors)
    in
        { points = points, edges = edges, index = index, vectors = vectors, circumcenters = circumcenters, halfedges = halfedges, hull = hullArray, triangles = triangles }


render { halfedges, circumcenters, hull, triangles, vectors } viewport =
    Array.foldl
        (\j path ->
            let
                t =
                    floor (toFloat j / 3) * 2

                x =
                    unsafeAGet t circumcenters

                y =
                    unsafeAGet (t + 1) circumcenters

                v =
                    unsafeAGet j triangles * 4

                _ =
                    Debug.log "project" ( x, y, (unsafeAGet (v + 2) vectors), (unsafeAGet (v + 3) vectors) )
            in
                case Debug.log "project result" <| project x y (unsafeAGet (v + 2) vectors) (unsafeAGet (v + 3) vectors) viewport of
                    Just ( p0, p1 ) ->
                        renderSegment x y p0 p1 viewport path

                    Nothing ->
                        path
        )
        (IntDict.foldl
            (\i j path ->
                if j < i then
                    path
                else
                    let
                        _ =
                            Debug.log "circumcenters" (Array.toList circumcenters)

                        ti =
                            (floor (toFloat i / 3)) * 2

                        tj =
                            (floor (toFloat j / 3)) * 2

                        xi =
                            unsafeAGet ti circumcenters

                        yi =
                            unsafeAGet (ti + 1) circumcenters

                        xj =
                            unsafeAGet tj circumcenters

                        yj =
                            unsafeAGet (tj + 1) circumcenters
                    in
                        renderSegment xi yi xj yj viewport path
            )
            Path.begin
            halfedges
        )
        hull


infinity =
    1 / 0


project x0 y0 vx vy { xmax, xmin, ymax, ymin } =
    let
        projectHorizontal x y t =
            if vx > 0 then
                if x0 >= xmax then
                    Nothing
                else
                    let
                        c =
                            (xmax - x0) / vx
                    in
                        if c < t then
                            Just ( xmax, y0 + c * vy )
                        else
                            Just ( x, y )
            else if vx < 0 then
                if x0 <= xmin then
                    Nothing
                else
                    let
                        c =
                            (xmin - x0) / vx
                    in
                        if c < t then
                            Just ( xmin, y0 + c * vy )
                        else
                            Just ( x, y )
            else
                Just ( x, y )
    in
        if vy < 0 then
            if y0 <= ymin then
                Nothing
            else
                let
                    c =
                        (ymin - y0) / vy
                in
                    if c < infinity then
                        projectHorizontal (x0 + c * vx) ymin c
                    else
                        projectHorizontal 0 0 infinity
        else if vy > 0 then
            if y0 >= ymax then
                Nothing
            else
                let
                    c =
                        (ymax - y0) / vy
                in
                    if c < infinity then
                        projectHorizontal (x0 + c * vx) ymax c
                    else
                        projectHorizontal 0 0 infinity
        else
            projectHorizontal 0 0 infinity


renderSegment x0 y0 x1 y1 viewport path =
    let
        c0 =
            regionCode x0 y0 viewport

        c1 =
            regionCode x1 y1 viewport
    in
        if c0 == 0 && c1 == 0 then
            path
                |> Path.moveTo x0 y0
                |> Path.lineTo x1 y1
        else
            let
                _ =
                    Debug.log "clipSegment call" ( x0, y0, x1, y1, c0, c1, viewport )
            in
                case Debug.log "clipSegment" <| clipSegment x0 y0 x1 y1 c0 c1 viewport of
                    Just ( s0, s1, s2, s3 ) ->
                        path
                            |> Path.moveTo s0 s1
                            |> Path.lineTo s2 s3

                    Nothing ->
                        path


clipSegment x0 y0 x1 y1 c0 c1 ({ xmax, xmin, ymax, ymin } as viewport) =
    do
        (\{ x0, y0, x1, y1, c0, c1 } ->
            if c0 == 0 && c1 == 0 then
                Break <| Just ( x0, y0, x1, y1 )
            else if Bitwise.and c0 c1 /= 0 then
                Break Nothing
            else
                let
                    c =
                        if c0 == 0 then
                            c1
                        else
                            c0

                    ( x, y ) =
                        if Bitwise.and c 8 /= 0 then
                            ( x0 + (x1 - x0) * (ymax - y0) / (y1 - y0), ymax )
                        else if Bitwise.and c 4 /= 0 then
                            ( x0 + (x1 - x0) * (ymin - y0) / (y1 - y0), ymin )
                        else if Bitwise.and c 2 /= 0 then
                            ( xmax, y0 + (y1 - y0) * (xmax - x0) / (x1 - x0) )
                        else
                            ( xmin, y0 + (y1 - y0) * (xmin - x0) / (x1 - x0) )
                in
                    if c0 /= 0 then
                        Continue { x0 = x, y0 = y, x1 = x1, y1 = y1, c0 = regionCode x y viewport, c1 = c1 }
                    else
                        Continue { x0 = x0, y0 = y0, x1 = x, y1 = y, c1 = regionCode x y viewport, c0 = c0 }
        )
        { x0 = x0, y0 = y0, x1 = x1, y1 = y1, c1 = c1, c0 = c0 }


regionCode x y { xmax, xmin, ymax, ymin } =
    Bitwise.or
        (if x < xmin then
            1
         else if x > xmax then
            2
         else
            0
        )
        (if y < ymin then
            4
         else if y > ymax then
            8
         else
            0
        )


reverseSub : Int -> Int -> Array a -> Array a
reverseSub a b dict =
    List.range a (b - 1)
        |> List.map (\index -> ( index, Array.get index dict ))
        |> List.foldr
            (\( index, value ) d ->
                case value of
                    Just v ->
                        Array.set (b - (index - a)) v d

                    Nothing ->
                        d
            )
            dict


type Termination a b
    = Break b
    | Continue a


do : (a -> Termination a b) -> a -> b
do fn initial =
    let
        helper fn initial iter =
            case fn initial of
                Break result ->
                    result

                Continue intermediate ->
                    if iter > 5000 then
                        Debug.crash "exceeded iteration count"
                    else
                        helper fn intermediate (iter + 1)
    in
        helper fn initial 0



-- case fn initial of
--     Break result ->
--         result
--
--     Continue intermediate ->
--         do fn intermediate


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
            0
