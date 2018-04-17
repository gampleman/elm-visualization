module Visualization.Voronoi.Delaunator exposing (..)

{- Based on https://github.com/mapbox/delaunator/blob/master/index.js -}

import Array.Hamt as Array exposing (Array)
import Bitwise
import IntDict exposing (IntDict)
import OpenSolid.Circle2d
import OpenSolid.Point2d exposing (in_)
import Visualization.Voronoi.Zipper as Zipper exposing (Zipper)


force : Int -> Array Float -> Float
force i arr =
    case Array.get i arr of
        Just a ->
            a

        Nothing ->
            0 / 0


dist : number -> number -> number -> number -> number
dist ax ay bx by =
    (ax - bx) ^ 2 + (ay - by) ^ 2


reduce : (Int -> b -> b) -> b -> Int -> b
reduce f initial n =
    if n <= 1 then
        f 0 initial
    else
        reduce f (f (n - 1) initial) (n - 1)


infinity : Float
infinity =
    1 / 0


delaunator :
    Array Float
    -> Result String { halfedges : IntDict Int, triangles : Array Int }
delaunator coords =
    let
        _ =
            Debug.log "running" "delaunator"

        -- divide by two and floor
        n =
            Array.length coords |> Bitwise.shiftRightBy 1

        computeExtrema i r =
            let
                x =
                    force (2 * i) coords

                y =
                    force (2 * i + 1) coords
            in
                { minX = min r.minX x
                , minY = min r.minY y
                , maxX = max r.maxX x
                , maxY = max r.maxY y
                }

        { minX, minY, maxX, maxY } =
            reduce computeExtrema
                { minX = infinity
                , minY = infinity
                , maxX = -infinity
                , maxY = -infinity
                }
                n

        cx =
            (minX + maxX) / 2

        cy =
            (minY + maxY) / 2

        minDist ax ay i ( pdist, pi ) =
            let
                cdist =
                    dist ax ay (force (2 * i) coords) (force (2 * i + 1) coords)
            in
                if pdist > cdist then
                    ( cdist, i )
                else
                    ( pdist, pi )

        -- pick a seed point close to the centroid
        i0 =
            reduce (minDist cx cy) ( infinity, -1 ) n |> Tuple.second

        i0x =
            (force (2 * i0) coords)

        i0y =
            (force (2 * i0 + 1) coords)

        -- find the point closest to the seed
        tmpi1 =
            reduce
                (\i r ->
                    if i == i0 then
                        r
                    else
                        minDist i0x i0y i r
                )
                ( infinity, -1 )
                n
                |> Tuple.second

        -- find the third point which forms the smallest circumcircle with the first two
        ( minRadius, tmpi2 ) =
            reduce
                (\i ( pr, pi ) ->
                    if i == i0 || i == tmpi1 then
                        ( pr, pi )
                    else
                        let
                            r =
                                circumradius i0x i0y (force (2 * tmpi1) coords) (force (2 * tmpi1 + 1) coords) (force (2 * i) coords) (force (2 * i + 1) coords)
                        in
                            if r < pr then
                                ( r, i )
                            else
                                ( pr, pi )
                )
                ( infinity, -1 )
                n

        ( i1, i2 ) =
            if area i0x i0y (force (2 * tmpi1) coords) (force (2 * tmpi1 + 1) coords) (force (2 * tmpi2) coords) (force (2 * tmpi2 + 1) coords) < 0 then
                ( tmpi2, tmpi1 )
            else
                ( tmpi1, tmpi2 )

        i1x =
            force (2 * i1) coords

        i1y =
            force (2 * i1 + 1) coords

        i2x =
            force (2 * i2) coords

        i2y =
            force (2 * i2 + 1) coords

        center =
            circumcenter i0x i0y i1x i1y i2x i2y

        ( centerx, centery ) =
            center

        ids =
            List.range 0 (n - 1)
                |> List.sortBy (\id -> ( dist centerx centery (force (id * 2) coords) (force (id * 2 + 1) coords), (force (id * 2) coords), (force (id * 2 + 1) coords), (id % 2 * -1) * id ))
                |> Debug.log "ids"

        hashSize =
            ceiling (sqrt (toFloat n))

        hashKey x y =
            let
                dx =
                    x - centerx

                dy =
                    y - centery

                p =
                    1 - dx / (abs dx + abs dy)
            in
                floor ((2 + p * sign dy) / 4 * (toFloat hashSize))

        compute i ({ xp, yp, hash, hull, triangles, halfedges } as rec) =
            let
                x =
                    force (i * 2) coords

                y =
                    force (i * 2 + 1) coords

                _ =
                    ( Debug.log "iteration id" i, Debug.log "hash" (IntDict.toList <| IntDict.map (\_ { i } -> i) hash) )
            in
                if (x == xp && y == yp) then
                    --  skip duplicate points
                    rec
                else if
                    (x == i0x && y == i0y)
                        || (x == i1x && y == i1y)
                        || (x == i2x && y == i2y)
                    -- skip seed triangle points
                then
                    { rec | xp = x, yp = y }
                else
                    let
                        startKey =
                            hashKey x y

                        ( startItemMaybe, _ ) =
                            until (\( start, key ) -> key == startKey || isJust start) (\( start, key ) -> ( IntDict.get key hash, (key + 1) % hashSize )) ( Nothing, startKey )

                        startItem =
                            case startItemMaybe of
                                Just item ->
                                    Debug.log "start" item

                                Nothing ->
                                    Debug.crash "oh shit"

                        --|> Maybe.withDefault { x = i0x, y = i0y, i = i0, t = 0 }
                        hull_0 =
                            Zipper.walkForward
                                (\z ->
                                    let
                                        e =
                                            Zipper.current z

                                        next =
                                            Zipper.next z
                                    in
                                        area x y e.x e.y next.x next.y >= 0
                                )
                                (Zipper.findBy (\{ i } -> i == startItem.i) hull)

                        e =
                            Zipper.current hull_0

                        shouldWalkBack =
                            startItem == e

                        _ =
                            ( Debug.log "triangles" (Array.toList triangles), Debug.log "halfedges" (IntDict.toList halfedges), Debug.log "hull_0" (Zipper.toList hull_0), Debug.log "hull_2" (Zipper.toList hull_2) )

                        ( t, triangles_0, halfedges_0 ) =
                            addTriangle e.i i (.i (Zipper.next hull_0)) -1 -1 e.t triangles halfedges

                        ( legalizedT, triangles_1, halfedges_1 ) =
                            legalize (t + 2) triangles_0 halfedges_0 coords

                        hull_1 =
                            hull_0
                                |> Zipper.replace { e | t = t }
                                |> Zipper.insert { x = x, y = y, i = i, t = legalizedT }

                        e_0 =
                            Zipper.current hull_1

                        hull_2 =
                            if Just (.t (Zipper.previous (Zipper.backward hull_1))) == IntDict.get (t + 1) halfedges_1 then
                                hull_1
                                    |> Zipper.backward
                                    |> Zipper.backward
                                    |> Zipper.mapCurrent (\a -> { a | t = t + 2 })
                                    |> Zipper.forward
                                    |> Zipper.forward
                            else
                                hull_1

                        res =
                            while (\{ hull } -> Debug.log "forward area" (area x y (.x (Zipper.current hull)) (.y (Zipper.current hull)) (.x (Zipper.next hull)) (.y (Zipper.next hull))) < 0)
                                (\{ triangles, halfedges, hull, hash } ->
                                    let
                                        ( t, triangles_0, halfedges_0 ) =
                                            addTriangle (.i (Zipper.current hull)) i (.i (Zipper.next hull)) (.t (Zipper.previous hull)) -1 (.t (Zipper.current hull)) triangles halfedges

                                        ( legalizedT, triangles_1, halfedges_1 ) =
                                            legalize (t + 2) triangles_0 halfedges_0 coords

                                        hk =
                                            hashKey (.x (Zipper.current hull)) (.y (Zipper.current hull))

                                        hash_0 =
                                            if Maybe.map .i (IntDict.get hk hash) == Just (.i (Zipper.current hull)) then
                                                IntDict.remove (hk) hash
                                            else
                                                hash

                                        _ =
                                            ( Debug.log "forward hash removing" (hashKey (.x (Zipper.current hull)) (.y (Zipper.current hull))), Debug.log "hash after forward removal" (IntDict.toList <| IntDict.map (\_ { i } -> i) hash_0) )

                                        hull_0 =
                                            hull
                                                |> Zipper.backward
                                                |> Zipper.mapCurrent (\a -> { a | t = legalizedT })
                                                |> Zipper.forward
                                                |> Zipper.remove
                                    in
                                        { triangles = triangles_1, halfedges = halfedges_1, hull = hull_0, hash = hash_0 }
                                )
                                { hull = Zipper.forward hull_2, halfedges = halfedges_1, triangles = triangles_1, hash = hash }

                        res1 =
                            if shouldWalkBack then
                                while (\{ hull } -> Debug.log "backward area" (area x y (.x (Zipper.previous hull)) (.y (Zipper.previous hull)) (.x (Zipper.current hull)) (.y (Zipper.current hull))) < 0)
                                    (\{ triangles, halfedges, hull, hash } ->
                                        let
                                            ( t, triangles_0, halfedges_0 ) =
                                                addTriangle (.i (Zipper.previous hull)) i (.i (Zipper.current hull)) -1 (.t (Zipper.current hull)) (.t (Zipper.previous hull)) triangles halfedges

                                            ( legalizedT, triangles_1, halfedges_1 ) =
                                                legalize (t + 2) triangles_0 halfedges_0 coords

                                            hk =
                                                hashKey (.x (Zipper.current hull)) (.y (Zipper.current hull))

                                            hash_0 =
                                                if Maybe.map .i (IntDict.get hk hash) == Just (.i (Zipper.current hull)) then
                                                    IntDict.remove (hk) hash
                                                else
                                                    hash

                                            hull_0 =
                                                hull
                                                    |> Zipper.backward
                                                    |> Zipper.mapCurrent (\a -> { a | t = t })
                                                    |> Zipper.forward
                                                    |> Zipper.remove
                                                    |> Zipper.backward

                                            _ =
                                                ( Debug.log "halfedges at start of backward" (IntDict.toList halfedges)
                                                , Debug.log "addTriangle"
                                                    [ (.i (Zipper.previous hull)), i, (.i (Zipper.current hull)), -1, (.t (Zipper.current hull)), (.t (Zipper.previous hull)) ]
                                                , Debug.log "halfedges in backward after addTriangle" (IntDict.toList halfedges_0)
                                                , Debug.log "halfedges at end of backward" (IntDict.toList halfedges_1)
                                                )
                                        in
                                            { triangles = triangles_1, halfedges = halfedges_1, hull = hull_0, hash = hash_0 }
                                    )
                                    { hull = Zipper.backward (Zipper.findBy (\{ i } -> i == e_0.i) res.hull), halfedges = res.halfedges, triangles = res.triangles, hash = res.hash }
                            else
                                res

                        hull_3 =
                            Zipper.findBy (\{ i } -> i == e_0.i) res1.hull
                    in
                        { xp = x
                        , yp = y
                        , triangles = res1.triangles
                        , halfedges = res1.halfedges
                        , hull = res1.hull
                        , hash =
                            res1.hash
                                |> IntDict.insert (hashKey (.x (Zipper.current hull_3)) (.y (Zipper.current hull_3))) (Zipper.current hull_3)
                                |> IntDict.insert (hashKey (.x (Zipper.previous hull_3)) (.y (Zipper.previous hull_3))) (Zipper.previous hull_3)
                        }

        ( _, initialTriangles, initialHalfedges ) =
            addTriangle i0 i1 i2 -1 -1 -1 Array.empty IntDict.empty

        initial =
            { xp = 0 / 0
            , yp = 0 / 0
            , triangles = initialTriangles
            , halfedges = initialHalfedges
            , hull = Zipper.singleton { x = i0x, y = i0y, i = i0, t = 0 } |> Zipper.insert { x = i1x, y = i1y, i = i1, t = 1 } |> Zipper.insert { x = i2x, y = i2y, i = i2, t = 2 }
            , hash =
                IntDict.empty
                    |> IntDict.insert (hashKey i0x i0y) { x = i0x, y = i0y, i = i0, t = 0 }
                    |> IntDict.insert (hashKey i1x i1y) { x = i1x, y = i1y, i = i1, t = 1 }
                    |> IntDict.insert (hashKey i2x i2y) { x = i2x, y = i2y, i = i2, t = 2 }
            }

        { triangles, halfedges } =
            List.foldl compute initial ids
    in
        if minRadius == infinity then
            Err "No Delaunay triangulation exists for this input."
        else
            Ok { triangles = triangles, halfedges = halfedges }


type alias Node =
    { x : Float, y : Float, i : Int, t : Int }


usafeIDGet k s =
    case IntDict.get k s of
        Just v ->
            v

        Nothing ->
            -1


unsageAGet k s =
    case Array.get k s of
        Just v ->
            v

        Nothing ->
            -1


legalize a triangles halfedges coords =
    let
        b =
            usafeIDGet a halfedges

        a0 =
            a - a % 3

        b0 =
            b - b % 3

        al =
            a0 + (a + 1) % 3

        ar =
            a0 + (a + 2) % 3

        bl =
            b0 + (b + 2) % 3

        p0 =
            unsageAGet ar triangles

        pr =
            unsageAGet a triangles

        pl =
            unsageAGet al triangles

        p1 =
            unsageAGet bl triangles

        illegal =
            inCircle (force (2 * p0) coords) (force (2 * p0 + 1) coords) (force (2 * pr) coords) (force (2 * pr + 1) coords) (force (2 * pl) coords) (force (2 * pl + 1) coords) (force (2 * p1) coords) (force (2 * p1 + 1) coords)
    in
        if illegal then
            let
                triangles_0 =
                    triangles
                        |> Array.set a p1
                        |> Array.set b p0

                halfedges_0 =
                    link a (usafeIDGet bl halfedges) halfedges

                halfedges_1 =
                    link b (usafeIDGet ar halfedges) halfedges_0 |> link ar bl

                br =
                    b0 + (b + 1) % 3

                ( _, triangles_1, halfedges_2 ) =
                    legalize a triangles_0 halfedges_1 coords
            in
                legalize br triangles_1 halfedges_2 coords
        else
            ( ar, triangles, halfedges )


inCircle ax ay bx by cx cy px py =
    let
        dx =
            ax - px

        dy =
            ay - py

        ex =
            bx - px

        ey =
            by - py

        fx =
            cx - px

        fy =
            cy - py

        ap =
            dx * dx + dy * dy

        bp =
            ex * ex + ey * ey

        cp =
            fx * fx + fy * fy
    in
        dx
            * (ey * cp - bp * fy)
            - dy
            * (ex * cp - bp * fx)
            + ap
            * (ex * fy - ey * fx)
            < 0


link : Int -> Int -> IntDict Int -> IntDict Int
link a b halfedges =
    if b == -1 then
        IntDict.insert a b halfedges
    else
        halfedges |> IntDict.insert a b |> IntDict.insert b a


addTriangle i0 i1 i2 a b c triangles halfedges =
    let
        t =
            Array.length triangles
    in
        ( t, Array.append triangles (Array.fromList [ i0, i1, i2 ]), halfedges |> link t a |> link (t + 1) b |> link (t + 2) c )


isJust : Maybe a -> Bool
isJust a =
    case a of
        Just _ ->
            True

        Nothing ->
            False


sign a =
    if a < 0 then
        -1
    else
        1


until : (a -> Bool) -> (a -> a) -> a -> a
until cond body initiall =
    let
        helper counter cond body initial =
            let
                next =
                    body initial
            in
                if counter > 10000 then
                    Debug.crash "Boom"
                else if cond next then
                    next
                else
                    helper (counter + 1) cond body next
    in
        helper 0 cond body initiall


while : (a -> Bool) -> (a -> a) -> a -> a
while cond body initial =
    let
        helper counter cond body initial =
            if counter > 10000 then
                Debug.crash "Cr$h"
            else if cond initial then
                helper (counter + 1) cond body (body initial)
            else
                initial
    in
        helper 0 cond body initial


area px py qx qy rx ry =
    (qy - py) * (rx - qx) - (qx - px) * (ry - qy)


circumradius ax ay bx by cx cy =
    case OpenSolid.Circle2d.throughPoints ( (OpenSolid.Point2d.fromCoordinates ( ax, ay )), (OpenSolid.Point2d.fromCoordinates ( bx, by )), (OpenSolid.Point2d.fromCoordinates ( cx, cy )) ) of
        Just circle ->
            OpenSolid.Circle2d.radius circle

        Nothing ->
            infinity


circumcenter ax ay bx by cx cy =
    let
        dx =
            bx - ax

        dy =
            by - ay

        ex =
            cx - ax

        ey =
            cy - ay

        bl =
            dx * dx + dy * dy

        cl =
            ex * ex + ey * ey

        d =
            dx * ey - dy * ex

        x =
            ax + (ey * bl - dy * cl) * 0.5 / d

        y =
            ay + (dx * cl - ex * bl) * 0.5 / d
    in
        ( x, y )
