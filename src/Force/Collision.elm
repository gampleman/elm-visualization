module Force.Collision exposing (wrapper)

import Array exposing (Array)
import BoundingBox2d
import Circle2d exposing (Circle2d)
import Dict
import Force.Jiggle exposing (jiggleVector)
import Force.QuadTree as QuadTree exposing (QuadTree, UserCoords)
import Point2d
import Units.Pixels as Pixels exposing (Pixels)
import Units.Quantity as Quantity exposing (Quantity)
import Vector2d exposing (Vector2d)


type alias Vertex comparable =
    { index : Int
    , circle : Circle2d Pixels UserCoords
    , velocity : Vector2d Pixels UserCoords
    , key : comparable
    }


wrapper strength iters radii points =
    let
        vertices =
            Dict.toList points
                |> List.filterMap
                    (\( key, { x, y, vx, vy } ) ->
                        Dict.get key radii
                            |> Maybe.map
                                (\radius ->
                                    { key = key, index = 0, circle = Circle2d.withRadius (Pixels.pixels radius) (Point2d.pixels x y), velocity = Vector2d.pixels vx vy }
                                )
                    )
                |> List.indexedMap (\index point -> { point | index = index })
                |> collision strength iters

        folder newVertex =
            Dict.update newVertex.key
                (Maybe.map
                    (\point ->
                        let
                            dv =
                                Vector2d.toPixels newVertex.velocity
                        in
                        { point | vx = dv.x, vy = dv.y }
                    )
                )
    in
    Array.foldl folder points vertices


toRadius =
    .circle >> Circle2d.radius


nonEmptyMaximum head tail =
    Quantity.max head (Maybe.withDefault head (Quantity.maximum tail))


config : QuadTree.Config (Quantity Float Pixels) (Vertex a)
config =
    { toPoint = .circle >> Circle2d.centerPoint
    , combineVertices =
        \vertex vertices ->
            nonEmptyMaximum (toRadius vertex) (List.map toRadius vertices)
    , combineAggregates =
        \radius radii ->
            nonEmptyMaximum radius radii
    }


collision : Float -> Int -> List (Vertex comparable) -> Array (Vertex comparable)
collision strength iters vertices =
    let
        withAggregates =
            QuadTree.fromList (.circle >> Circle2d.centerPoint) vertices
                |> QuadTree.performAggregate config

        apply vertex velocities =
            applyForce
                strength
                withAggregates
                velocities
                { vertex
                    | circle = Circle2d.translateBy vertex.velocity vertex.circle
                }
    in
    nTimes (\velocities -> List.foldl apply velocities vertices) iters (Array.fromList vertices)


nTimes : (a -> a) -> Int -> a -> a
nTimes fn times input =
    if times <= 0 then
        input

    else
        nTimes fn (times - 1) (fn input)


applyForce : Float -> QuadTree (Quantity Float Pixels) (Vertex comparable) -> Array (Vertex comparable) -> Vertex comparable -> Array (Vertex comparable)
applyForce strength qtree velocities node =
    case qtree of
        QuadTree.Empty ->
            velocities

        QuadTree.Leaf leaf ->
            let
                ( head, tail ) =
                    leaf.children
            in
            List.foldl
                (\vertex velos ->
                    if vertex.index > node.index then
                        let
                            nodeNextCenterPoint =
                                Circle2d.centerPoint node.circle |> Point2d.translateBy node.velocity

                            xy =
                                Vector2d.from (Circle2d.centerPoint vertex.circle) nodeNextCenterPoint
                                    |> jiggleVector

                            rj =
                                Circle2d.radius vertex.circle

                            ri =
                                Circle2d.radius node.circle

                            r =
                                Quantity.plus rj ri

                            l =
                                Vector2d.length xy
                        in
                        if l |> Quantity.lessThan r then
                            let
                                lp =
                                    r |> Quantity.minus l |> Quantity.per l |> Quantity.multiplyBy strength

                                rp =
                                    Quantity.ratio (Quantity.squared rj) (Quantity.plus (Quantity.squared rj) (Quantity.squared ri))

                                repelantVector =
                                    Vector2d.at lp xy
                            in
                            velos
                                |> arrayUpdate node.index (updateVelocity (Vector2d.plus (repelantVector |> Vector2d.scaleBy rp)))
                                |> arrayUpdate vertex.index (updateVelocity (Vector2d.minus (repelantVector |> Vector2d.scaleBy (1 - rp))))

                        else
                            velos

                    else
                        velos
                )
                velocities
                (head :: tail)

        QuadTree.Node box ->
            if BoundingBox2d.separatedByAtLeast box.aggregate box.boundingBox (Circle2d.boundingBox node.circle) then
                -- No way are any child objects touching, so we can stop traversal here
                velocities

            else
                -- IGNORE TCO
                applyForce strength box.se (applyForce strength box.sw (applyForce strength box.ne (applyForce strength box.nw velocities node) node) node) node


updateVelocity : (Vector2d Pixels UserCoords -> Vector2d Pixels UserCoords) -> Vertex comparable -> Vertex comparable
updateVelocity fn vert =
    { vert | velocity = fn vert.velocity }


arrayUpdate : Int -> (a -> a) -> Array a -> Array a
arrayUpdate index fn arr =
    case Array.get index arr of
        Just v ->
            Array.set index (fn v) arr

        Nothing ->
            arr
