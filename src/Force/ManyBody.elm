module Force.ManyBody exposing (AggregateVertex, Vertex, applyForce, config, constructSuperPoint, manyBody, wrapper)

import BoundingBox2d exposing (BoundingBox2d)
import Dict exposing (Dict)
import Force.QuadTree as QuadTree exposing (QuadTree, UserCoords)
import Point2d exposing (Point2d)
import Units.Pixels exposing (Pixels)
import Units.Quantity as Quantity
import Vector2d exposing (Vector2d)


type alias Vertex comparable =
    { key : comparable, position : Point2d Pixels UserCoords, strength : Float, velocity : Vector2d Pixels UserCoords }


type alias AggregateVertex =
    { position : Point2d Pixels UserCoords, strength : Float }


{-| Combine a non-empty list of points into one superpoint
with the average position and accumulated strength
-}
constructSuperPoint :
    { a | position : Point2d Pixels UserCoords, strength : Float }
    -> List { a | position : Point2d Pixels UserCoords, strength : Float }
    -> AggregateVertex
constructSuperPoint first rest =
    let
        initialPoint =
            Point2d.coordinates first.position

        initialStrength =
            first.strength

        folder point ( ( accumX, accumY ), strength, size ) =
            let
                ( x, y ) =
                    Point2d.coordinates point.position
            in
            ( ( accumX |> Quantity.plus x, accumY |> Quantity.plus y ), strength + point.strength, size + 1 )

        ( ( totalX, totalY ), totalStrength, totalSize ) =
            List.foldl folder ( initialPoint, initialStrength, 1 ) rest
    in
    { position = Point2d.xy (totalX |> Quantity.divideBy totalSize) (totalY |> Quantity.divideBy totalSize)
    , strength = totalStrength
    }


config : QuadTree.Config AggregateVertex (Vertex a)
config =
    { toPoint = .position
    , combineVertices = constructSuperPoint
    , combineAggregates = constructSuperPoint
    }


wrapper :
    Float
    -> Float
    -> Dict comparable Float
    -> Dict comparable { point | x : Float, y : Float, vx : Float, vy : Float }
    -> Dict comparable { point | x : Float, y : Float, vx : Float, vy : Float }
wrapper alpha theta strengths points =
    let
        vertices =
            Dict.toList points
                |> List.map
                    (\( key, { x, y } as point ) ->
                        let
                            strength =
                                Dict.get key strengths
                                    |> Maybe.withDefault 0
                        in
                        { position = Point2d.pixels x y, strength = strength, key = key, velocity = Vector2d.zero }
                    )

        newVertices =
            manyBody alpha theta vertices

        updater newVertex maybePoint =
            case maybePoint of
                Nothing ->
                    Nothing

                Just point ->
                    let
                        dv =
                            Vector2d.toPixels newVertex.velocity
                    in
                    Just { point | vx = point.vx + dv.x, vy = point.vy + dv.y }

        folder newVertex pointsDict =
            Dict.update newVertex.key (updater newVertex) pointsDict
    in
    List.foldl folder points newVertices


manyBody : Float -> Float -> List (Vertex comparable) -> List (Vertex comparable)
manyBody alpha theta vertices =
    let
        withAggregates =
            QuadTree.fromList .position vertices
                |> QuadTree.performAggregate config

        updateVertex vertex =
            { vertex | velocity = Vector2d.plus vertex.velocity (applyForce alpha theta withAggregates vertex) }
    in
    List.map updateVertex vertices


applyForce : Float -> Float -> QuadTree.QuadTree AggregateVertex (Vertex comparable) -> Vertex comparable -> Vector2d Pixels UserCoords
applyForce alpha theta qtree vertex =
    let
        -- based on https://en.wikipedia.org/wiki/Barnes%E2%80%93Hut_simulation#Calculating_the_force_acting_on_a_body
        -- when a group of other vertices is sufficiently far away, treat them as one vertex.
        -- its position is the average position, its strength the combined strength
        isFarAway : { a | boundingBox : BoundingBox2d Pixels UserCoords, aggregate : AggregateVertex } -> Bool
        isFarAway treePart =
            let
                ( width, _ ) =
                    BoundingBox2d.dimensions treePart.boundingBox

                distance =
                    Point2d.distanceFrom vertex.position treePart.aggregate.position
            in
            Quantity.ratio width distance < theta

        useAggregate : { a | boundingBox : BoundingBox2d Pixels UserCoords, aggregate : AggregateVertex } -> Vector2d Pixels UserCoords
        useAggregate treePart =
            calculateVelocity vertex treePart.aggregate

        calculateVelocity : { a | position : Point2d Pixels UserCoords } -> { b | position : Point2d Pixels UserCoords, strength : Float } -> Vector2d Pixels UserCoords
        calculateVelocity target source =
            let
                delta =
                    Vector2d.from target.position source.position

                len =
                    Vector2d.length delta |> Units.Pixels.inPixels

                weight =
                    source.strength * alpha / (len ^ 2)
            in
            -- in rare cases, the delta can be the zero vector, and weight becomes NaN
            if isNaN weight then
                Vector2d.zero

            else
                Vector2d.scaleBy weight delta
    in
    case qtree of
        QuadTree.Empty ->
            Vector2d.zero

        QuadTree.Leaf leaf ->
            if isFarAway leaf then
                useAggregate leaf

            else
                let
                    ( first, rest ) =
                        leaf.children

                    applyForceFromPoint point accum =
                        -- don't distribute force to yourself
                        if point.key == vertex.key then
                            accum

                        else
                            Vector2d.plus (calculateVelocity vertex point) accum
                in
                List.foldl applyForceFromPoint Vector2d.zero (first :: rest)

        QuadTree.Node node ->
            if isFarAway node then
                useAggregate node

            else
                let
                    helper tree =
                        applyForce alpha theta tree vertex
                in
                Vector2d.sum
                    [ helper node.nw
                    , helper node.ne
                    , helper node.se
                    , helper node.sw
                    ]
