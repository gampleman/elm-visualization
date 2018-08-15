module ManyBody exposing (..)

import Dict exposing (Dict)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)
import OpenSolid.BoundingBox2d as BoundingBox2d exposing (BoundingBox2d)
import AggregateQuadTree exposing (AggregateQuadTree)


type alias ManyBody comparable =
    { theta : Float
    , distanceMinSquared : Float
    , distanceMaxSquared : Float
    , entityStrengths : Dict comparable { strength : Float }
    }


type alias AggregateVertex =
    { position : Point2d, strength : Float }


type alias Vertex comparable =
    { key : comparable, position : Point2d, strength : Float, velocity : Vector2d }


config : AggregateQuadTree.Config AggregateVertex (Vertex a)
config =
    let
        -- combine a non-empty list of points into one superpoint
        -- with the average position and accumulated strength
        combiner =
            \first rest ->
                let
                    ( initialX, initialY ) =
                        Point2d.coordinates first.position

                    initialStrength =
                        first.strength

                    folder point ( accumX, accumY, strength, size ) =
                        let
                            ( x, y ) =
                                Point2d.coordinates point.position
                        in
                            ( accumX + x, accumY + y, strength + point.strength, size + 1 )

                    ( totalX, totalY, totalStrength, totalSize ) =
                        List.foldl folder ( initialX, initialY, initialStrength, 1 ) rest
                in
                    { position = Point2d.fromCoordinates ( totalX / totalSize, totalY / totalSize )
                    , strength = totalStrength / totalSize
                    }
    in
        { toPoint = .position
        , combineVertices = combiner
        , combineAggregates = combiner
        }


wrapper :
    Float
    -> Float
    -> Dict comparable { strength : Float }
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
                                    |> Maybe.map .strength
                                    |> Maybe.withDefault 0
                        in
                            { position = Point2d.fromCoordinates ( x, y ), strength = strength, key = key, velocity = Vector2d.zero }
                    )

        newVertices =
            manyBody alpha theta vertices

        updater newVertex maybePoint =
            case maybePoint of
                Nothing ->
                    Nothing

                Just point ->
                    let
                        ( dvx, dvy ) =
                            Vector2d.components newVertex.velocity
                    in
                        Just { point | vx = point.vx + dvx, vy = point.vy + dvy }

        folder newVertex pointsDict =
            Dict.update newVertex.key (updater newVertex) pointsDict
    in
        List.foldl folder points newVertices


manyBody : Float -> Float -> List (Vertex comparable) -> List (Vertex comparable)
manyBody alpha theta vertices =
    let
        withAggregates =
            (AggregateQuadTree.fromList .position vertices)
                |> AggregateQuadTree.aggregate config

        updateVertex vertex =
            { vertex | velocity = Vector2d.sum vertex.velocity (applyForce alpha theta withAggregates vertex) }
    in
        List.map updateVertex vertices


applyForce : Float -> Float -> AggregateQuadTree.AggregateQuadTree AggregateVertex (Vertex comparable) -> Vertex comparable -> Vector2d
applyForce alpha theta qtree vertex =
    let
        -- based on https://en.wikipedia.org/wiki/Barnes%E2%80%93Hut_simulation#Calculating_the_force_acting_on_a_body
        -- when a group of other vertices is sufficiently far away, treat them as one vertex.
        -- its position is the average position, its strength the combined strength
        isFarAway : { a | boundingBox : BoundingBox2d, aggregate : AggregateVertex } -> Bool
        isFarAway treePart =
            let
                ( s, _ ) =
                    BoundingBox2d.dimensions treePart.boundingBox

                d =
                    Point2d.distanceFrom vertex.position treePart.aggregate.position
            in
                s / d < theta

        useAggregate : { a | boundingBox : BoundingBox2d, aggregate : AggregateVertex } -> Vector2d
        useAggregate treePart =
            let
                delta =
                    Vector2d.from vertex.position treePart.aggregate.position

                weight =
                    treePart.aggregate.strength * alpha / (Vector2d.squaredLength delta)
            in
                -- in rare cases, the delta can be the zero vector, and weight becomes NaN
                if isNaN weight then
                    Vector2d.zero
                else
                    Vector2d.scaleBy weight delta
    in
        case qtree of
            AggregateQuadTree.Empty ->
                Vector2d.zero

            AggregateQuadTree.Leaf leaf ->
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
                                let
                                    delta =
                                        Vector2d.from vertex.position point.position

                                    weight =
                                        point.strength * alpha / (Vector2d.squaredLength delta)
                                in
                                    -- in rare cases, the delta can be the zero vector, and weight becomes NaN
                                    if isNaN weight then
                                        accum
                                    else
                                        Vector2d.sum (Vector2d.scaleBy weight delta) accum
                    in
                        List.foldl applyForceFromPoint Vector2d.zero (first :: rest)

            AggregateQuadTree.Node node ->
                if isFarAway node then
                    useAggregate node
                else
                    let
                        helper tree =
                            applyForce alpha theta tree vertex
                    in
                        helper node.nw
                            |> Vector2d.sum (helper node.ne)
                            |> Vector2d.sum (helper node.se)
                            |> Vector2d.sum (helper node.sw)
