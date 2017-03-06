module Visualization.Force exposing (..)

import Dict exposing (Dict)
import Svg.Attributes exposing (y)
import Time exposing (Time)


type alias Entity a =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , fx : Maybe Float
    , fy : Maybe Float
    , value : a
    }


entity : a -> Entity a
entity a =
    { x = 0.0, y = 0.0, vx = 0.0, vy = 0.0, fx = Nothing, fy = Nothing, value = a }


initialRadius =
    10


initialAngle =
    pi * (3 - sqrt 5)


initialPositions : List (Entity a) -> List (Entity a)
initialPositions =
    let
        initialize index entity =
            if entity.x == 0 && entity.y == 0 then
                let
                    radius =
                        toFloat index * initialRadius

                    angle =
                        toFloat index * initialAngle
                in
                    { entity
                        | x = radius * cos angle
                        , y = radius * sin angle
                    }
            else
                entity
    in
        List.indexedMap initialize


applyForce : Force comparable -> ( Dict comparable (Entity a), List (Force comparable) ) -> ( Dict comparable (Entity a), List (Force comparable) )
applyForce force ( entities, forces ) =
    let
        ( newEntities, newForce ) =
            case force of
                Center x y ->
                    let
                        ( sumx, sumy ) =
                            Dict.foldr (\_ ent ( sx, sy ) -> ( sx + ent.x, sy + ent.y )) ( 0, 0 ) entities

                        n =
                            toFloat <| Dict.size entities

                        sx =
                            sumx / n - x

                        sy =
                            sumy / n - y
                    in
                        ( Dict.map (\_ ent -> { ent | x = ent.x - sx, y = ent.y - sy }) entities, Center x y )

                Collision float collisionParamidDict ->
                    Debug.crash "not implemented"

                Links idLinkParamList ->
                    Debug.crash "not implemented"

                ManyBody float float2 float3 manyBodyParamidDict ->
                    Debug.crash "not implemented"

                X directionalParamidDict ->
                    Debug.crash "not implemented"

                Y directionalParamidDict ->
                    Debug.crash "not implemented"
    in
        ( newEntities, newForce :: forces )


tick : State comparable -> Dict comparable (Entity a) -> ( State comparable, Dict comparable (Entity a) )
tick (State state) nodes =
    let
        alpha =
            state.alpha + (state.alphaTarget - state.alpha) * state.alphaDecay

        ( newNodes, newForces ) =
            List.foldr applyForce ( nodes, [] ) state.forces

        updateEntity _ ent =
            { ent
                | x = ent.x + ent.vx * state.velocityDecay
                , vx = ent.vx * state.velocityDecay
                , y = ent.y + ent.vy * state.velocityDecay
                , vy = ent.vy * state.velocityDecay
            }
    in
        ( State { state | alpha = alpha, forces = newForces }, Dict.map updateEntity newNodes )


simulation : List (Force comparable) -> State comparable
simulation forces =
    State
        { forces = forces
        , alpha = 0.0
        , minAlpha = 0.0
        , alphaDecay = 0.0
        , alphaTarget = 0.0
        , velocityDecay = 0.0
        }


type State comparable
    = State
        { forces : List (Force comparable)
        , alpha : Float
        , minAlpha : Float
        , alphaDecay : Float
        , alphaTarget : Float
        , velocityDecay : Float
        }


type alias CollisionParam =
    { radius : Float
    , strength : Float
    }


type alias LinkParam comparable =
    { source : comparable
    , target : comparable
    , distance : Float
    , strength : Maybe Float
    }


type alias ManyBodyParam =
    { strength : Float
    }


type alias DirectionalParam =
    { force : Float
    , position : Float
    }


type Force comparable
    = Center Float Float
    | Collision Float (Dict comparable CollisionParam)
    | Links (List (LinkParam comparable))
    | ManyBody Float Float Float (Dict comparable ManyBodyParam)
    | X (Dict comparable DirectionalParam)
    | Y (Dict comparable DirectionalParam)



--
--
--
--
-- type alias Force =
--     Float -> List Node -> List Node
--
--
--
--
-- simulation : List Force -> List Node -> SimulationState
-- simulationWithOptions : Options -> List Force -> List Node -> SimulationState
--
-- step : Simulation -> SimulationState -> SimulationState
--
--
--
-- nodes : SimulationState -> List Node
