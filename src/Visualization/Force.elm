module Visualization.Force
    exposing
        ( entity
        , tick
        , simulation
        , isCompleted
        , reheat
        , center
        , manyBody
        , manyBodyStrength
        , links
        , customLinks
        , Entity
        , State
        , Force
        , computeSimulation
        )

{-| This module implements a velocity Verlet numerical integrator for simulating physical forces on particles.
The simulation is simplified: it assumes a constant unit time step *Δt = 1* for each step, and a constant unit
mass *m = 1* for all particles. As a result, a force *F* acting on a particle is equivalent to a constant
acceleration *a* over the time interval *Δt*, and can be simulated simply by adding to the particle’s velocity,
which is then added to the particle’s position.

In the domain of information visualization, physical simulations are useful for studying networks and hierarchies!

-}

import Dict exposing (Dict)


type alias Entity comparable a =
    { a
        | x : Float
        , y : Float
        , vx : Float
        , vy : Float
        , id : comparable
    }


initialRadius =
    10


initialAngle =
    pi * (3 - sqrt 5)


{-| This will run the entire simulation until it is completed and then returns the entities.
-}
computeSimulation : State comparable -> List (Entity comparable a) -> List (Entity comparable a)
computeSimulation state entities =
    if isCompleted state then
        entities
    else
        let
            ( newState, newEntities ) =
                tick state entities
        in
            computeSimulation newState newEntities


entity : Int -> a -> Entity Int { value : a }
entity index a =
    let
        radius =
            sqrt (toFloat index) * initialRadius

        angle =
            toFloat index * initialAngle
    in
        { x = radius * cos angle
        , y = radius * sin angle
        , vx = 0.0
        , vy = 0.0
        , id = index
        , value = a
        }


applyForce : Float -> Force comparable -> Dict comparable (Entity comparable a) -> Dict comparable (Entity comparable a)
applyForce alpha force entities =
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
                Dict.map (\_ ent -> { ent | x = ent.x - sx, y = ent.y - sy }) entities

        Collision float collisionParamidDict ->
            Debug.crash "not implemented"

        Links iterations links ->
            List.foldl
                (\{ source, target, distance, strength, bias } ents ->
                    case ( Dict.get source ents, Dict.get target ents ) of
                        ( Just sourceNode, Just targetNode ) ->
                            let
                                x =
                                    targetNode.x + targetNode.vx - sourceNode.x - sourceNode.vx

                                y =
                                    targetNode.y + targetNode.vy - sourceNode.y - sourceNode.vy

                                d =
                                    sqrt (x ^ 2 + y ^ 2)

                                l =
                                    (d - distance) / d * alpha * strength
                            in
                                ents
                                    |> Dict.update target (Maybe.map (\sn -> { sn | vx = sn.vx - x * l * bias, vy = sn.vy - y * l * bias }))
                                    |> Dict.update source (Maybe.map (\tn -> { tn | vx = tn.vx + x * l * (1 - bias), vy = tn.vy + y * l * (1 - bias) }))

                        otherwise ->
                            ents
                )
                entities
                links

        ManyBody theta distanceMin2 distanceMax2 entityStrengths ->
            -- TODO: optimize performance with quadtree implementation
            Dict.map
                (\key opEntity ->
                    Dict.foldr
                        (\key2 entity2 entity ->
                            if key /= key2 then
                                let
                                    x =
                                        entity2.x - entity.x

                                    y =
                                        entity2.y - entity.y

                                    l =
                                        x ^ 2 + y ^ 2

                                    strength =
                                        Dict.get key2 entityStrengths
                                            |> Maybe.map .strength
                                            |> Maybe.withDefault 0

                                    w =
                                        strength * alpha / l
                                in
                                    { entity | vx = entity.vx + x * w, vy = entity.vy + y * w }
                            else
                                entity
                        )
                        opEntity
                        entities
                )
                entities

        X directionalParamidDict ->
            Debug.crash "not implemented"

        Y directionalParamidDict ->
            Debug.crash "not implemented"


tick : State comparable -> List (Entity comparable a) -> ( State comparable, List (Entity comparable a) )
tick (State state) nodes =
    let
        alpha =
            state.alpha + (state.alphaTarget - state.alpha) * state.alphaDecay

        dictNodes =
            List.foldl (\node -> Dict.insert node.id node) Dict.empty nodes

        newNodes =
            List.foldl (applyForce alpha) dictNodes state.forces

        updateEntity ent =
            { ent
                | x = ent.x + ent.vx * state.velocityDecay
                , vx = ent.vx * state.velocityDecay
                , y = ent.y + ent.vy * state.velocityDecay
                , vy = ent.vy * state.velocityDecay
            }
    in
        ( State { state | alpha = alpha }, List.map updateEntity <| Dict.values newNodes )


simulation : List (Force comparable) -> State comparable
simulation forces =
    State
        { forces = forces
        , alpha = 1.0
        , minAlpha = 0.001
        , alphaDecay = 1 - 0.001 ^ (1 / 300)
        , alphaTarget = 0.0
        , velocityDecay = 0.6
        }


isCompleted : State comparable -> Bool
isCompleted (State { alpha, minAlpha }) =
    alpha <= minAlpha


reheat : State comparable -> State comparable
reheat (State config) =
    State { config | alpha = 1.0 }


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
    , strength : Float
    , bias : Float
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
    | Links Int (List (LinkParam comparable))
    | ManyBody Float Float Float (Dict comparable ManyBodyParam)
    | X (Dict comparable DirectionalParam)
    | Y (Dict comparable DirectionalParam)


center : Float -> Float -> Force comparable
center =
    Center


manyBody : List comparable -> Force comparable
manyBody =
    manyBodyStrength -30


manyBodyStrength : Float -> List comparable -> Force comparable
manyBodyStrength strength =
    ManyBody 0.9 1 (1 / 0) << Dict.fromList << List.map (\key -> ( key, { strength = strength } ))


links : List { source : comparable, target : comparable } -> Force comparable
links =
    List.map (\{ source, target } -> { source = source, target = target, distance = 30, strength = Nothing }) >> customLinks


customLinks : List { source : comparable, target : comparable, distance : Float, strength : Maybe Float } -> Force comparable
customLinks list =
    let
        counts =
            List.foldr
                (\{ source, target } d ->
                    d
                        |> Dict.update source
                            (Just << Maybe.withDefault 1 << Maybe.map ((+) 1))
                        |> Dict.update target
                            (Just << Maybe.withDefault 1 << Maybe.map ((+) 1))
                )
                Dict.empty
                list

        count key =
            Dict.get key counts |> Maybe.withDefault 0
    in
        list
            |> List.map
                (\{ source, target, distance, strength } ->
                    { source = source
                    , target = target
                    , distance = distance
                    , strength = Maybe.withDefault (1 / min (count source) (count target)) strength
                    , bias = count source / (count source + count target)
                    }
                )
            |> Links 1
