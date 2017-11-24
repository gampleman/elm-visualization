module Visualization.Force
    exposing
        ( entity
        , tick
        , simulation
        , isCompleted
        , reheat
        , iterations
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

<a href="http://code.gampleman.eu/elm-visualization/ForceDirectedGraph/"><img style="max-width: 100%" src="http://code.gampleman.eu/elm-visualization/ForceDirectedGraph/preview@2x.png" alt="force directed graph illustration" /></a>

In the domain of information visualization, physical simulations are useful for studying networks and hierarchies!


## Simulation

@docs Entity, entity, simulation, State, isCompleted, reheat, iterations, computeSimulation, tick


## Forces

@docs Force, center, links, customLinks, manyBody, manyBodyStrength

-}

import Dict exposing (Dict)


{-| Visualization.Force needs to compute and update positions and velocities on any objects that it is simulating.
However, you can use your own data structure to manage these, as long as the individual objects expose the necessary
properties. Therefore this type alias is an extensible record allowing you to avoid excessive nesting.

The `id` property must be unique among objects, otherwise some of the colliding objects will be ignored by the simulation.

Also take care when initializing the positions so that the points don't overlap.

-}
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


{-| This will run the entire simulation until it is completed and then returns the entities. Essentially keeps calling
`tick` until the simulation is done.

Note that computing these is fairly computationally expensive and may freeze the UI for a while if the dataset is large.

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


{-| This is a conveninence function for wrapping data up as Entities. The initial position of entities is arranged
in a [phylotaxic pattern](http://code.gampleman.eu/elm-visualization/Petals/).
-}
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


{-| Advances the simulation a single tick, returning both updated entities and a new State of the simulation.
-}
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


{-| Create a new simulation by passing a list of forces.
-}
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


{-| You can set this to control how quickly the simulation should converge. The default value is 300 iterations.

Lower number of iterations will produce a layout quicker, but risk getting stuck in a local minimum. Higher values take
longer, but typically produce better results.

-}
iterations : Int -> State comparable -> State comparable
iterations iterations (State config) =
    State { config | alphaDecay = 1 - config.minAlpha ^ (1 / toFloat iterations) }


{-| Resets the computation. This is useful if you need to change the parameters at runtime, such as the position or
velocity of nodes during a drag operation.
-}
reheat : State comparable -> State comparable
reheat (State config) =
    State { config | alpha = 1.0 }


{-| Has the simulation stopped?
-}
isCompleted : State comparable -> Bool
isCompleted (State { alpha, minAlpha }) =
    alpha <= minAlpha


{-| This holds internal state of the simulation.
-}
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


{-| A force modifies nodes’ positions or velocities; in this context, a force can apply a classical physical force such
as electrical charge or gravity, or it can resolve a geometric constraint, such as keeping nodes within a bounding box
or keeping linked nodes a fixed distance apart.
-}
type Force comparable
    = Center Float Float
    | Collision Float (Dict comparable CollisionParam)
    | Links Int (List (LinkParam comparable))
    | ManyBody Float Float Float (Dict comparable ManyBodyParam)
    | X (Dict comparable DirectionalParam)
    | Y (Dict comparable DirectionalParam)


{-| The centering force translates nodes uniformly so that the mean position of all nodes (the center of mass) is at
the given position ⟨x,y⟩. This force modifies the positions of nodes on each application; it does not modify velocities,
as doing so would typically cause the nodes to overshoot and oscillate around the desired center. This force helps keep
nodes in the center of the viewport, and it does not distort their relative positions.
-}
center : Float -> Float -> Force comparable
center =
    Center


{-| The many-body (or n-body) force applies mutually amongst all nodes. It can be used to simulate gravity (attraction)
if the strength is positive, or electrostatic charge (repulsion) if the strength is negative.

Unlike links, which only affect two linked nodes, the charge force is global: it affects all nodes whose ids are passed
to it.

The default strength is -30 simulating a repulsing charge.

-}
manyBody : List comparable -> Force comparable
manyBody =
    manyBodyStrength -30


{-| This allows you to specify the strength of the many-body force.
-}
manyBodyStrength : Float -> List comparable -> Force comparable
manyBodyStrength strength =
    ManyBody 0.9 1 (1 / 0) << Dict.fromList << List.map (\key -> ( key, { strength = strength } ))


{-| The link force pushes linked nodes together or apart according to the desired link distance. The strength of the
force is proportional to the difference between the linked nodes’ distance and the target distance, similar to a spring
force.

The link distance here is 30, the strength of the force is proportional to the number of links on each side of the
present link, according to the formule: `1 / min (count souce) (count target)` where `count` if a function that counts
links connected to those nodes.

-}
links : List ( comparable, comparable ) -> Force comparable
links =
    List.map (\( source, target ) -> { source = source, target = target, distance = 30, strength = Nothing }) >> customLinks 1


{-| Allows you to specify the link distance and optionally the strength. You must also specify the iterations count,
however this parameter is currently ignored (so set it to 1). This will change in a future release.
-}
customLinks : Int -> List { source : comparable, target : comparable, distance : Float, strength : Maybe Float } -> Force comparable
customLinks iterations list =
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
            |> Links iterations
