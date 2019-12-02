module Transition exposing
    ( Transition, for, easeFor, constant, step, value, isComplete
    , Easing, easeLinear, easeCubic
    )

{-| Transition is a module for writing animations. It does not attempt to be an animation library for every use case,
it is specifically designed for the needs of data visualization apps. The main idea is that one animates data in some
intermediate form and then leaves the `view` function to display the data as normal.


### Setting up animation in an app

While there are many ways to use this module, a typical setup will look like this:

    import Browser.Events
    import Interpolation exposing (Interpolator)
    import Transition exposing (Transition)

    type alias Model =
        { transition : Transition MyThing
        }

    type Msg
        = Tick Int
        | StartAnimation MyThing

First setup a default transition that doesn't actually do anything:

    init : () -> ( Model, Cmd Msg )
    init () =
        ( { transition = Transition.constant initialThing }
        , Cmd.none
        )

Next setup a subscription:

    subscriptions : Model -> Sub Msg
    subscriptions model =
        if Transition.isComplete model.transition then
            Sub.none

        else
            Browser.Events.onAnimationFrameDelta (round >> Tick)

Define an interpolator for your value:

    interpolateThing : MyThing -> MyThing -> Interpolator MyThing
    interpolateThing from to =
         -- ...

Then handle stuff in your update function:

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            Tick t ->
                ( { model
                    | transition = Transition.step t model.transition
                  }
                , Cmd.none
                )

            StartAnimation newThing ->
                let
                    oldThing =
                        Transition.value model.transition
                in
                ( { model
                    | transition =
                        Transition.for 600 (interpolateThing oldThing newThing)
                  }
                , Cmd.none
                )

Then make your view like normal:

    view : Model -> Html Msg
    view model =
        viewMyThing (Transition.value model.transition)

    viewMyThing : MyThing -> Html Msg
    viewMyThing thing =
        --- ...


## Transitions

@docs Transition, for, easeFor, constant, step, value, isComplete


## Easing

@docs Easing, easeLinear, easeCubic

-}

import Interpolation exposing (Interpolator)


{-| A transition is a smooth interpolation between a beginning state and an end state, with a duration and easing.
-}
type Transition a
    = Transition Int Int Easing (Interpolator a)


{-| A transition that is already complete that will always return the value passed in.
-}
constant : a -> Transition a
constant val =
    Transition 0 0 easeLinear (always val)


{-| Create a transition that will run _for_ a certain number of miliseconds. You need to provide an interpolation between the start and end states.

For example to fade something in for 400ms:

    fadeIn : Item -> Transition Item
    fadeIn item =
        Interpolation.map (\opacity -> { item | opacity = opacity)
            (Interpolation.float 0 1)
            |> Transition.for 400

-}
for : Int -> Interpolator a -> Transition a
for t =
    Transition 0 (abs t) easeCubic


{-| This is like `Transition.for`, but allows one to specify a custom Easing function. `Transition.for` defaults to `Transition.easeCubic`.
-}
easeFor : Int -> Easing -> Interpolator a -> Transition a
easeFor t easing =
    Transition 0 (abs t) easing


{-| Updates the internal state forward by the passed number of miliseconds. You would typically do this in your `update` function.
-}
step : Int -> Transition a -> Transition a
step ms (Transition soFar total easeing interp) =
    Transition (min (soFar + ms) total) total easeing interp


{-| Returns the "current" value. You would typically call this in the view and render whatever this returns.

    import Interpolation

    transition : Transition Int
    transition =
        Transition.easeFor 500 Transition.easeLinear (Interpolation.int 0 10)

    transition |> Transition.value --> 0
    transition |> Transition.step 250 |> Transition.value --> 5
    transition |> Transition.step 600 |> Transition.value --> 10

-}
value : Transition a -> a
value (Transition soFar total (Easing easeing) interp) =
    interp (easeing (toFloat soFar / toFloat total))


{-| Allows you to check if a transition has finished running. This can be used to clean up subscriptions.
-}
isComplete : Transition a -> Bool
isComplete (Transition soFar total _ _) =
    soFar >= total



--
-- stagger :
--     { duration : Int
--     , delay : Int
--     , easing : Easing
--     }
--     -> List (Interpolator a)
--     -> Transition (List a)
-- stagger { duration, delay, easing } interpolations =
--     Debug.todo "not implemeneted yet"


{-| Easing is a method of distorting time to control apparent motion in animation. It is most commonly used for [slow-in, slow-out](https://en.wikipedia.org/wiki/Twelve_basic_principles_of_animation#Slow_In_and_Slow_Out). By easing time, animated transitions are smoother and exhibit more plausible motion.
-}
type Easing
    = Easing (Float -> Float)


{-| Linear easing is esentially the identity function of easing.
-}
easeLinear : Easing
easeLinear =
    Easing identity


easePolynomial : Float -> Easing
easePolynomial exponent =
    Easing
        (\time ->
            let
                t =
                    time * 2
            in
            if t <= 1 then
                t ^ exponent / 2

            else
                ((t - 2) ^ exponent + 2) / 2
        )


{-| Symetric cubic easing. This is quite a good default for a lot of animation.
-}
easeCubic : Easing
easeCubic =
    easePolynomial 3
