module Transition exposing
    ( Transition, for, easeFor, constant, step, value, isComplete, reverse
    , repeat, repeatAlternate, repeatIndefinitely, repeatAlternateIndefinitely
    , stagger
    , Easing, easeLinear, easeCubic, easePolynomialIn, easePolynomialOut, easePolynomial, easeSinusoidalIn, easeSinusoidalOut, easeSinusoidal, easeExponentialIn, easeExponentialOut, easeExponential, easeCircleIn, easeCircleOut, easeCircle, easeElasticIn, easeElasticOut, easeElastic, easeBackIn, easeBackOut, easeBack, easeBounceIn, easeBounceOut, easeBounce
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

@docs Transition, for, easeFor, constant, step, value, isComplete, reverse


## Repetition

@docs repeat, repeatAlternate, repeatIndefinitely, repeatAlternateIndefinitely


## Staggered animation

@docs stagger


## Easing

@docs Easing, easeLinear, easeCubic, easePolynomialIn, easePolynomialOut, easePolynomial, easeSinusoidalIn, easeSinusoidalOut, easeSinusoidal, easeExponentialIn, easeExponentialOut, easeExponential, easeCircleIn, easeCircleOut, easeCircle, easeElasticIn, easeElasticOut, easeElastic, easeBackIn, easeBackOut, easeBack, easeBounceIn, easeBounceOut, easeBounce

-}

import Interpolation exposing (Interpolator)


{-| A transition is a smooth interpolation between a beginning state and an end state, with a duration and easing.
-}
type Transition a
    = Transition
        { soFar : Int
        , total : Int
        , repetitions : Int
        , completed : Int
        , repeatType : Repeat
        , easing : Float -> Float
        , interpolator : Interpolator a
        }


type Repeat
    = Bounce
    | FromBeginning


{-| A transition that is already complete that will always return the value passed in.
-}
constant : a -> Transition a
constant val =
    easeFor 0 easeLinear (always val)


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
    easeFor t easeCubic


{-| This is like `Transition.for`, but allows one to specify a custom Easing function. `Transition.for` defaults to `Transition.easeCubic`.
-}
easeFor : Int -> Easing -> Interpolator a -> Transition a
easeFor t (Easing easing) interpolator =
    Transition
        { soFar = 0
        , total = abs t
        , repetitions = 1
        , completed = 0
        , repeatType = FromBeginning
        , easing = easing
        , interpolator = interpolator
        }


{-| Updates the internal state forward by the passed number of miliseconds. You would typically do this in your `update` function.
-}
step : Int -> Transition a -> Transition a
step ms (Transition transition) =
    if ms + transition.soFar > transition.total then
        if transition.repetitions + 1 >= transition.completed then
            Transition { transition | soFar = transition.total }

        else
            case transition.repeatType of
                Bounce ->
                    Transition
                        { transition
                            | soFar = (transition.soFar - ms) - transition.total
                            , repetitions = transition.repetitions + 1
                            , interpolator = \t -> transition.interpolator (1 - t)
                        }

                FromBeginning ->
                    Transition
                        { transition
                            | soFar = (transition.soFar - ms) - transition.total
                            , repetitions = transition.repetitions + 1
                        }

    else
        Transition { transition | soFar = transition.soFar + ms }


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
value (Transition { soFar, total, easing, interpolator }) =
    interpolator (easing (toFloat soFar / toFloat total))


{-| Allows you to check if a transition has finished running. This can be used to clean up subscriptions.
-}
isComplete : Transition a -> Bool
isComplete (Transition t) =
    t.completed + 1 >= t.repetitions && t.soFar >= t.total


{-| Reverses the direction of a transition running it backwards.
-}
reverse : Transition a -> Transition a
reverse (Transition transition) =
    Transition
        { transition
            | soFar = transition.total - transition.soFar
            , interpolator = \t -> transition.interpolator (1 - t)
            , completed = transition.repetitions - (transition.completed + 1)
        }



-- Repetition


{-| Repeat the transition a number of times.
-}
repeat : Int -> Transition a -> Transition a
repeat n (Transition transition) =
    Transition { transition | repetitions = n, repeatType = FromBeginning }


{-| Repeat the transition a set number of times, but run every even run in reverse.
-}
repeatAlternate : Int -> Transition a -> Transition a
repeatAlternate n (Transition transition) =
    Transition { transition | repetitions = n, repeatType = Bounce }


{-| Keep running the transition.
-}
repeatIndefinitely : Transition a -> Transition a
repeatIndefinitely =
    repeat (round (1 / 0))


{-| Keep running the transition indefinitely, but alternating forward and reverse runs.
-}
repeatAlternateIndefinitely : Transition a -> Transition a
repeatAlternateIndefinitely =
    repeatAlternate (round (1 / 0))


{-| Run a bunch of animations with a duration and easing, but delay each successive animation by the delay amount.

**Tip:** You may find the `List (Interpolator a) -> Transition (List a)` a little inconvenient for organizing staggered animations.
However, you can create an `List (Interpolator (a -> a))`, where each function touches and interpolates some orthogonal property,
then `List.foldl (\fn val -> fn val) model.target (Transition.value model.transition)`. This way you can stagger updates to almost any
datastructure. However, you need to be somewhat careful if there are other ways the datastructure can be changed (like user input) to
make sure to interupt the animation suitably.

    type Foo =
        { position: (Float, Float)
        , color : Color
        }

    [ \t foo -> { foo | position = interpolatePosition t }
    , \t foo -> { foo | color = Interpolation.rgb Color.blue Color.red t }
    ]
    |> Transition.stagger { durationEach = 200, delay = 100, easing Transition.easingCubic }

Will first start animating the position, then after 100ms start animating the color, for a total duration of 300ms.

-}
stagger :
    { durationEach : Int
    , delay : Int
    , easing : Easing
    }
    -> List (Interpolator a)
    -> Transition (List a)
stagger { durationEach, delay, easing } interpolations =
    let
        n =
            List.length interpolations

        duration =
            ((n - 1) * delay) + durationEach

        parallelism =
            toFloat (durationEach * n) / toFloat (delay * n)

        (Easing ease) =
            easing
    in
    Transition
        { soFar = 0
        , total = abs duration
        , repetitions = 1
        , completed = 0
        , repeatType = FromBeginning
        , easing = identity
        , interpolator = Interpolation.staggeredWithParallelism parallelism (List.map (\interp -> ease >> interp) interpolations)
        }


{-| Easing is a method of distorting time to control apparent motion in animation. It is most commonly used for [slow-in, slow-out](https://en.wikipedia.org/wiki/Twelve_basic_principles_of_animation#Slow_In_and_Slow_Out). By easing time, animated transitions are smoother and exhibit more plausible motion.
-}
type Easing
    = Easing (Float -> Float)


{-| Linear easing is esentially the identity function of easing.
-}
easeLinear : Easing
easeLinear =
    Easing identity


{-| Polynomial easing; raises _t_ to the provided exponent.
-}
easePolynomialIn : Float -> Easing
easePolynomialIn exponent =
    Easing
        (\time ->
            time ^ exponent
        )


{-| Reverse polynomial easing; equivalent to `1 - easePolynomialIn (1 - t)`.
-}
easePolynomialOut : Float -> Easing
easePolynomialOut exponent =
    Easing
        (\time ->
            1 - (1 - time) ^ exponent
        )


{-| Symmetric polynomial easing. In _t_ [0, 0.5] equivalent to `easePolynomialIn` in [0.5, 1] `easePolynomialOut`
-}
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


{-| Symetric cubic easing. This is quite a good default for a lot of animation. Equivalent to `easePolynomial 3`
-}
easeCubic : Easing
easeCubic =
    easePolynomial 3


halfPi : Float
halfPi =
    pi / 2


{-| Sinusoidal easing; returns sin(t).
-}
easeSinusoidalIn : Easing
easeSinusoidalIn =
    Easing (\time -> 1 - cos (time * halfPi))


{-| Reverse sinusoidal easing; equivalent to 1 - sinIn(1 - t).
-}
easeSinusoidalOut : Easing
easeSinusoidalOut =
    Easing (\time -> sin (time * halfPi))


{-| Symmetric sinusoidal easing
-}
easeSinusoidal : Easing
easeSinusoidal =
    Easing (\time -> (1 - cos (pi * time)) / 2)


{-| Exponential easing; raises 2 to the exponent 10 \* (t - 1).
-}
easeExponentialIn : Easing
easeExponentialIn =
    Easing (\time -> 2 ^ (10 * time - 10))


{-| Reverse exponential easing.
-}
easeExponentialOut : Easing
easeExponentialOut =
    Easing (\time -> 1 - 2 ^ (-10 * time))


{-| Symetric exponential easing.
-}
easeExponential : Easing
easeExponential =
    Easing
        (\time ->
            let
                t =
                    time * 2
            in
            (if t <= 1 then
                2 ^ (10 * time - 10)

             else
                1 - 2 ^ (-10 * t)
            )
                / 2
        )


{-| Circular easing.
-}
easeCircleIn : Easing
easeCircleIn =
    Easing (\t -> 1 - sqrt (1 - t ^ 2))


{-| Reverse circular easing.
-}
easeCircleOut : Easing
easeCircleOut =
    Easing (\t -> sqrt (1 - (t - 1) ^ 2))


{-| Symetric circular easing.
-}
easeCircle : Easing
easeCircle =
    Easing
        (\time ->
            let
                t =
                    time * 2
            in
            (if t <= 1 then
                1 - sqrt (1 - t ^ 2)

             else
                sqrt (1 - (t - 2) ^ 2) + 1
            )
                / 2
        )


tau : Float
tau =
    pi * 2


{-| Elastic easing, like a rubber band. Reasonable defaults for the parameters would be `{ amplitude = 1, period = 0.3 }`.
-}
easeElasticIn : { amplitude : Float, period : Float } -> Easing
easeElasticIn { amplitude, period } =
    let
        a =
            max 1 amplitude

        p =
            period / tau

        s =
            asin (1 / a) * p
    in
    Easing (\t -> a * 2 ^ (10 * (t - 1)) * sin ((s / t) / p))


{-| Reverse elastic easing.
-}
easeElasticOut : { amplitude : Float, period : Float } -> Easing
easeElasticOut { amplitude, period } =
    let
        a =
            max 1 amplitude

        p =
            period / tau

        s =
            asin (1 / a) * p
    in
    Easing (\t -> 1 - a * 2 ^ (-10 * t) * sin ((t + s) / p))


{-| Symmetric elastic easing.
-}
easeElastic : { amplitude : Float, period : Float } -> Easing
easeElastic { amplitude, period } =
    let
        a =
            max 1 amplitude

        p =
            period / tau

        s =
            asin (1 / a) * p
    in
    Easing
        (\time ->
            let
                t =
                    time * 2 - 1
            in
            (if t < 0 then
                a * 2 ^ (10 * t) * sin ((s - t) / p)

             else
                2 - a * 2 ^ (-10 * t) * sin ((s + t) / p)
            )
                / 2
        )


{-| [Anticipatory](https://en.wikipedia.org/wiki/Twelve_basic_principles_of_animation#Anticipation) easing, like a dancer bending his knees before jumping off the floor. The degree of overshoot is configurable. A reasonable default is 1.70158. [This represents about 10% more than the difference between the numbers](http://void.heteml.jp/blog/archives/2014/05/easing_magicnumber.html).
-}
easeBackIn : Float -> Easing
easeBackIn s =
    Easing (\t -> t * t * ((s + 1) * t - s))


{-| Reverse anticipatory easing.
-}
easeBackOut : Float -> Easing
easeBackOut s =
    Easing
        (\time ->
            let
                t =
                    time - 1
            in
            t * t * ((s + 1) * t + s) + 1
        )


{-| Symmetric anticipatory easing.
-}
easeBack : Float -> Easing
easeBack s =
    Easing
        (\time ->
            let
                t =
                    time * 2
            in
            (if t < 1 then
                t * t * ((s + 1) * t - s)

             else
                let
                    tp =
                        t - 2
                in
                tp * tp * ((s + 1) * tp + s) + 2
            )
                / 2
        )



-- Bounce


b1 : Float
b1 =
    4 / 11


b2 : Float
b2 =
    6 / 11


b3 : Float
b3 =
    8 / 11


b4 : Float
b4 =
    3 / 4


b5 : Float
b5 =
    9 / 11


b6 : Float
b6 =
    10 / 11


b7 : Float
b7 =
    15 / 16


b8 : Float
b8 =
    21 / 22


b9 : Float
b9 =
    63 / 64


b0 : Float
b0 =
    1 / b1 / b1


bounceOut : Float -> Float
bounceOut t =
    if t < b1 then
        b0 * t * t

    else if t < b3 then
        b0 * (t - b2) ^ 2 + b4

    else if t < b6 then
        b0 * (t - b5) ^ 2 + b7

    else
        b0 * (t - b8) ^ 2 + b9


{-| Bounce easing, like a rubber ball.
-}
easeBounceIn : Easing
easeBounceIn =
    Easing (\t -> 1 - bounceOut (1 - t))


{-| Reverse bounce easing.
-}
easeBounceOut : Easing
easeBounceOut =
    Easing bounceOut


{-| Symmetric bounce easing.
-}
easeBounce : Easing
easeBounce =
    Easing
        (\time ->
            let
                t =
                    time * 2
            in
            (if t <= 1 then
                1 - bounceOut (1 - t)

             else
                bounceOut (t - 1) + 1
            )
                / 2
        )
