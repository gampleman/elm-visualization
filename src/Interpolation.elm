module Interpolation exposing
    ( Interpolator
    , float, int, step, rgb, rgbWithGamma, hsl, hslLong, lab, hcl, hclLong
    , map, map2, map3, map4, map5, piecewise, tuple
    , inParallel, staggeredWithParallelism, list, ListCombiner(..), combineParallel
    , pointAlongPath
    , samples
    )

{-| This module provides a variety of interpolation methods for blending between two values.
While primitives for numbers, colors and lists are provided, the library focuses on composition
so that you can build interpolators for your own custom datatypes.

@docs Interpolator


### Primitive interpolators

@docs float, int, step, rgb, rgbWithGamma, hsl, hslLong, lab, hcl, hclLong


### Composition

@docs map, map2, map3, map4, map5, piecewise, tuple


### Lists

@docs inParallel, staggeredWithParallelism, list, ListCombiner, combineParallel


### Advanced

@docs pointAlongPath


## Helpers

@docs samples

-}

import Array
import Color exposing (Color)
import Color.Lab as Lab
import Dict exposing (Dict)
import LowLevel.Command
import Path exposing (Path)
import Statistics
import SubPath


{-| An interpolator is merely a function that takes a float parameter `t` roughly in the range [0..1].
0 would represent the "before" value, 1 the after value and values in between are the values in between.

Note: Sometimes the range of the interpolator can go slightly above or below zero - this is useful for some
animation techniques. If this is not suitable for your data type, remember to clamp the values as necessary.

-}
type alias Interpolator a =
    Float -> a


{-| Transform values from another interpolator.

Note: This function is provided as a convenience, since thinking in `mapN` is pretty natural for Elm developers (and
works well in pipelines). However, keep in mind that this function is literally an alias for `<<`.

-}
map : (a -> b) -> Interpolator a -> Interpolator b
map =
    (<<)


{-| Combine two interpolators, combining them with the given function.

    type alias Coords =
        ( Float, Float )

    interpolateCoords : Coords -> Coords -> Interpolator Coords
    interpolateCoords ( x1, y1 ) ( x2, y2 ) =
        Interpolation.map2
            Tuple.pair
            (Interpolation.float x1 x2)
            (Interpolation.float y1 y2)

-}
map2 : (a -> b -> c) -> Interpolator a -> Interpolator b -> Interpolator c
map2 fn a b =
    \t -> fn (a t) (b t)


{-| -}
map3 : (a -> b -> c -> d) -> Interpolator a -> Interpolator b -> Interpolator c -> Interpolator d
map3 fn a b c =
    \t -> fn (a t) (b t) (c t)


{-| -}
map4 : (a -> b -> c -> d -> e) -> Interpolator a -> Interpolator b -> Interpolator c -> Interpolator d -> Interpolator e
map4 fn a b c d =
    \t -> fn (a t) (b t) (c t) (d t)


{-| -}
map5 : (a -> b -> c -> d -> e -> f) -> Interpolator a -> Interpolator b -> Interpolator c -> Interpolator d -> Interpolator e -> Interpolator f
map5 fn a b c d e =
    \t -> fn (a t) (b t) (c t) (d t) (e t)


{-| Returns a piecewise interpolator, composing interpolators for each adjacent pair of values.

For example:

    myInterpolator : Interpolator Int
    myInterpolator =
         Interpolation.piecewise Interpolation.int 6 [ 10, -2 ]

    myInterpolator 0 --> 6
    myInterpolator 0.25 --> 8
    myInterpolator 0.5 --> 10
    myInterpolator 0.75 --> 4
    myInterpolator 1 --> -2

-}
piecewise : (a -> a -> Interpolator a) -> a -> List a -> Interpolator a
piecewise makeInterpolator head tail =
    let
        n =
            List.length tail

        interpolators =
            List.foldl (\item ( prev, accu ) -> ( item, makeInterpolator prev item :: accu )) ( head, [] ) tail
                |> Tuple.second
                |> List.reverse
                |> Array.fromList
    in
    \t ->
        let
            tn =
                t * toFloat n

            i =
                clamp 0 (n - 1) (floor tn)
        in
        Array.get i interpolators
            |> Maybe.map (\fn -> fn (tn - toFloat i))
            |> Maybe.withDefault head


{-| Composes interpolators around a tuple. This is a convenience function for the common case of 2 element tuples.

You can for example define an interpolator for a position:

    interpolatePosition : ( Float, Float ) -> ( Float, Float ) -> Interpolator ( Float, Float )
    interpolatePosition =
        Interpolation.tuple Interpolation.float Interpolation.float

-}
tuple : (a -> a -> Interpolator a) -> (b -> b -> Interpolator b) -> ( a, b ) -> ( a, b ) -> Interpolator ( a, b )
tuple ia ib ( fromA, fromB ) ( toA, toB ) =
    map2 Tuple.pair (ia fromA toA) (ib fromB toB)



-- We can also provide an `andThen` function for our Interpolator:
--
--     andThen : (a -> Interpolator b) -> Interpolator a -> Interpolator b
--     andThen fn interpolator =
--           \param ->
--                 fn (interpolator param) param
--
--   However, we have not actually come up with a scenario where this would make sense.
--
--
-- Basic interpolators


{-| Interpolates between the two provided float values.

    myInterpolator : Interpolator Float
    myInterpolator = Interpolation.float 5 17

    myInterpolator 0.2 -- 7.4
    myInterpolator 0.5 -- 11

-}
float : Float -> Float -> Interpolator Float
float a to =
    let
        b =
            to - a
    in
    \t -> a + b * t


{-| Interpolates between ints.
-}
int : Int -> Int -> Interpolator Int
int from to =
    round << float (toFloat from) (toFloat to)


{-| Interpolate between arbitrary values by just showing them in sequence.

The list is provided is passed as head and tail seperately, to avoid needing to handle the empty list case.

     type StageOfGrief
         = Denial
         | Anger
         | Bargaining
         | Depression
         | Acceptance

     griefInterpolator : Interpolator StageOfGrief
     griefInterpolator =
         Interpolation.step Denial
             [ Anger
             , Bargaining
             , Depression
             , Acceptance
             ]

     griefInterpolator 0 --> Denial
     griefInterpolator 0.5 --> Bargaining
     griefInterpolator 1.1 --> Acceptance

-}
step : a -> List a -> Interpolator a
step head tail =
    let
        n =
            List.length tail

        items =
            Array.fromList (head :: tail)

        nt =
            toFloat n + 1
    in
    \t ->
        Array.get (clamp 0 n (floor (t * nt))) items
            |> Maybe.withDefault head



-- Color


{-| Interpolates between two Color values using the sRGB color space.
-}
rgb : Color -> Color -> Interpolator Color
rgb =
    rgbWithGamma 1.0


{-| Interpolates between two Color values using the sRGB color space using [gamma correction](https://web.archive.org/web/20160112115812/http://www.4p8.com/eric.brasseur/gamma.html).
-}
rgbWithGamma : Float -> Color -> Color -> Interpolator Color
rgbWithGamma gamma from to =
    let
        start =
            Color.toRgba from

        end =
            Color.toRgba to

        c =
            gammaCorrected gamma
    in
    map4 Color.rgba (c start.red end.red) (c start.green end.green) (c start.blue end.blue) (float start.alpha end.alpha)


{-| Interpolates between two Color values using the HSL color space. It will always take the shortest path between the target hues.
-}
hsl : Color -> Color -> Interpolator Color
hsl =
    hslImpl hue


{-| Like `Interpolation.hsl`, but does not use the shortest path between hues.
-}
hslLong : Color -> Color -> Interpolator Color
hslLong =
    hslImpl float


hslImpl : (Float -> Float -> Interpolator Float) -> Color -> Color -> Interpolator Color
hslImpl hueInt from to =
    let
        start =
            Color.toHsla from

        end =
            Color.toHsla to
    in
    map4 Color.hsla (hueInt start.hue end.hue) (float start.saturation end.saturation) (float start.lightness end.lightness) (float start.alpha end.alpha)


{-| Interpolates between two Color values using the [CIELAB](https://en.wikipedia.org/wiki/CIELAB_color_space) color space, that is more perceptually linear than other color spaces.
Perceptually linear means that a change of the same amount in a color value should produce a change of about the same visual importance.
This property makes it ideal for accurate visual encoding of data.
-}
lab : Color -> Color -> Interpolator Color
lab from to =
    let
        start =
            Lab.toLab from

        end =
            Lab.toLab to
    in
    map4 (\l a b alpha -> Lab.fromLab { l = l, a = a, b = b, alpha = alpha })
        (float start.l end.l)
        (float start.a end.a)
        (float start.b end.b)
        (float start.alpha end.alpha)


{-| Interpolates between two Color values using the [CIE Lch(ab)](https://en.wikipedia.org/wiki/HCL_color_space) color space.
-}
hcl : Color -> Color -> Interpolator Color
hcl =
    hclImpl hue360


{-| Like hcl, but does not use the shortest path between hues.
-}
hclLong : Color -> Color -> Interpolator Color
hclLong =
    hclImpl float


{-| We do not want negative values in an rgb color
TODO: is ths a non issue or does it hide a problem in the Color.Lab module?
-}
forcePositive : Color -> Color
forcePositive c =
    let
        rgbaColor =
            c |> Color.toRgba
    in
    Color.rgb (clamp 0 1 rgbaColor.red) (clamp 0 1 rgbaColor.green) (clamp 0 1 rgbaColor.blue)


hclImpl : (Float -> Float -> Interpolator Float) -> Color -> Color -> Interpolator Color
hclImpl hueInt from to =
    let
        start =
            Lab.toHcl from

        end =
            Lab.toHcl to
    in
    map4 (\h c l alpha -> Lab.fromHcl { hue = h, chroma = c, luminance = l, alpha = alpha })
        (hueInt start.hue end.hue)
        (float start.chroma end.chroma)
        (float start.luminance end.luminance)
        (float start.alpha end.alpha)
        >> forcePositive


hue : Float -> Float -> Interpolator Float
hue from to =
    let
        d =
            to - from
    in
    float from
        (if d > 0.5 || d < -0.5 then
            from + (d - toFloat (round d))

         else
            to
        )


hue360 : Float -> Float -> Interpolator Float
hue360 from to =
    hue (from / 360) (to / 360) >> (*) 360


gammaCorrected : Float -> Float -> Float -> Interpolator Float
gammaCorrected gamma from to =
    if gamma == 1 then
        float from to

    else
        let
            a =
                from ^ gamma

            b =
                to ^ gamma - a

            y =
                1 / gamma
        in
        \t -> (a + t * b) ^ y



-- Sampling


{-| Returns a list of uniformly spaced samples from the specified interpolator. The first sample is always at t = 0, and the last sample is always at t = 1. This can be useful in generating a fixed number of samples from a given interpolator.

Can be quite handy when debugging interpolators or as a way to create a quantize scale.

-}
samples : Int -> Interpolator a -> List a
samples n interpolator =
    List.map (\i -> interpolator (toFloat i / (toFloat n - 1))) (List.range 0 (n - 1))



-- OK, this gets complex


{-| This will run all of the interpolators provided in parallel.

Can be handy for constructing complex interpolations in conjuction with `List.map2`:

    before : List Float
    before =
         [ 3, 4, 7, 8 ]

    after : List Float
    after =
         [ 6, 4, 1, 9 ]

    myInterpolator0 : Interpolator (List Float)
    myInterpolator0 =
         List.map2 Interpolation.float before after
             |> Interpolation.inParallel

    myInterpolator0 0 --> [ 3, 4, 7, 8 ]
    myInterpolator0 0.5 --> [ 4.5, 4, 4, 8.5]
    myInterpolator0 1 --> [ 6, 4, 1, 9 ]

-}
inParallel : List (Interpolator a) -> Interpolator (List a)
inParallel =
    List.foldr (map2 (::)) (always [])


{-| Combines a bunch of interpolators with a controlled amount of parallelism.

Let's illustrate what we mean by showing an example:

    interpolate : Float -> Interpolator (List Int)
    interpolate parallelism =
        List.repeat 5 (Interpolation.int 0 8)
            |> Interpolation.staggeredWithParallelism parallelism

Now when the parallelism is 1, we will run each interpolator when the previous one concludes:

    Interpolation.samples 11 (interpolate 1)
    --> [ [ 0, 0, 0, 0, 0 ]
    --> , [ 4, 0, 0, 0, 0 ]
    --> , [ 8, 0, 0, 0, 0 ]
    --> , [ 8, 4, 0, 0, 0 ]
    --> , [ 8, 8, 0, 0, 0 ]
    --> , [ 8, 8, 4, 0, 0 ]
    --> , [ 8, 8, 8, 0, 0 ]
    --> , [ 8, 8, 8, 4, 0 ]
    --> , [ 8, 8, 8, 8, 0 ]
    --> , [ 8, 8, 8, 8, 4 ]
    --> , [ 8, 8, 8, 8, 8 ]
    --> ]

If we set it to 2, we will start each interpolator approximately halfway through the previous one:

    Interpolation.samples 11 (interpolate 2)
    --> [ [ 0, 0, 0, 0, 0 ]
    --> , [ 2, 0, 0, 0, 0 ]
    --> , [ 5, 1, 0, 0, 0 ]
    --> , [ 7, 3, 0, 0, 0 ]
    --> , [ 8, 6, 2, 0, 0 ]
    --> , [ 8, 8, 4, 0, 0 ]
    --> , [ 8, 8, 6, 2, 0 ]
    --> , [ 8, 8, 8, 5, 1 ]
    --> , [ 8, 8, 8, 7, 3 ]
    --> , [ 8, 8, 8, 8, 6 ]
    --> , [ 8, 8, 8, 8, 8 ]
    --> ]

If we set it to 1/2, we will wait approximately the time a single interpolator runs after each run doing nothing (slightly more samples so it's easier to see what's going on):

    Interpolation.samples 16 (interpolate 0.5)
    --> [ [ 0, 0, 0, 0, 0 ]
    --> , [ 5, 0, 0, 0, 0 ]
    --> , [ 8, 0, 0, 0, 0 ]
    --> , [ 8, 0, 0, 0, 0 ]
    --> , [ 8, 3, 0, 0, 0 ]
    --> , [ 8, 8, 0, 0, 0 ]
    --> , [ 8, 8, 0, 0, 0 ]
    --> , [ 8, 8, 2, 0, 0 ]
    --> , [ 8, 8, 6, 0, 0 ]
    --> , [ 8, 8, 8, 0, 0 ]
    --> , [ 8, 8, 8, 0, 0 ]
    --> , [ 8, 8, 8, 5, 0 ]
    --> , [ 8, 8, 8, 8, 0 ]
    --> , [ 8, 8, 8, 8, 0 ]
    --> , [ 8, 8, 8, 8, 3 ]
    --> , [ 8, 8, 8, 8, 8 ]
    --> ]

As parallelism approaches infinity, than this will behave like `Interpolation.inParallel`.

If thinking about this in terms of parallelism doesn't feel natural, you may appreciate `Transition.stagger` which deals
with durations and delays instead.

-}
staggeredWithParallelism : Float -> List (Interpolator a) -> Interpolator (List a)
staggeredWithParallelism concurrency lst =
    let
        n =
            toFloat (List.length lst)

        duration =
            (concurrency / n) / (1 + (concurrency - 1) / n)

        offset =
            (1 / concurrency) * duration

        scale offset_ interp t =
            interp (clamp 0 1 ((t - offset_) / duration))
    in
    (List.foldl (\item ( soFar, off ) -> ( map2 (::) (scale off item) soFar, off + offset )) ( always [], 0 ) lst
        |> Tuple.first
    )
        >> List.reverse


{-| This is an interpolator for lists. It is quite complex and should be used if these conditions hold:

1.  You need to interpolate additions, removals and changes.
2.  Each item in the list has some notion of identity, for example an `.id` member, which is `comparable`.
3.  You have a way to deal with positions in the list being somewhat muddy during the transition (e.g. if an item is being created at the same position a different item is being removed, while adjacent items are switching position, then the exact order of items will be arbitrary during the interpolation).

The first argument is a configuration record. It has the following keys:

  - `id : a -> comparable` is a function that retrieves some sort of identifier for each item in the list. It is used to figure out if an item is added, removed, or modified.
  - `add : a -> Interpolator a` will be invoked for each item being added to the list.
  - `remove : a -> Interpolator a` will be invoked for each item disappearing from the list. Note that the item won't actually be removed from the list until `t = 1`, so you will most likely want to make the item disappear visually.
  - `change : a -> a -> Interpolator a` is called for an item where the `id` matches for both lists, but which are not equal.
  - `combine : ListCombiner` configures a strategy that orchestrates all the interpolations created. At the moment only 'combineParallel' is supported, but staggered transitions will be supported in the future.

-}
list :
    { add : a -> Interpolator a
    , remove : a -> Interpolator a
    , change : a -> a -> Interpolator a
    , id : a -> comparable
    , combine : ListCombiner
    }
    -> List a
    -> List a
    -> Interpolator (List a)
list config from to =
    let
        fromIds =
            Dict.fromList (List.indexedMap (\idx a -> ( config.id a, ( idx, a ) )) from)

        toIds =
            Dict.fromList (List.indexedMap (\idx a -> ( config.id a, ( idx, a ) )) to)

        removals : Dict comparable ( Int, a )
        removals =
            Dict.diff fromIds toIds

        additions =
            Dict.diff toIds fromIds
                |> Dict.values
                |> Dict.fromList

        fromMaxIndex =
            List.length from - 1

        onTop =
            Dict.toList additions
                |> List.filter (\( idx, _ ) -> idx > fromMaxIndex)
                |> List.map (\( _, a ) -> config.add a)

        folder : comparable -> ( Int, a ) -> List (Interpolator a) -> List (Interpolator a)
        folder id ( idx, a ) result =
            let
                add =
                    Dict.get idx additions
                        |> Maybe.map (\x -> config.add x)
                        |> Maybe.map List.singleton
                        |> Maybe.withDefault []

                interpolator =
                    if Dict.member id removals then
                        config.remove a

                    else
                        case Dict.get id toIds of
                            Just ( _, b ) ->
                                if b == a then
                                    always b

                                else
                                    config.change a b

                            Nothing ->
                                cantHappen ()
            in
            result ++ (interpolator :: add)

        cantHappen () =
            cantHappen ()

        resultingInterpolator =
            Dict.foldl folder [] fromIds
                ++ onTop
                |> inParallel
    in
    \t ->
        let
            result =
                resultingInterpolator t
        in
        if t >= 1 then
            List.filter (\item -> not (Dict.member (config.id item) removals)) result

        else
            result


{-| -}
type ListCombiner
    = CombineParallel


{-| Runs all the list interpolations in parallel.
-}
combineParallel : ListCombiner
combineParallel =
    CombineParallel


{-| A somewhat elaborate interpolator that gives points along an arbitrary path, where at `t=0` this will give the first point in the path,
and at `t=1` gives the last. If overshot, will give points along the tangent at these points.

Will give the origin point if the path is empty or invalid (generally you should check somewhere earlier if there is a possibility of that).

Performance note: this function does a fair amount of work when the interpolator is being constructed, so that later when it is called with
the `t` parameter it can be rather efficient. Therefore it is more efficient to partially apply this with a path in a static context or store
the partially applied function in a model.

-}
pointAlongPath : Path -> Interpolator ( Float, Float )
pointAlongPath path =
    let
        tolerance =
            List.foldr
                (\subPath coords ->
                    case SubPath.unwrap subPath of
                        Nothing ->
                            coords

                        Just { moveto, drawtos } ->
                            let
                                (LowLevel.Command.MoveTo coord) =
                                    moveto
                            in
                            coord
                                :: List.concatMap
                                    (\drawto ->
                                        case drawto of
                                            LowLevel.Command.LineTo lts ->
                                                lts

                                            LowLevel.Command.CurveTo cts ->
                                                List.concatMap (\( a, b, c ) -> [ a, b, c ]) cts

                                            LowLevel.Command.QuadraticBezierCurveTo qbcts ->
                                                List.concatMap (\( a, b ) -> [ a, b ]) qbcts

                                            LowLevel.Command.EllipticalArc eas ->
                                                List.concatMap (\arc -> [ arc.radii, arc.target ]) eas

                                            LowLevel.Command.ClosePath ->
                                                []
                                    )
                                    drawtos
                )
                []
                path
                |> Statistics.extent
                |> Maybe.map (\( ( x0, x1 ), ( y0, y1 ) ) -> max (abs (x0 - x1)) (abs (y0 - y1)) / 100)
                |> Maybe.withDefault 1

        alps =
            List.map
                (\subPath ->
                    let
                        alp =
                            SubPath.arcLengthParameterized tolerance subPath
                    in
                    ( SubPath.arcLength alp, alp )
                )
                path

        totalLength =
            List.map Tuple.first alps |> List.sum

        ( _, interp ) =
            List.foldl
                (\( len, alp ) ( soFar, fn ) ->
                    let
                        interpStart =
                            soFar / totalLength
                    in
                    ( soFar + len
                    , \t ->
                        if soFar == 0 || t >= interpStart then
                            SubPath.pointAlong alp (t * totalLength - soFar)
                                |> Maybe.withDefault ( 0, 0 )

                        else
                            fn t
                    )
                )
                ( 0, always ( 0, 0 ) )
                alps
    in
    interp
