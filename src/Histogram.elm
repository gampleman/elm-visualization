module Histogram exposing
    ( HistogramGenerator, float, generator, custom, withDomain
    , Bin, compute
    , Threshold, sturges, steps, binCount
    )

{-| A histogram is an accurate graphical representation of the distribution of
numerical data. It is an estimate of the probability distribution of a continuous
variable (quantitative variable)

[![Histogram](https://code.gampleman.eu/elm-visualization/Histogram/preview.png)](https://code.gampleman.eu/elm-visualization/Histogram/)

To compute a histogram, one first configures a Histogram Generator and then uses
it to compute a histogram. Histograms can then be visualized in a variety of ways,
for example using Svg rects and linear scales.


### Configuring a Generator

@docs HistogramGenerator, float, generator, custom, withDomain


### Computing a Histogram

@docs Bin, compute


### Thresholds

@docs Threshold, sturges, steps, binCount

-}

import Array
import Histogram.Array as Array
import Statistics


{-| A bin holding data. All of the data falling into the bin is available in `values`. Each of the
`values` (when transformed to a comparable) falls between `x0` and `x1`. The number of elements in the
bin is available as `length`, which is equivalent to (but faster then) `List.length values`.
-}
type alias Bin a comparable =
    { x0 : comparable
    , x1 : comparable
    , values : List a
    , length : Int
    }


{-| Represents configuration to compute a histogram from a list of arbitrary data.

However, to compute a histogram, the data must be made comparable, this is typically done
through a conversion to a `Float`, however any `comparable` type will do.

-}
type HistogramGenerator a comparable
    = H { value : a -> comparable, threshold : Threshold a comparable, domain : List a -> Maybe ( a, a ) }


{-| A function that computes threshold values separating the individual bins. It is passed a function that
can convert values to comparables, the list of all valus and the extent (i.e. smallest and largest value).
Note that the smallest and largest value may be the same, however the list of all values is guaranteed not to
be empty.

It must return a list of boundary values that separate the bins. If you wish to have `n` bins, this should
return `n-1` thresholds.

-}
type alias Threshold a comparable =
    (a -> comparable) -> List a -> ( a, a ) -> List comparable


{-| Computes appropriate threshold values given an extent and the desired number of bins. Useful for implementing
your custom `Threshold` values when you have a way to compute the desired number of bins.
-}
binCount : ( Float, Float ) -> Int -> List Float
binCount ( x0, x1 ) num =
    let
        tz =
            Statistics.tickStep x0 x1 num
    in
    Statistics.range (toFloat (ceiling (x0 / tz)) * tz) (toFloat (floor (x1 / tz)) * tz) tz


{-| For creating an appropriate Threshold value if you already have appropriate
Threshold values (i.e. from `Scale.ticks`).
-}
steps : List a -> Threshold a comparable
steps xs =
    \fn _ _ -> List.map fn xs


{-| Returns the threshold values according to [Sturges’ formula](https://en.wikipedia.org/wiki/Histogram#Mathematical_definition).
This is a decent default value, however it implicitly assumes an approximately normal distribution and may perform poorly
if you have less than 30 data points.
-}
sturges : Threshold a Float
sturges fn list domain =
    List.length list
        |> toFloat
        |> logBase 2
        |> ceiling
        |> (+) 1
        |> binCount (tupleMap fn domain)


{-| Create a histogram generator that takes float data and uses Sturges' formula for thresholding.
-}
float : HistogramGenerator Float Float
float =
    generator identity


{-| Make histograms with arbitrary data passing in a function that converts the data to a Float.

This is pretty similar to using `Histogram.float` and `List.map`ing your data in advance, however
here you will have access to the original data in the bins if needed for further analysis.

-}
generator : (a -> Float) -> HistogramGenerator a Float
generator f =
    custom sturges f


{-| Create a custom generator by supplying your own threshold function and a mapping function.
-}
custom : Threshold a comparable -> (a -> comparable) -> HistogramGenerator a comparable
custom thresh function =
    H { value = function, threshold = thresh, domain = Statistics.extentBy function }


{-| Set the domain for the HistogramGenerator. All values falling outside the domain will be ignored.
-}
withDomain : ( a, a ) -> HistogramGenerator a comparable -> HistogramGenerator a comparable
withDomain =
    Just >> always >> limitedBy


{-| Let's not expose this for now. Not entirely sure what the use case would be.
-}
limitedBy : (List a -> Maybe ( a, a )) -> HistogramGenerator a comparable -> HistogramGenerator a comparable
limitedBy domain (H cfg) =
    H { cfg | domain = domain }


tupleMap : (a -> b) -> ( a, a ) -> ( b, b )
tupleMap f ( a1, a2 ) =
    ( f a1, f a2 )


{-| Given some data and a configured HistogramGenerator, computes the binning of the data.

If the data is empty, returns an empty list.

-}
compute : List a -> HistogramGenerator a comparable -> List (Bin a comparable)
compute list (H { value, threshold, domain }) =
    case list of
        [] ->
            []

        x :: _ ->
            let
                defaultValue =
                    value x

                computedDomain =
                    domain list |> Maybe.withDefault ( x, x )

                ( x0, x1 ) =
                    computedDomain |> tupleMap value

                thresholds =
                    threshold value list computedDomain
                        |> List.filter (\t -> t >= x0 && t < x1)
                        |> Array.fromList

                thresholdsCount =
                    Array.length thresholds

                initBin i thresh =
                    { x0 =
                        if i > 0 then
                            Array.get (i - 1) thresholds |> Maybe.withDefault defaultValue

                        else
                            x0
                    , x1 =
                        if i < thresholdsCount then
                            thresh

                        else
                            x1
                    , values = []
                    , length = 0
                    }

                fromMaybe =
                    Maybe.withDefault (initBin 0 defaultValue)

                -- this is for escaping maybes
                defaultBins =
                    Array.indexedMap initBin thresholds
                        |> Array.push (initBin thresholdsCount <| Maybe.withDefault defaultValue <| Array.get (thresholdsCount - 1) thresholds)

                binify item bins =
                    let
                        threshIndex =
                            Array.bisectRight (value item) thresholds <| Just ( 0, thresholdsCount )

                        oldBin =
                            fromMaybe <| Array.get threshIndex bins

                        newBin =
                            { oldBin | values = item :: oldBin.values, length = oldBin.length + 1 }
                    in
                    Array.set threshIndex newBin bins
            in
            List.foldl binify defaultBins list |> Array.toList
