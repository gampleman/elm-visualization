module Visualization.Histogram exposing (HistogramGenerator, Threshold, Bin, binCount, sturges, float, generator, custom, compute)

import Array exposing (Array)
import Visualization.Array as Array
import Visualization.List as VList


type alias Bin a comparable =
    { x0 : comparable
    , x1 : comparable
    , values : List a
    , length : Int
    }


type HistogramGenerator a comparable
    = H { value : a -> comparable, threshold : Threshold a comparable, domain : List a -> Maybe ( a, a ) }


type Threshold a comparable
    = StepsThreshold (List a)
    | FnThreshold (List a -> ( a, a ) -> List comparable)


binCount : ( Float, Float ) -> Int -> List Float
binCount ( x0, x1 ) num =
    let
        tz =
            VList.tickStep x0 x1 num
    in
        VList.range (toFloat (ceiling (x0 / tz)) * tz) (toFloat (floor (x1 / tz)) * tz) tz


sturges : (a -> Float) -> Threshold a Float
sturges fn =
    FnThreshold (\list extent -> binCount (tupleMap fn extent) <| (+) 1 <| ceiling <| logBase 2 <| toFloat <| List.length list)


float : HistogramGenerator Float Float
float =
    generator identity


generator : (a -> Float) -> HistogramGenerator a Float
generator f =
    custom (sturges f) f


custom : Threshold a comparable -> (a -> comparable) -> HistogramGenerator a comparable
custom thresh function =
    H { value = function, threshold = thresh, domain = VList.extentWith function }


resolveThreshold : (a -> comparable) -> List a -> ( a, a ) -> Threshold a comparable -> List comparable
resolveThreshold value list domain threshold =
    case threshold of
        StepsThreshold thresh ->
            List.map value thresh

        FnThreshold fn ->
            fn list domain


tupleMap : (a -> b) -> ( a, a ) -> ( b, b )
tupleMap f ( a1, a2 ) =
    ( f a1, f a2 )


compute : List a -> HistogramGenerator a comparable -> List (Bin a comparable)
compute list (H { value, threshold, domain }) =
    case list of
        [] ->
            []

        x :: xs ->
            let
                defaultValue =
                    value x

                computedDomain =
                    domain list |> Maybe.withDefault ( x, x )

                ( x0, x1 ) =
                    computedDomain |> tupleMap value

                thresholds =
                    resolveThreshold value list computedDomain threshold
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
                bins =
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
                List.foldl binify bins list |> Array.toList
