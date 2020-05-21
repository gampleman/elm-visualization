module Scale.Quantile exposing (scale)

import Array exposing (Array)
import Histogram.Array exposing (bisectRight)
import Statistics


scale ( r, range ) domain =
    let
        domain_ =
            List.sort domain

        range_ =
            Array.fromList (r :: range)

        n =
            max 0 (List.length range)

        quantiles =
            Array.initialize n (\i -> Statistics.quantile (toFloat (i + 1) / toFloat (n + 1)) domain_ |> Maybe.withDefault 0)
    in
    { convert = convert r quantiles
    , invertExtent = invertExtent quantiles
    , domain = domain_
    , range = range_
    , quantiles = quantiles |> Array.toList
    }


convert default thresholds domain range x =
    Array.get (bisectRight x thresholds Nothing) range |> Maybe.withDefault default


invertExtent : Array Float -> List Float -> Array a -> a -> Maybe ( Float, Float )
invertExtent thresholds domain range y =
    Maybe.andThen
        (\idx ->
            Maybe.map2 Tuple.pair
                (if idx <= 0 then
                    List.head domain

                 else
                    Array.get (idx - 1) thresholds
                )
                (if idx >= Array.length thresholds then
                    last domain

                 else
                    Array.get idx thresholds
                )
        )
        (indexOf y range)


last : List a -> Maybe a
last xs =
    case xs of
        [ x ] ->
            Just x

        [] ->
            Nothing

        _ :: ys ->
            last ys


indexOf : a -> Array a -> Maybe Int
indexOf a =
    Array.foldl
        (\item idxSoFar ->
            case ( item == a, idxSoFar ) of
                ( False, Err idx ) ->
                    Err (idx + 1)

                ( True, Err idx ) ->
                    Ok idx

                ( _, Ok idx ) ->
                    Ok idx
        )
        (Err 0)
        >> Result.toMaybe
