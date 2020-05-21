module Scale.Threshold exposing (scale)

import Array exposing (Array)
import Histogram.Array exposing (bisectRight)
import Statistics


deinterleave domrange domain range =
    case domrange of
        ( d, r ) :: xs ->
            deinterleave xs (d :: domain) (r :: range)

        [] ->
            ( List.reverse domain |> Array.fromList, List.reverse range |> Array.fromList )


scale ( r0, domrange ) =
    let
        ( domain_, range_ ) =
            deinterleave domrange [] [ r0 ]
    in
    { convert = convert r0
    , domain = domain_
    , range = range_
    }


convert default thresholds range x =
    Array.get (bisectRight x thresholds Nothing) range |> Maybe.withDefault default
