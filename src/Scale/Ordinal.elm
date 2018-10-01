module Scale.Ordinal exposing (convert)

import Dict exposing (Dict)


convert : List a -> List b -> a -> Maybe b
convert domain range val =
    case range of
        [] ->
            Nothing

        otherwise ->
            convertHelp domain range [] val


convertHelp : List a -> List b -> List b -> a -> Maybe b
convertHelp d r used needle =
    case ( d, r ) of
        ( [], _ ) ->
            Nothing

        ( x :: xs, [] ) ->
            convertHelp d (List.reverse used) [] needle

        ( x :: xs, y :: ys ) ->
            if x == needle then
                Just y

            else
                convertHelp xs ys (y :: used) needle
