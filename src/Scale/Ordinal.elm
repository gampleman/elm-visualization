module Scale.Ordinal exposing (convert)


convert : List a -> List b -> a -> Maybe b
convert domain range val =
    case range of
        [] ->
            Nothing

        _ ->
            convertHelp domain range [] val


convertHelp : List a -> List b -> List b -> a -> Maybe b
convertHelp d r used needle =
    case ( d, r ) of
        ( [], _ ) ->
            Nothing

        ( _ :: _, [] ) ->
            convertHelp d (List.reverse used) [] needle

        ( x :: xs, y :: ys ) ->
            if x == needle then
                Just y

            else
                convertHelp xs ys (y :: used) needle
