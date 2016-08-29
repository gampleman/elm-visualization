module Visualization.Scale.Internal exposing (bimap)


bimap :
    ( comparable, comparable )
    -> ( comparable', comparable' )
    -> (comparable -> comparable -> a -> b)
    -> (comparable' -> comparable' -> b -> c)
    -> (a -> c)
bimap ( d0, d1 ) ( r0, r1 ) deinterpolate reinterpolate =
    let
        ( de, re ) =
            if d1 < d0 then
                ( deinterpolate d1 d0, reinterpolate r1 r0 )
            else
                ( deinterpolate d0 d1, reinterpolate r0 r1 )
    in
        re << de
