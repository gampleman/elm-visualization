module Stack exposing (StackConfig, StackResult, create, repeatFirst, toArea, calculateExtremes)

import Visualization.Scale as Scale exposing (ContinuousScale, OrdinalScale, Scale)
import Path exposing (Path, moveTo, subpath, lineTo, closePath)
import List.Extra as List


type alias Offset =
    List (List ( Float, Float )) -> List (List ( Float, Float ))


type alias Order a =
    List ( a, List Float ) -> List ( a, List Float )


type alias StackConfig a =
    { data : List ( a, List Float )
    , offset : Offset
    , order : Order a
    }


type alias StackResult a =
    { values : List (List ( Float, Float ))
    , labels : List a
    }


{-|

    evenlySpaced 2 (0, 100) --> [ 0, 50, 100 ]

-}
evenlySpaced : Int -> ( Float, Float ) -> List Float
evenlySpaced n (( lower, upper ) as extent) =
    if n <= 1 then
        [ lower, upper ]
    else
        Scale.linear ( 1, toFloat n ) extent
            |> (\intermediateScale -> List.map (Scale.convert intermediateScale << toFloat) (List.range 1 n))


create : StackConfig a -> StackResult a
create config =
    let
        ( labels, stacked ) =
            stack config.offset config.order config.data
    in
        { values = stacked
        , labels = labels
        }


stack : Offset -> Order a -> List ( a, List Float ) -> ( List a, List (List ( Float, Float )) )
stack offset order items =
    let
        ( labels, values ) =
            items
                |> order
                |> List.unzip
    in
        values
            |> List.map (List.indexedMap (\i e -> ( toFloat i, e )))
            |> Debug.log "before offset"
            |> offset
            |> (,) labels


{-| Calculates the minimal and maximal y values, for correct scaling
-}
calculateExtremes : List (List ( Float, Float )) -> ( Float, Float )
calculateExtremes coords =
    let
        folder ( y1, y2 ) ( accmin, accmax ) =
            ( Basics.min y1 y2 |> Basics.min accmin, Basics.max y1 y2 |> Basics.max accmax )
    in
        List.map (List.foldl folder ( 0, 0 )) coords
            |> List.foldl (\( mi, ma ) ( accmin, accmax ) -> ( Basics.min mi accmin, Basics.max ma accmax )) ( 0, 0 )


repeatFirst : List a -> List a
repeatFirst items =
    case items of
        [] ->
            []

        x :: xs ->
            x :: x :: xs


{-| Draws the area between two functions

The third argument contains the lower and upper y values. The x value is linearly interpolated
based on the x range.


-}
toArea : (List ( Float, Float ) -> Path) -> ( Scale { a | range : ( Float, Float ) }, ContinuousScale ) -> List ( Float, Float ) -> Path
toArea curve ( scaleX, scaleY ) ys =
    let
        order ( y1, y2 ) =
            if y1 < y2 then
                ( y1, y2 )
            else
                ( y2, y1 )

        ( low_, high_ ) =
            List.map order ys
                |> List.map (\( l, h ) -> ( Scale.convert scaleY l, Scale.convert scaleY h ))
                |> List.unzip

        xCoordinates =
            evenlySpaced (List.length ys - 1) (Scale.range scaleX)

        ( low, high ) =
            ( List.map2 (,) xCoordinates low_
            , List.map2 (,) xCoordinates high_
            )
    in
        cycle curve low high


cycle : (List ( Float, Float ) -> Path) -> List ( Float, Float ) -> List ( Float, Float ) -> Path
cycle curve low high_ =
    let
        high =
            List.reverse high_
    in
        case ( low, high ) of
            ( l :: ls, h :: hs ) ->
                let
                    lastLow =
                        List.last ls |> Maybe.withDefault l

                    lastHigh =
                        List.head high_ |> Maybe.withDefault h

                    connectorRight =
                        [ subpath (moveTo lastLow) [ lineTo [ h ] ] ]

                    connectorLeft =
                        [ subpath (moveTo lastHigh) [ closePath ] ]
                in
                    [ curve low
                    , connectorRight
                    , curve high
                    , connectorLeft
                    ]
                        |> List.concat
                        |> List.concatMap .drawtos
                        |> subpath (moveTo l)
                        |> List.singleton

            _ ->
                []
