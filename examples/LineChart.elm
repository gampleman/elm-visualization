module LineChart exposing (..)

import Visualization.Scale as Scale
import Visualization.Axis as Axis
import Visualization.List as List
import Date
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Date exposing (Date)
import String


w =
    600


h =
    350


padding =
    30


xScale data =
    let
        ( mi, ma ) =
            (Maybe.withDefault ( 1420070400000, 1420070430000 ) <| List.extent <| List.map (fst >> Date.toTime) data)
    in
        Scale.time ( Date.fromTime mi, Date.fromTime ma ) ( 0, w - 2 * padding )


yScale data =
    Scale.linear ( 0, 5 ) ( h - 2 * padding, 0 )


makePoints data =
    let
        yScaler =
            Scale.convert <| yScale data

        xScaler =
            Scale.convert <| xScale data

        buildPair ( date, count ) =
            toString (xScaler date) ++ "," ++ toString (yScaler (toFloat count))
    in
        String.join " " <| List.map buildPair data


view model =
    let
        opts =
            Axis.defaultOptions

        xAxis =
            Axis.axis { opts | orientation = Axis.Bottom, tickCount = List.length model } (xScale model)

        yAxis =
            Axis.axis { opts | orientation = Axis.Left, tickCount = 5 } (yScale model)
    in
        svg [ width (toString w ++ "px"), height (toString h ++ "px") ]
            [ g [ transform ("translate(" ++ toString padding ++ ", " ++ toString (h - padding) ++ ")") ]
                [ xAxis ]
            , g [ transform ("translate(" ++ toString padding ++ ", " ++ toString padding ++ ")") ]
                [ yAxis ]
            , g [ transform ("translate(" ++ toString padding ++ ", " ++ toString padding ++ ")"), class "series" ]
                [ polyline [ points (makePoints model), stroke "red", strokeWidth "3px", fill "none" ] [] ]
            ]



-- From here onwards this is simply example boilerplate.
-- In a real app you would load the data from a server and parse it, perhaps in
-- a separate module.


main =
    view model



-- Here we simply define the data inline. The examples don't include logic for fetching and parsing this data.


model =
    [ ( Date.fromTime 1448928000000, 2 )
    , ( Date.fromTime 1451606400000, 2 )
    , ( Date.fromTime 1454284800000, 1 )
    , ( Date.fromTime 1456790400000, 1 )
    ]
