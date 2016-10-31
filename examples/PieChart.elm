module PieChart exposing (main)

import Visualization.Shape as Shape
import Array exposing (Array)
import Svg exposing (svg, g, path, text', text)
import Svg.Attributes exposing (transform, d, style, dy, width, height)


model : List ( String, Float )
model =
    [ ( "<5", 2704659 )
    , ( "5-13", 4499890 )
    , ( "14-17", 2159981 )
    , ( "18-24", 3853788 )
    , ( "25-44", 14106543 )
    , ( "45-64", 8819342 )
    , ( "≥65", 612463 )
    ]


screenWidth =
    990


screenHeight =
    504


colors : Array String
colors =
    Array.fromList [ "#98abc5", "#8a89a6", "#7b6888", "#6b486b", "#a05d56", "#d0743c", "#ff8c00" ]


radius =
    min screenWidth screenHeight / 2


view model =
    let
        pieData =
            model |> List.map snd |> Shape.pie (radius - 60) radius 0.01 |> Debug.log "pie"

        makeSlice index datum =
            path [ d (Shape.arc { datum | padRadius = 0, cornerRadius = 10 }), style ("fill:" ++ (Maybe.withDefault "#000" <| Array.get index colors)) ] []

        makeLabel slice ( label, value ) =
            text' [ transform ("translate(" ++ toString (Shape.centroid { slice | innerRadius = radius - 40, outerRadius = radius - 40 }) ++ ")"), dy ".35em" ]
                [ text label ]
    in
        svg [ width (toString screenWidth ++ "px"), height (toString screenHeight ++ "px") ]
            [ g [ transform ("translate(" ++ toString (screenWidth / 2) ++ "," ++ toString (screenHeight / 2) ++ ")") ]
                [ g [] <| List.indexedMap makeSlice pieData
                  --, g [] <| List.map2 makeLabel pieData model
                ]
            ]


main =
    view model
