module PieChart exposing (main)

{-| An example showing how to render a basic pie chart.
-}

import Array exposing (Array)
import Path
import Svg.Attributes exposing (fill)
import TypedSvg exposing (g, svg, text_)
import TypedSvg.Attributes exposing (dy, stroke, textAnchor, transform)
import TypedSvg.Attributes.InPx exposing (height, width)
import TypedSvg.Color exposing (white)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Transform(..), em)
import Visualization.Shape as Shape exposing (defaultPieConfig)


w : Float
w =
    990


h : Float
h =
    504


colors : Array String
colors =
    Array.fromList [ "#98abc5", "#8a89a6", "#7b6888", "#6b486b", "#a05d56", "#d0743c", "#ff8c00" ]


radius : Float
radius =
    min w h / 2


view : List ( String, Float ) -> Svg msg
view model =
    let
        pieData =
            model |> List.map Tuple.second |> Shape.pie { defaultPieConfig | outerRadius = radius }

        makeSlice index datum =
            Path.element (Shape.arc datum) [ fill (Maybe.withDefault "#000" <| Array.get index colors), stroke white ]

        makeLabel slice ( label, value ) =
            let
                ( x, y ) =
                    Shape.centroid { slice | innerRadius = radius - 40, outerRadius = radius - 40 }
            in
            text_
                [ transform [ Translate x y ]
                , dy (em 0.35)
                , textAnchor AnchorMiddle
                ]
                [ text label ]
    in
    svg [ width w, height h ]
        [ g [ transform [ Translate (w / 2) (h / 2) ] ]
            [ g [] <| List.indexedMap makeSlice pieData
            , g [] <| List.map2 makeLabel pieData model
            ]
        ]


data : List ( String, Float )
data =
    [ ( "/notifications", 2704659 )
    , ( "/about", 4499890 )
    , ( "/product", 2159981 )
    , ( "/blog", 3853788 )
    , ( "/shop", 14106543 )
    , ( "/profile", 8819342 )
    , ( "/", 612463 )
    ]


main =
    view data
