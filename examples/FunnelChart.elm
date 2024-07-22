module FunnelChart exposing (main)

import Color exposing (Color)
import Float.Extra
import List.Extra
import Scale exposing (BandScale, ContinuousScale, SequentialScale, defaultBandConfig)
import Scale.Color
import TypedSvg exposing (g, polygon, rect, svg, text_)
import TypedSvg.Attributes exposing (fill, fillOpacity, fontFamily, points, stroke, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (fontSize, height, strokeWidth, width, x, y)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Opacity(..), Paint(..), Transform(..))


w : Float
w =
    900


h : Float
h =
    450


titleWidth : Float
titleWidth =
    100


padding : Float
padding =
    30


maxValue : Float
maxValue =
    data
        |> List.map .value
        |> List.maximum
        |> Maybe.withDefault 0


xScale : ContinuousScale Float
xScale =
    Scale.linear ( 0, w - 2 * padding - 2 * titleWidth ) ( 0, maxValue )


yScale : BandScale String
yScale =
    data
        |> List.map .title
        |> Scale.band { defaultBandConfig | paddingInner = 0.2 } ( padding, h - padding )


colorScale : SequentialScale Color
colorScale =
    Scale.sequential Scale.Color.bluesInterpolator ( -1, toFloat (List.length data - 1) )


view : Svg msg
view =
    List.map
        (\shape ->
            case shape of
                Bar index datum ->
                    let
                        color =
                            Scale.convert colorScale (toFloat index)
                    in
                    g []
                        [ rect
                            [ x (-(Scale.convert xScale datum.value) / 2)
                            , y (Scale.convert yScale datum.title)
                            , width (Scale.convert xScale datum.value)
                            , height (Scale.bandwidth yScale)
                            , fill (Paint color)
                            ]
                            []
                        , text_
                            [ x (-w / 2 + padding)
                            , y (Scale.convert yScale datum.title + Scale.bandwidth yScale / 2)
                            , transform [ Translate 0 5 ]
                            ]
                            [ text datum.title ]
                        , text_
                            [ x (w / 2 - padding)
                            , y (Scale.convert yScale datum.title + Scale.bandwidth yScale / 2)
                            , transform [ Translate 0 5 ]
                            , textAnchor AnchorEnd
                            ]
                            [ text (Float.Extra.toFixedDecimalPlaces 1 (100 * datum.value / maxValue) ++ "%") ]
                        , text_
                            [ x 0
                            , y (Scale.convert yScale datum.title + Scale.bandwidth yScale / 2)
                            , transform [ Translate 0 5 ]
                            , textAnchor AnchorMiddle
                            , fill
                                (Paint
                                    (if (Color.toHsla color).lightness > 0.5 then
                                        Color.black

                                     else
                                        Color.white
                                    )
                                )
                            , stroke (Paint color)
                            , strokeWidth 3
                            , TypedSvg.Core.attribute "paint-order" "stroke"
                            , fillOpacity (Opacity 0.9)
                            ]
                            [ text (String.fromFloat datum.value) ]
                        ]

                Connector top bottom ->
                    g []
                        [ polygon
                            [ points
                                [ ( -(Scale.convert xScale top.value) / 2, Scale.convert yScale top.title + Scale.bandwidth yScale )
                                , ( Scale.convert xScale top.value / 2, Scale.convert yScale top.title + Scale.bandwidth yScale )
                                , ( Scale.convert xScale bottom.value / 2, Scale.convert yScale bottom.title )
                                , ( -(Scale.convert xScale bottom.value) / 2, Scale.convert yScale bottom.title )
                                ]
                            , fill (Paint (Scale.convert colorScale -1))
                            ]
                            []
                        , text_
                            [ x 0
                            , y (Scale.convert yScale top.title + Scale.bandwidth yScale)
                            , transform [ Translate 0 14 ]
                            , textAnchor AnchorMiddle
                            , fontSize 12
                            , fillOpacity (Opacity 0.7)
                            ]
                            [ text (Float.Extra.toFixedDecimalPlaces 1 (100 * bottom.value / top.value) ++ "%") ]
                        ]
        )
        preprocessed
        |> svg [ viewBox (-w / 2) 0 w h, fontFamily [ "sans-serif" ] ]


type Shape
    = Bar Int Datum
    | Connector Datum Datum


type alias Datum =
    { title : String
    , value : Float
    }


preprocessed : List Shape
preprocessed =
    List.map2 Connector data (List.drop 1 data)
        |> List.Extra.interweave (List.indexedMap Bar data)


data : List Datum
data =
    [ { title = "Total"
      , value = 54809
      }
    , { title = "Helpful"
      , value = 29434
      }
    , { title = "Qualified"
      , value = 10345
      }
    , { title = "Successful"
      , value = 3432
      }
    ]


main =
    view
