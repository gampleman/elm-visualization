module Slopechart exposing (main)

{-| A polar plot of `sin(2x)cos(2x)`.

@category Advanced

-}

import Color
import Force
import Html
import Html.Attributes exposing (style)
import List.Extra
import Scale exposing (ContinuousScale)
import Statistics
import TypedSvg exposing (g, svg)
import TypedSvg.Attributes exposing (dy, fill, stroke, strokeLinejoin, textAnchor, textDecoration, viewBox)
import TypedSvg.Attributes.InPx exposing (fontSize, height, strokeWidth, width, x, y)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Paint(..), StrokeLinejoin(..), em)


w : Float
w =
    500


h : Float
h =
    804


xPadding : Float
xPadding =
    150


yPadding : Float
yPadding =
    16


xScale : ContinuousScale Float
xScale =
    data
        |> List.concatMap (Tuple.second >> List.map (Tuple.first >> toFloat))
        |> Statistics.extent
        |> Maybe.withDefault ( 0, 0 )
        |> Scale.linear ( xPadding, w - xPadding )


yScale : ContinuousScale Float
yScale =
    Scale.linear ( h - 5, yPadding ) ( 0, 100 )


years =
    [ 5, 10, 15, 20 ]


main : Svg msg
main =
    Html.div [ style "display" "flex" ]
        [ Html.div [ style "height" "504px", style "overflow-y" "auto" ]
            [ svg
                [ viewBox 0 0 w h, width w, height h ]
                [ data
                    |> List.map
                        (\( _, values ) ->
                            TypedSvg.polyline
                                [ fill PaintNone
                                , stroke (Paint Color.gray)
                                , List.map
                                    (\( year, survival ) ->
                                        ( Scale.convert xScale (toFloat year), Scale.convert yScale survival )
                                    )
                                    values
                                    |> TypedSvg.Attributes.points
                                ]
                                []
                        )
                    |> g []
                , years
                    |> List.map
                        (\year ->
                            labelsAtYear year
                                |> yOcclusion
                                |> List.map
                                    (\( label, xPos, yPos ) ->
                                        TypedSvg.text_
                                            [ x xPos
                                            , y yPos
                                            , fill (Paint Color.black)
                                            , stroke (Paint Color.white)
                                            , strokeWidth 5
                                            , strokeLinejoin StrokeLinejoinRound
                                            , TypedSvg.Core.attribute "paint-order" "stroke"
                                            , dy (em 0.32)
                                            , fontSize 10
                                            , textAnchor
                                                (if year == 5 then
                                                    AnchorEnd

                                                 else if year == 20 then
                                                    AnchorStart

                                                 else
                                                    AnchorMiddle
                                                )
                                            ]
                                            [ text label ]
                                    )
                                >> g []
                        )
                    |> g []
                , years
                    |> List.map
                        (\year ->
                            TypedSvg.text_
                                [ x (Scale.convert xScale (toFloat year))
                                , y 5
                                , fill (Paint Color.gray)
                                , dy (em 0.32)
                                , fontSize 10
                                , textAnchor AnchorMiddle
                                , textDecoration "underline"
                                ]
                                [ TypedSvg.tspan [ fill (Paint Color.black) ] [ text (String.fromInt year ++ " year") ] ]
                        )
                    |> g []
                ]
            ]
        , Html.div []
            [ Html.h1 [ style "font-size" "24px", style "margin" "0" ] [ text "Estimates of relative survival rates" ]
            , Html.h2 [ style "font-size" "24px", style "margin-top" "0" ] [ text "by cancer site" ]
            , Html.p [] [ Html.text "Based on ", Html.a [ Html.Attributes.href "https://www.edwardtufte.com/bboard/q-and-a-fetch-msg?msg_id=0003nk" ] [ Html.text "Tufte" ] ]
            , Html.p [ style "position" "absolute", style "bottom" "10px" ] [ text "↓ Scroll to see more" ]
            ]
        ]


yOcclusion : List ( String, Float, Float ) -> List ( String, Float, Float )
yOcclusion labels =
    let
        initialState =
            Force.simulation
                [ Force.towardsY (List.map (\( l, _, y ) -> { node = l, target = y, strength = 0.1 }) labels)
                , Force.collision 6.5 (List.map (\( l, _, _ ) -> l) labels)
                ]

        helper i state entities =
            if i <= 0 then
                List.map (\e -> ( e.id, e.fx, e.y )) entities

            else
                let
                    ( newState, newEntities ) =
                        Force.tick state entities
                in
                helper (i - 1) newState (List.map (\e -> { e | x = e.fx }) newEntities)
    in
    helper 20 initialState (List.map (\( l, x, y ) -> { x = x, y = y, fx = x, vx = 0, vy = 0, id = l }) labels)


labelsAtYear : Int -> List ( String, Float, Float )
labelsAtYear y =
    data
        |> List.filterMap
            (\( name, values ) ->
                List.Extra.find (\( year, _ ) -> year == y) values
                    |> Maybe.map
                        (\( year, survival ) ->
                            ( if year == 5 then
                                name ++ " " ++ String.fromFloat survival

                              else if year == 20 then
                                String.fromFloat survival ++ " " ++ name

                              else
                                String.fromFloat survival
                            , Scale.convert xScale (toFloat year)
                            , Scale.convert yScale survival
                            )
                        )
            )
        |> List.Extra.uniqueBy (\( n, _, _ ) -> n)


type alias Data =
    List ( String, List ( Int, Float ) )


data : Data
data =
    [ ( "Prostate", [ ( 5, 99 ), ( 10, 95 ), ( 15, 87 ), ( 20, 81 ) ] )
    , ( "Thyroid", [ ( 5, 96 ), ( 10, 96 ), ( 15, 94 ), ( 20, 95 ) ] )
    , ( "Testis", [ ( 5, 95 ), ( 10, 94 ), ( 15, 91 ), ( 20, 88 ) ] )
    , ( "Melanomas", [ ( 5, 89 ), ( 10, 87 ), ( 15, 84 ), ( 20, 83 ) ] )
    , ( "Breast", [ ( 5, 86 ), ( 10, 78 ), ( 15, 71 ), ( 20, 65 ) ] )
    , ( "Hodgkin’s disease", [ ( 5, 85 ), ( 10, 80 ), ( 15, 74 ), ( 20, 67 ) ] )
    , ( "Corpus uteri,uterus", [ ( 5, 84 ), ( 10, 83 ), ( 15, 81 ), ( 20, 79 ) ] )
    , ( "Urinary, bladder", [ ( 5, 82 ), ( 10, 76 ), ( 15, 70 ), ( 20, 68 ) ] )
    , ( "Cervix, uteri", [ ( 5, 71 ), ( 10, 64 ), ( 15, 63 ), ( 20, 60 ) ] )
    , ( "Larynx", [ ( 5, 69 ), ( 10, 57 ), ( 15, 46 ), ( 20, 38 ) ] )
    , ( "Rectum", [ ( 5, 63 ), ( 10, 55 ), ( 15, 52 ), ( 20, 49 ) ] )
    , ( "Kidney, renal pelvis", [ ( 5, 62 ), ( 10, 54 ), ( 15, 50 ), ( 20, 47 ) ] )
    , ( "Colon", [ ( 5, 62 ), ( 10, 55 ), ( 15, 54 ), ( 20, 52 ) ] )
    , ( "Non-Hodgkin’s", [ ( 5, 58 ), ( 10, 46 ), ( 15, 38 ), ( 20, 34 ) ] )
    , ( "Oral cavity, pharynx", [ ( 5, 57 ), ( 10, 44 ), ( 15, 38 ), ( 20, 33 ) ] )
    , ( "Ovary", [ ( 5, 55 ), ( 10, 49 ), ( 15, 50 ), ( 20, 50 ) ] )
    , ( "Leukemia", [ ( 5, 43 ), ( 10, 32 ), ( 15, 30 ), ( 20, 26 ) ] )
    , ( "Brain, nervous system", [ ( 5, 32 ), ( 10, 29 ), ( 15, 28 ), ( 20, 26 ) ] )
    , ( "Multiple myeloma", [ ( 5, 30 ), ( 10, 13 ), ( 15, 7 ), ( 20, 5 ) ] )
    , ( "Stomach", [ ( 5, 24 ), ( 10, 19 ), ( 15, 19 ), ( 20, 15 ) ] )
    , ( "Lung and bronchus", [ ( 5, 15 ), ( 10, 11 ), ( 15, 8 ), ( 20, 6 ) ] )
    , ( "Esophagus", [ ( 5, 14 ), ( 10, 8 ), ( 15, 8 ), ( 20, 5 ) ] )
    , ( "Liver, bile duct", [ ( 5, 8 ), ( 10, 6 ), ( 15, 6 ), ( 20, 8 ) ] )
    , ( "Pancreas", [ ( 5, 4 ), ( 10, 3 ), ( 15, 3 ), ( 20, 3 ) ] )
    ]
