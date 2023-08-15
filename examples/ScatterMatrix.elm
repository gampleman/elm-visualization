module ScatterMatrix exposing (main)

{-| This example demonstrates building a scatterplot matrix. Normally you might want to arrange the scatterplots in a square configuration, however since we have limited space in this example, we have shrunk these a little.

Also try dragging on the scatterplots - this shows using brushing to highlight instances across the scatterplots.

@category Advanced

-}

import Axis
import Browser
import Brush exposing (Brush, OnBrush, TwoDimensional)
import Color exposing (Color)
import Dict exposing (Dict)
import Random
import Scale exposing (ContinuousScale, OrdinalScale)
import Scale.Color
import Set exposing (Set)
import Statistics
import TypedSvg exposing (circle, g, svg, text_)
import TypedSvg.Attributes exposing (fill, fontFamily, stroke, strokeOpacity, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, fontSize, height, r, strokeWidth, width, x, y)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Opacity(..), Paint(..), Transform(..))


w : Float
w =
    900


h : Float
h =
    450


padding : Float
padding =
    40


type alias Car =
    { id : Int
    , horsepower : Float
    , brand : Brand
    , maxSpeed : Float
    , mpg : Float
    }


type Brand
    = FW
    | Nolvo
    | Coyota


type alias Model =
    { brush : Dict ( String, String ) (Brush TwoDimensional)
    , data : List Car
    }


type Msg
    = BrushMsg ( String, String ) OnBrush


init : () -> ( Model, Cmd Msg )
init () =
    ( { brush =
            charts
                |> List.map
                    (\key ->
                        ( key
                        , Brush.initXY
                            { top = padding
                            , bottom = h - padding
                            , left = Scale.convert chartPositionScale key
                            , right = Scale.convert chartPositionScale key + Scale.bandwidth chartPositionScale
                            }
                        )
                    )
                |> Dict.fromList
      , data = Random.step dataPoints (Random.initialSeed 3353434) |> Tuple.first
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    model.brush
        |> Dict.toList
        |> List.map (\( key, brush ) -> Brush.subscriptions brush (BrushMsg key))
        |> Sub.batch


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BrushMsg key brushMsg ->
            ( { model
                | brush =
                    Dict.map
                        (\k v ->
                            if k == key then
                                Brush.update brushMsg v

                            else
                                Brush.clearSelection v
                        )
                        model.brush
              }
            , Cmd.none
            )



-- Scales


charts : List ( String, String )
charts =
    [ ( "Horsepower [hp]", "Max speed [km/h]" )
    , ( "Fuel economy [mpg]", "Max speed [km/h]" )
    , ( "Horsepower [hp]", "Fuel economy [mpg]" )
    ]


chartPositionScale : Scale.BandScale ( String, String )
chartPositionScale =
    Scale.band { paddingInner = 0.2, paddingOuter = 0.12, align = 0.8 } ( 0, w ) charts


chartToAccessor : String -> Car -> Float
chartToAccessor chart =
    case chart of
        "Horsepower [hp]" ->
            .horsepower

        "Max speed [km/h]" ->
            .maxSpeed

        _ ->
            .mpg


chartScales : List Car -> List ( ContinuousScale Float, ContinuousScale Float )
chartScales data =
    charts
        |> List.map
            (\chartPair ->
                ( data
                    |> List.map (chartToAccessor (Tuple.first chartPair))
                    |> Statistics.extent
                    |> Maybe.withDefault ( 0, 0 )
                    |> Scale.linear ( Scale.convert chartPositionScale chartPair, Scale.convert chartPositionScale chartPair + Scale.bandwidth chartPositionScale )
                    |> Scale.nice 5
                , data
                    |> List.map (chartToAccessor (Tuple.second chartPair))
                    |> Statistics.extent
                    |> Maybe.withDefault ( 0, 0 )
                    |> Scale.linear ( h - padding, padding )
                    |> Scale.nice 8
                )
            )


computeSelectedCars : Model -> List ( ContinuousScale Float, ContinuousScale Float ) -> Set Int
computeSelectedCars model scales =
    if List.all (\brush -> Brush.selection2d brush == Nothing) (Dict.values model.brush) then
        Set.fromList (List.map .id model.data)

    else
        List.map2
            (\chartKey ( xScale, yScale ) ->
                case Maybe.andThen Brush.selection2d (Dict.get chartKey model.brush) of
                    Just sel ->
                        let
                            xAccessor =
                                chartToAccessor (Tuple.first chartKey)

                            yAccessor =
                                chartToAccessor (Tuple.second chartKey)

                            minX =
                                Scale.invert xScale sel.left

                            maxX =
                                Scale.invert xScale sel.right

                            maxY =
                                Scale.invert yScale sel.top

                            minY =
                                Scale.invert yScale sel.bottom
                        in
                        model.data
                            |> List.filter
                                (\datum ->
                                    xAccessor datum >= minX && xAccessor datum <= maxX && yAccessor datum >= minY && yAccessor datum <= maxY
                                )
                            |> List.map .id

                    Nothing ->
                        []
            )
            charts
            scales
            |> List.concat
            |> Set.fromList


view : Model -> Svg Msg
view model =
    let
        scales =
            chartScales model.data
    in
    svg [ viewBox 0 0 w h, width w, height h ]
        [ List.map2 (scatterChart (computeSelectedCars model scales) model) charts scales
            |> g []
        , colorLegend
        ]


colorLegend : Svg msg
colorLegend =
    g [ fontFamily [ "sans-serif" ], fontSize 12 ]
        [ text_ [ x 50, y 25 ] [ text "Brand:" ]
        , g [ transform [ Translate 100 20 ] ] <| colorLegendSwatch FW "FW"
        , g [ transform [ Translate 150 20 ] ] <| colorLegendSwatch Nolvo "Nolvo"
        , g [ transform [ Translate 205 20 ] ] <| colorLegendSwatch Coyota "Coyota"
        ]


colorLegendSwatch : Brand -> String -> List (Svg msg)
colorLegendSwatch brand label =
    [ circle
        [ stroke <| Paint (Scale.convert colorScale brand |> Maybe.withDefault Color.black)
        , fill PaintNone
        , strokeWidth 2
        , r 3
        , cy 1
        ]
        []
    , text_ [ x 10, y 5 ] [ text label ]
    ]


colorScale : OrdinalScale Brand Color
colorScale =
    Scale.ordinal Scale.Color.category10 [ FW, Nolvo, Coyota ]


scatterChart : Set Int -> Model -> ( String, String ) -> ( ContinuousScale Float, ContinuousScale Float ) -> Svg Msg
scatterChart selected model ( xLabel, yLabel ) ( xScale, yScale ) =
    let
        xAccessor =
            chartToAccessor xLabel

        yAccessor =
            chartToAccessor yLabel
    in
    g []
        [ model.data
            |> List.map
                (\datum ->
                    circle
                        [ stroke
                            (if Set.member datum.id selected then
                                Paint (Scale.convert colorScale datum.brand |> Maybe.withDefault Color.black)

                             else
                                Paint Color.black
                            )
                        , fill PaintNone
                        , strokeWidth 2
                        , r 3
                        , strokeOpacity
                            (if Set.member datum.id selected then
                                Opacity 1

                             else
                                Opacity 0.2
                            )
                        , cx (Scale.convert xScale (xAccessor datum))
                        , cy (Scale.convert yScale (yAccessor datum))
                        ]
                        []
                )
            |> g []
        , g [ transform [ Translate (Tuple.first (Scale.range xScale)) 0 ] ] [ Axis.left [ Axis.tickCount 8 ] yScale ]
        , g [ transform [ Translate 0 (Tuple.first (Scale.range yScale)) ] ] [ Axis.bottom [ Axis.tickCount 5 ] xScale ]
        , text_
            [ y (Tuple.first (Scale.range yScale) + padding * 0.75)
            , x (Tuple.first (Scale.range xScale) + (Tuple.second (Scale.range xScale) - Tuple.first (Scale.range xScale)) / 2)
            , textAnchor AnchorMiddle
            , fontFamily [ "sans-serif" ]
            , fontSize 12
            ]
            [ text xLabel ]
        , text_
            [ x (Tuple.first (Scale.range xScale) + padding)
            , y (Tuple.first (Scale.range yScale) + (Tuple.second (Scale.range yScale) - Tuple.first (Scale.range yScale)) / 2)
            , textAnchor AnchorMiddle
            , transform
                [ Rotate 270 (Tuple.first (Scale.range xScale) + padding / 4) (Tuple.first (Scale.range yScale) + (Tuple.second (Scale.range yScale) - Tuple.first (Scale.range yScale)) / 2)
                , Translate 0 -padding
                ]
            , fontFamily [ "sans-serif" ]
            , fontSize 12
            ]
            [ text yLabel ]
        , Maybe.map (Brush.view [] (BrushMsg ( xLabel, yLabel ))) (Dict.get ( xLabel, yLabel ) model.brush) |> Maybe.withDefault (text "")
        ]



-- Random Generators
{- This example uses random data to save space, however we make sure to introduce some relationships between the variables. -}


randomBetween : Float -> Float -> Random.Generator Float
randomBetween min max =
    Random.map2 (\a b -> (a + b) / 2) (Random.float min max) (Random.float min max)


dataPoints : Random.Generator (List Car)
dataPoints =
    Random.list 100
        (Random.map4
            (\horsepower mpg maxSpeed brand ->
                let
                    ( hpMult, mpgMult, maxSpeedMult ) =
                        case brand of
                            FW ->
                                ( 0.9, 1.1, 1 )

                            Nolvo ->
                                ( 1.1, 1, 1.2 )

                            Coyota ->
                                ( 0.8, 0.9, 0.96 )
                in
                { id = 0
                , horsepower = horsepower * hpMult
                , brand = brand
                , mpg = mpg * mpgMult / (horsepower * hpMult / 260)
                , maxSpeed = maxSpeed * maxSpeedMult
                }
            )
            (randomBetween 120 400)
            (randomBetween 15 45)
            (randomBetween 165 265)
            (Random.weighted ( 0.3, Coyota ) [ ( 0.5, FW ), ( 0.2, Nolvo ) ])
        )
        |> Random.map (List.indexedMap (\i rec -> { rec | id = i }))


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
