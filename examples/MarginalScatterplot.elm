module MarginalScatterplot exposing (main)

{-| This module shows a scatterplot with marginal distributions.

It demonstrates the following techniques:

  - Using Svg.Lazy for increased rendering performance
  - Using a Voronoi overlay for larger mouse targets to make selecting points easier
  - Using the axis compontent to build interactive crosshairs
  - Building custom axes using marginal histograms

@category Advanced
@minify false

-}

import Array
import Axis
import BoundingBox2d
import Browser
import Color
import Histogram
import Html
import Html.Attributes
import List
import Point2d
import Polygon2d exposing (Polygon2d)
import Random
import Scale exposing (ContinuousScale)
import Svg.Lazy
import TypedSvg exposing (circle, g, rect, style, svg)
import TypedSvg.Attributes exposing (class, fill, opacity, stroke, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, strokeWidth, width, x, y)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Events
import TypedSvg.Types exposing (Opacity(..), Paint(..), Transform(..))
import Units.Pixels exposing (Pixels)
import VoronoiDiagram2d


w : Float
w =
    900


h : Float
h =
    450


padding : Float
padding =
    30


xScale : ContinuousScale Float
xScale =
    Scale.linear ( padding, w - padding ) ( 10, 50 )


yScale : ContinuousScale Float
yScale =
    Scale.linear ( h - padding, padding ) ( 0, 800 )


circles : List ( Float, Float ) -> Svg msg
circles model =
    List.map pointCircle model
        |> g []


pointCircle : ( Float, Float ) -> Svg msg
pointCircle ( dataX, dataY ) =
    circle
        [ cx (Scale.convert xScale dataX)
        , cy (Scale.convert yScale dataY)
        , r 1.5
        , strokeWidth 0
        , stroke <| PaintNone
        , opacity <| Opacity 0.85
        ]
        []


histogram : List Float -> List (Histogram.Bin Float Float)
histogram model =
    Histogram.float
        |> Histogram.compute model


pointToStr : { x : Float, y : Float } -> String
pointToStr { x, y } =
    String.fromFloat x ++ " " ++ String.fromFloat y


polygonToStr : Polygon2d Pixels coordinates -> String
polygonToStr polygon =
    case Polygon2d.outerLoop polygon of
        [] ->
            ""

        fst :: rest ->
            "M" ++ pointToStr (Point2d.toPixels fst) ++ "L" ++ (List.map (Point2d.toPixels >> pointToStr) rest |> String.join ",")


voronoiOverlay : List ( Float, Float ) -> Svg Msg
voronoiOverlay data =
    VoronoiDiagram2d.fromVerticesBy (\( x, y ) -> Point2d.pixels (Scale.convert xScale x) (Scale.convert yScale y)) (Array.fromList data)
        |> Result.withDefault VoronoiDiagram2d.empty
        |> VoronoiDiagram2d.polygons (BoundingBox2d.from (Point2d.pixels padding padding) (Point2d.pixels (w - padding) (h - padding)))
        |> List.map
            (\( point, polygon ) ->
                TypedSvg.path
                    [ TypedSvg.Attributes.d (polygonToStr polygon)
                    , fill <| Paint Color.white
                    , stroke <| Paint Color.black
                    , TypedSvg.Events.onMouseEnter (Hover point)
                    , class [ "voronoi" ]
                    , TypedSvg.Events.onMouseLeave EndHover
                    ]
                    []
            )
        |> List.reverse
        |> g []


makeBinScale : List (Histogram.Bin Float Float) -> ContinuousScale Float
makeBinScale bins =
    bins
        |> List.map (.length >> toFloat)
        |> List.maximum
        |> Maybe.withDefault 0
        |> Tuple.pair 0
        |> Scale.linear ( 0, padding / 2 )


isInBin : Histogram.Bin Float Float -> Maybe Float -> Bool
isInBin { x0, x1 } x =
    case x of
        Just v ->
            v >= x0 && v < x1

        Nothing ->
            False


marginalHistogramTopAxis : Model -> Svg msg
marginalHistogramTopAxis model =
    let
        bins =
            histogram (List.map Tuple.first model.data)

        binScale =
            makeBinScale bins
    in
    bins
        |> List.map
            (\bin ->
                rect
                    [ x (Scale.convert xScale bin.x0)
                    , y 5
                    , width <| Scale.convert xScale bin.x1 - Scale.convert xScale bin.x0
                    , height (Scale.convert binScale (toFloat bin.length))
                    , opacity
                        (Opacity
                            (if isInBin bin (Maybe.map Tuple.first model.hovered) then
                                0.2

                             else
                                0.15
                            )
                        )
                    ]
                    []
            )
        |> g []


marginalHistogramRightAxis : Model -> Svg msg
marginalHistogramRightAxis model =
    let
        bins =
            histogram (List.map Tuple.second model.data)

        binScale =
            makeBinScale bins
    in
    bins
        |> List.map
            (\bin ->
                rect
                    [ y (Scale.convert yScale bin.x1)
                    , x (w - Scale.convert binScale (toFloat bin.length) - 4)
                    , height <| Scale.convert yScale bin.x0 - Scale.convert yScale bin.x1
                    , width (Scale.convert binScale (toFloat bin.length))
                    , opacity
                        (Opacity
                            (if isInBin bin (Maybe.map Tuple.second model.hovered) then
                                0.2

                             else
                                0.15
                            )
                        )
                    ]
                    []
            )
        |> g []


view : Model -> Svg Msg
view model =
    Html.div []
        [ style [] [ text """
            input, label {
                position: absolute;
                bottom: 4px;
                left: 30px;
            }
            label { left: 50px}
            .voronoi {
                opacity: 0;
            }
            
            #show-voronoi:checked ~ svg .voronoi {
                opacity: 0.2;
            }

            .crosshairs .domain {
                display: none;
            }
            .crosshairs .tick line {
                stroke: #000;
                stroke-width: .5px;
                stroke-dasharray: 2, 6;
            }

          """ ]
        , Html.input [ Html.Attributes.type_ "checkbox", Html.Attributes.id "show-voronoi" ] []
        , Html.label [ Html.Attributes.for "show-voronoi" ] [ Html.text "Show Voronoi" ]
        , svg [ viewBox 0 0 w h ]
            [ marginalHistogramTopAxis model
            , marginalHistogramRightAxis model
            , g [ transform [ Translate 0 (h - padding) ], class [ "crosshairs" ] ]
                [ Axis.bottom
                    [ Axis.tickSizeInner -h
                    , Axis.ticks (model.hovered |> Maybe.map (Tuple.first >> List.singleton) |> Maybe.withDefault [])
                    ]
                    xScale
                ]
            , g [ transform [ Translate padding 0 ], class [ "crosshairs" ] ]
                [ Axis.left
                    [ Axis.tickSizeInner -w
                    , Axis.ticks (model.hovered |> Maybe.map (Tuple.second >> List.singleton) |> Maybe.withDefault [])
                    ]
                    yScale
                ]
            , Svg.Lazy.lazy circles model.data
            , Svg.Lazy.lazy voronoiOverlay model.data
            ]
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { data : List ( Float, Float )
    , hovered : Maybe ( Float, Float )
    }


type Msg
    = Generated (List ( Float, Float ))
    | Hover ( Float, Float )
    | EndHover


init : () -> ( Model, Cmd Msg )
init =
    \() -> ( { data = [], hovered = Nothing }, Random.generate Generated dataPoints )


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Generated data ->
            ( { model | data = data }, Cmd.none )

        Hover point ->
            ( { model | hovered = Just point }, Cmd.none )

        EndHover ->
            ( { model | hovered = Nothing }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


randomBetween : Float -> Float -> Random.Generator Float
randomBetween min max =
    Random.map2 (\a b -> (a + b) / 2) (Random.float min max) (Random.float min max)


type Fruit
    = Apple
    | Orange
    | Pear


dataPoints : Random.Generator (List ( Float, Float ))
dataPoints =
    Random.list 200
        (Random.map3
            (\x y cat ->
                case cat of
                    Apple ->
                        ( x
                        , 0.25 * x ^ 2 - 0.5 * x + y
                        )

                    Orange ->
                        ( x + sqrt (y / 10) - 0.02 * y
                        , y
                        )

                    Pear ->
                        ( x, y )
            )
            (randomBetween 10 50)
            (randomBetween 0 200)
            (Random.weighted ( 0.3, Apple ) [ ( 0.6, Orange ), ( 0.1, Pear ) ])
        )
