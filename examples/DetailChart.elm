module DetailChart exposing (..)

{-| @category Advanced
-}

import Axis
import Browser
import Browser.Events
import Brush exposing (Brush, OnBrush, OneDimensional)
import Circle3d exposing (at_)
import Color
import DateFormat
import Events
import Json.Decode as D exposing (Decoder)
import LTTB
import Path
import Random
import Scale
import Shape
import Statistics
import Svg.Events exposing (custom)
import Time
import TypedSvg exposing (g, rect, svg)
import TypedSvg.Attributes exposing (cursor, fill, fillOpacity, opacity, pointerEvents, shapeRendering, stroke, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (height, width, x, y)
import TypedSvg.Core exposing (Attribute, Svg)
import TypedSvg.Types exposing (Cursor(..), Opacity(..), Paint(..), ShapeRendering(..), Transform(..))
import Zoom exposing (OnZoom, Zoom)


w : Float
w =
    900


h : Float
h =
    450


padding : Float
padding =
    30


overviewChartHeight : Float
overviewChartHeight =
    130


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Zoom.subscriptions model.zoom ZoomMsg
        , Brush.subscriptions model.brush BrushMsg
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ZoomMsg zoomMsg ->
            let
                zoom =
                    Zoom.update zoomMsg model.zoom

                { scale, translate } =
                    Zoom.asRecord zoom

                bounds =
                    ( (padding - translate.x) / scale
                    , (w - padding - translate.x) / scale
                    )
            in
            ( { model
                | zoom = zoom
                , brush = Brush.setSelection1d Brush.instantly bounds model.brush
              }
            , Cmd.none
            )

        BrushMsg brushMsg ->
            let
                brush =
                    Brush.update brushMsg model.brush

                ( brushStart, brushEnd ) =
                    Brush.selection1d brush
                        |> Maybe.withDefault ( padding, width - padding )

                delta =
                    brushEnd - brushStart

                width =
                    w - 2 * padding

                zoomTransform =
                    { scale = width / delta
                    , translate = { x = padding - brushStart * (width / delta), y = 0 }
                    }
            in
            ( { model
                | brush = brush
                , zoom = Zoom.setTransform Zoom.instantly zoomTransform model.zoom
              }
            , Cmd.none
            )


detailChart ( min, max ) model =
    let
        yScale =
            model.data
                |> List.map Tuple.second
                |> List.maximum
                |> Maybe.withDefault 0
                |> Tuple.pair 0
                |> Scale.linear ( h - padding - overviewChartHeight, padding )

        actualXScale =
            Scale.time Time.utc ( padding, w - padding ) ( min, max )

        line =
            model.data
                |> List.filter
                    (\( x, _ ) ->
                        Time.posixToMillis x + 35000 >= Time.posixToMillis min && Time.posixToMillis x - 35000 <= Time.posixToMillis max
                    )
                |> downsample
                |> List.map (\( x, y ) -> Just ( Scale.convert actualXScale x, Scale.convert yScale y ))
                |> Shape.line Shape.monotoneInXCurve
    in
    g []
        [ Path.element line [ stroke <| Paint Color.blue, fill PaintNone ]
        , g [ transform [ Translate padding 0 ] ] [ Axis.left [] yScale ]
        , g [ transform [ Translate 0 (h - padding - overviewChartHeight) ] ] [ Axis.bottom [] actualXScale ]
        , rect (width w :: height h :: opacity (Opacity 0) :: Zoom.events model.zoom ZoomMsg) []
        ]


overviewChart xScale ( min, max ) model =
    let
        yScale =
            model.overviewData
                |> List.map Tuple.second
                |> List.maximum
                |> Maybe.withDefault 0
                |> Tuple.pair 0
                |> Scale.linear ( h - padding, h - overviewChartHeight )

        line =
            List.map
                (\( x, y ) ->
                    Just ( Scale.convert xScale x, Scale.convert yScale y )
                )
                model.overviewData
                |> Shape.line Shape.monotoneInXCurve
    in
    g []
        [ Path.element line [ stroke <| Paint Color.blue, fill PaintNone ]
        , g [ transform [ Translate padding 0 ] ] [ Axis.left [ Axis.tickCount 4 ] yScale ]
        , g [ transform [ Translate 0 (h - padding) ] ] [ Axis.bottom [] xScale ]
        , Brush.view [] BrushMsg model.brush
        ]


view : Model -> Svg Msg
view model =
    let
        bounds =
            Brush.selection1d model.brush |> Maybe.withDefault ( padding, w - padding )

        xScale =
            model.data
                |> List.map Tuple.first
                |> Statistics.extentBy Time.posixToMillis
                |> Maybe.withDefault ( Time.millisToPosix 0, Time.millisToPosix 0 )
                |> Scale.time Time.utc ( padding, w - padding )
    in
    svg [ viewBox 0 0 w h, width w, height h ]
        [ detailChart (Tuple.mapBoth (Scale.invert xScale) (Scale.invert xScale) bounds) model
        , overviewChart xScale bounds model
        ]


type alias Model =
    { data : List ( Time.Posix, Float )
    , overviewData : List ( Time.Posix, Float )
    , zoom : Zoom
    , brush : Brush OneDimensional
    }


type Msg
    = ZoomMsg OnZoom
    | BrushMsg OnBrush


init : () -> ( Model, Cmd Msg )
init () =
    let
        data =
            Random.step timeseriesGenerator (Random.initialSeed 4354554) |> Tuple.first
    in
    ( { data = data
      , overviewData = downsample data
      , zoom =
            Zoom.init { width = w - padding, height = h }
                |> Zoom.scaleExtent 1 22
                |> Zoom.translateExtent ( ( 0, 0 ), ( w, h - overviewChartHeight ) )
      , brush = Brush.initX { top = h - overviewChartHeight, bottom = h - padding, left = padding, right = w - padding }
      }
    , Cmd.none
    )


downsample data =
    LTTB.downsample
        { data = data
        , threshold = floor w
        , xGetter = Tuple.first >> Time.posixToMillis >> toFloat
        , yGetter = Tuple.second
        }


timeseriesGenerator : Random.Generator (List ( Time.Posix, Float ))
timeseriesGenerator =
    Random.map
        (List.indexedMap
            (\idx noise ->
                ( Time.millisToPosix (idx * 30000 + 4534545), abs (sin (toFloat idx / 40) * 100 + noise + (toFloat idx / 60)) )
            )
        )
        (Random.list 10000 (Random.float -40 40))
