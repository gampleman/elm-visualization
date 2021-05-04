module DetailChart exposing (..)

{-| @category Advanced
-}

import Axis
import Browser
import Browser.Events
import Brush exposing (Brush, OnBrush, OneDimensional)
import Color
import Events
import Json.Decode as D exposing (Decoder)
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
            ( { model
                | zoom = Zoom.update zoomMsg model.zoom
              }
            , Cmd.none
            )

        BrushMsg brushMsg ->
            ( { model | brush = Brush.update brushMsg model.brush }, Cmd.none )


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
            List.filterMap
                (\( x, y ) ->
                    let
                        x_ =
                            Time.posixToMillis x
                    in
                    if x_ + 35000 >= Time.posixToMillis min && x_ - 35000 <= Time.posixToMillis max then
                        Just (Just ( Scale.convert actualXScale x, Scale.convert yScale y ))

                    else
                        Nothing
                )
                model.data
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
            model.data
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
                model.data
                |> Shape.line Shape.monotoneInXCurve
    in
    g []
        [ Path.element line [ stroke <| Paint Color.blue, fill PaintNone ]
        , g [ transform [ Translate padding 0 ] ] [ Axis.left [ Axis.tickCount 4 ] yScale ]
        , g [ transform [ Translate 0 (h - padding) ] ] [ Axis.bottom [] xScale ]
        , Brush.view [] BrushMsg model.brush
        ]


brushView : ( ( Float, Float ), ( Float, Float ) ) -> ( Float, Float ) -> Svg Msg
brushView ( ( left, top ), ( right, bottom ) ) ( min, max ) =
    let
        handleSizeHalf =
            3
    in
    g [ fill PaintNone, pointerEvents "all" ]
        [ rect
            [ x left
            , y top
            , width (right - left)
            , height (bottom - top)
            , cursor CursorCrosshair
            , pointerEvents "all"
            ]
            []
        , rect
            [ x min
            , y top
            , width (max - min)
            , height (bottom - top)
            , cursor CursorMove
            , pointerEvents "all"
            , fill (Paint (Color.rgb 0.43 0.43 0.43))
            , fillOpacity (Opacity 0.3)
            , stroke (Paint Color.white)
            , shapeRendering RenderCrispEdges
            ]
            []
        , rect
            [ x (min - handleSizeHalf)
            , y (top - handleSizeHalf)
            , width (handleSizeHalf * 2)
            , height (bottom - top + handleSizeHalf * 2)
            , cursor (Cursor "ew-resize")
            , pointerEvents "all"
            ]
            []
        , rect
            [ x max
            , y (top - handleSizeHalf)
            , width (handleSizeHalf * 2)
            , height (bottom - top + handleSizeHalf * 2)
            , cursor (Cursor "ew-resize")
            , pointerEvents "all"
            ]
            []
        ]


view : Model -> Svg Msg
view model =
    let
        zoom =
            Zoom.asRecord model.zoom

        xScale =
            model.data
                |> List.map Tuple.first
                |> Statistics.extentBy Time.posixToMillis
                |> Maybe.withDefault ( Time.millisToPosix 0, Time.millisToPosix 0 )
                |> Scale.time Time.utc ( padding, w - padding )

        bounds =
            ( Scale.invert xScale (padding - zoom.translate.x / zoom.scale), Scale.invert xScale ((w - padding - zoom.translate.x) / zoom.scale) )
    in
    svg [ viewBox 0 0 w h, width w, height h ]
        [ detailChart bounds model
        , overviewChart xScale (Brush.selection1d model.brush |> Maybe.withDefault ( padding, w - padding )) model
        ]


type alias Model =
    { data : List ( Time.Posix, Float )
    , zoom : Zoom
    , brush : Brush OneDimensional
    }


type Msg
    = ZoomMsg OnZoom
    | BrushMsg OnBrush



-- | Wheeled Float ( Float, Float )
-- | GestureStarted
-- | Gestured Float ( Float, Float )
-- | TouchStarted (List Touch)
-- | TouchStartedExpired
-- | TouchMoved (List Touch)
-- | TouchEnded (List Touch)
-- | Tick Float


init : () -> ( Model, Cmd Msg )
init () =
    ( { data = Random.step timeseriesGenerator (Random.initialSeed 4354554) |> Tuple.first
      , zoom =
            Zoom.init { width = w - padding, height = h }
                |> Zoom.scaleExtent 1 10000
                |> Zoom.translateExtent ( ( 0, 0 ), ( w - padding, h ) )
      , brush = Brush.initX { n = h - overviewChartHeight, s = h - padding, w = padding, e = w - padding }
      }
    , Cmd.none
    )


timeseriesGenerator : Random.Generator (List ( Time.Posix, Float ))
timeseriesGenerator =
    Random.map
        (List.indexedMap
            (\idx noise ->
                ( Time.millisToPosix (idx * 30000 + 4534545), abs (sin (toFloat idx / 40) * 100 + noise + (toFloat idx / 60)) )
            )
        )
        (Random.list 10000 (Random.float -40 40))
