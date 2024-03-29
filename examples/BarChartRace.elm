module BarChartRace exposing (main)

{-| Demonstrates a complex animation.

@requires data/category-brands.csv
@category Advanced

-}

import Axis
import Browser
import Browser.Events
import Color
import Csv.Decode as Csv
import DateFormat
import Dict exposing (Dict)
import Example
import Html exposing (Html)
import Http
import Interpolation exposing (Interpolator)
import Iso8601
import List.Extra
import Scale exposing (ContinuousScale, OrdinalScale)
import Scale.Color
import Set
import Time
import Transition exposing (Transition)
import TypedSvg exposing (g, rect, svg, text_, tspan)
import TypedSvg.Attributes exposing (dy, fill, fillOpacity, fontWeight, style, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (height, width, x, y)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), FontWeight(..), Opacity(..), Paint(..), Transform(..), em)


w : Float
w =
    990


h : Float
h =
    504


barSize : Float
barSize =
    (h - 2 * margin) / (n * 1.1)


type alias Brand =
    { name : String
    , value : Float
    , rank : Float
    , category : String
    }


type alias BrandWithTime =
    { category : String
    , name : String
    , time : Time.Posix
    , value : Float
    }


type alias Frame =
    ( Time.Posix, List Brand )


type alias ColorScale =
    OrdinalScale String Color.Color


type Model
    = Loading
    | Error Http.Error
    | Loaded { transition : Transition Frame, categories : List String }


type Msg
    = ReceivedData (Result Http.Error (List RawBrand))
    | Tick Int


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


margin : number
margin =
    10


paddingY : number
paddingY =
    10


n : number
n =
    12


init : () -> ( Model, Cmd Msg )
init () =
    ( Loading
    , Http.get
        { url = "data/category-brands.csv"
        , expect = expectCsv ReceivedData decoder
        }
    )


expectCsv : (Result Http.Error (List a) -> msg) -> Csv.Decoder a -> Http.Expect msg
expectCsv tagger decode =
    Http.expectString
        (Result.andThen
            (\data ->
                Csv.decodeCsv Csv.FieldNamesFromFirstRow decode data
                    |> Result.mapError (Csv.errorToString >> Http.BadBody)
            )
            >> tagger
        )


type alias RawBrand =
    { name : String, value : Float, category : String, time : Time.Posix }


decoder : Csv.Decoder RawBrand
decoder =
    Csv.into RawBrand
        |> Csv.pipeline (Csv.field "name" Csv.string)
        |> Csv.pipeline (Csv.field "value" Csv.float)
        |> Csv.pipeline (Csv.field "category" Csv.string)
        |> Csv.pipeline (Csv.field "date" dateDecoder)


dateDecoder : Csv.Decoder Time.Posix
dateDecoder =
    Csv.string
        |> Csv.andThen (Iso8601.toTime >> Result.mapError (always "Could not parse date") >> Csv.fromResult)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ReceivedData (Ok rawData), _ ) ->
            ( Loaded
                { transition = buildTransition rawData
                , categories =
                    rawData
                        |> List.map .category
                        |> Set.fromList
                        |> Set.toList
                }
            , Cmd.none
            )

        ( ReceivedData (Err e), _ ) ->
            ( Error e, Cmd.none )

        ( Tick ms, Loaded m ) ->
            ( Loaded { m | transition = Transition.step ms m.transition }, Cmd.none )

        _ ->
            ( model, Cmd.none )


groupBy : (a -> comparable) -> List a -> Dict comparable (List a)
groupBy accessor =
    List.foldr
        (\item dict ->
            let
                key =
                    accessor item

                vals =
                    Dict.get key dict |> Maybe.withDefault []
            in
            Dict.insert key (item :: vals) dict
        )
        Dict.empty


buildTransition : List RawBrand -> Transition Frame
buildTransition data =
    let
        baseFrames =
            groupBy (.time >> Time.posixToMillis) data
                |> Dict.toList
    in
    baseFrames
        |> List.sortBy Tuple.first
        |> generateSubframes
        |> List.map (\( time, brands ) -> ( Time.millisToPosix time, List.sortBy .value brands |> List.reverse |> List.indexedMap makeBrand ))
        |> List.Extra.uncons
        |> Maybe.map (\( head, tail ) -> Interpolation.piecewise buildInterpolation head tail)
        |> Maybe.map (Transition.easeFor (List.length baseFrames * duration) Transition.easeLinear)
        |> Maybe.withDefault (Transition.constant ( Time.millisToPosix 0, [] ))


k =
    10


generateSubframes : List ( Int, List RawBrand ) -> List ( Int, List RawBrand )
generateSubframes frameList =
    List.map2 (\before after -> Interpolation.tuple Interpolation.int interpolateSubframe before after |> Interpolation.samples k |> List.drop 1)
        frameList
        (List.tail frameList |> Maybe.withDefault [])
        |> List.concat


interpolateSubframe : List RawBrand -> List RawBrand -> Interpolator (List BrandWithTime)
interpolateSubframe from to =
    Dict.merge
        (\name fromItem -> Dict.insert name (interpolateRawBrand fromItem { fromItem | value = 0 }))
        (\name fromItem toItem -> Dict.insert name (interpolateRawBrand fromItem toItem))
        (\name toItem -> Dict.insert name (interpolateRawBrand { toItem | value = 0 } toItem))
        (indexByName from)
        (indexByName to)
        Dict.empty
        |> Dict.values
        |> Interpolation.inParallel


interpolateRawBrand : RawBrand -> RawBrand -> Interpolator BrandWithTime
interpolateRawBrand from to =
    Interpolation.map (\value -> { to | value = value })
        (Interpolation.float from.value to.value)


makeBrand : Int -> RawBrand -> Brand
makeBrand index rawBrand =
    { name = rawBrand.name, value = rawBrand.value, rank = toFloat index, category = rawBrand.category }


buildInterpolation : Frame -> Frame -> Interpolator Frame
buildInterpolation =
    Interpolation.tuple
        interpolateTime
        interpolateBrandList


interpolateTime : Time.Posix -> Time.Posix -> Interpolator Time.Posix
interpolateTime fromTime toTime =
    Interpolation.int (Time.posixToMillis fromTime) (Time.posixToMillis toTime) >> Time.millisToPosix


indexByName : List { a | name : String } -> Dict String { a | name : String }
indexByName =
    List.foldr (\item dict -> Dict.insert item.name item dict) Dict.empty


interpolateBrandList : List Brand -> List Brand -> Interpolator (List Brand)
interpolateBrandList from to =
    let
        fromByName =
            indexByName from

        toByName =
            indexByName to

        fromTop =
            List.take n from

        toTop =
            List.take n to

        fetch item =
            Dict.get item.name >> Maybe.withDefault item
    in
    Interpolation.list
        { add = \toB -> interpolateBrand (fetch toB fromByName) toB
        , remove = \fromB -> interpolateBrand fromB (fetch fromB toByName)
        , change = interpolateBrand
        , id = .name
        , combine = Interpolation.combineParallel
        }
        fromTop
        toTop


interpolateBrand : Brand -> Brand -> Interpolator Brand
interpolateBrand from to =
    Interpolation.map2 (\rank value -> { to | rank = rank, value = value })
        (Interpolation.float from.rank to.rank)
        (Interpolation.float from.value to.value)


view : Model -> Html Msg
view model =
    case model of
        Loading ->
            Example.loading []

        Error err ->
            Example.error Nothing err

        Loaded { transition, categories } ->
            viewChart categories (Transition.value transition)


viewChart : List String -> ( Time.Posix, List Brand ) -> Svg Msg
viewChart categories ( now, data ) =
    let
        xScale =
            List.map .value data
                |> List.maximum
                |> Maybe.withDefault 0
                |> Tuple.pair 0
                |> Scale.linear ( margin, w - margin )

        yScale =
            Scale.linear ( margin, h - margin ) ( 0, n )

        colorScale =
            Scale.ordinal Scale.Color.tableau10 categories
    in
    svg [ viewBox 0 0 w h ]
        [ viewBars colorScale xScale yScale data
        , viewAxes xScale
        , viewLabels xScale yScale data
        , viewTicker now
        ]


duration =
    2500


viewAxes : ContinuousScale Float -> Svg msg
viewAxes scale =
    g [ transform [ Translate 0 margin ] ]
        [ TypedSvg.Core.node "style" [] [ text """
          .tick:first-of-type text {
            display: none;
          }
          .tick:not(:first-of-type) line {
            stroke: white;
          }
          .domain {
            display: none;
          }
        """ ]
        , Axis.top
            [ Axis.tickCount (round w // 160)
            , Axis.tickSizeOuter 0
            , Axis.tickSizeInner (-barSize * (n + paddingY))
            ]
            scale
        ]


viewBars : ColorScale -> ContinuousScale Float -> ContinuousScale Float -> List Brand -> Svg msg
viewBars colorScale xScale yScale data =
    List.map
        (\datum ->
            rect
                [ fill <| Paint <| (Scale.convert colorScale datum.category |> Maybe.withDefault Color.black)
                , height barSize
                , x (Scale.convert xScale 0)
                , y (Scale.convert yScale datum.rank)
                , width (Scale.convert xScale datum.value - Scale.convert xScale 0)
                ]
                []
        )
        data
        |> g [ fillOpacity <| Opacity 0.6 ]


viewLabels : ContinuousScale Float -> ContinuousScale Float -> List Brand -> Svg msg
viewLabels xScale yScale data =
    List.map
        (\datum ->
            text_
                [ transform [ Translate (Scale.convert xScale datum.value) (Scale.convert yScale datum.rank) ]
                , height barSize
                , x -6
                , dy (em -0.25)
                , y (barSize / 2)
                , width (Scale.convert xScale datum.value - Scale.convert xScale 0)
                ]
                [ text datum.name
                , tspan
                    [ fillOpacity <| Opacity 0.7
                    , fontWeight FontWeightNormal
                    , x -6
                    , dy (em 1.15)
                    ]
                    [ text (String.fromInt (round datum.value)) ]
                ]
        )
        data
        |> g [ style "font: bold 12px sans-serif; font-variant-numeric: tabular-nums;", textAnchor AnchorEnd ]


viewTicker : Time.Posix -> Svg msg
viewTicker time =
    text_
        [ style ("font: bold " ++ String.fromFloat barSize ++ "px sans-serif; font-variant-numeric: tabular-nums")
        , textAnchor AnchorEnd
        , x (w - 6)
        , y (margin + barSize * (toFloat n - 0.45))
        , dy (em 0.32)
        ]
        [ text (formatYear time) ]


formatYear : Time.Posix -> String
formatYear =
    DateFormat.format [ DateFormat.yearNumber ] Time.utc


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Loading ->
            Sub.none

        Error _ ->
            Sub.none

        Loaded record ->
            if Transition.isComplete record.transition then
                Sub.none

            else
                Browser.Events.onAnimationFrameDelta (round >> Tick)
