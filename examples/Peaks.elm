module Peaks exposing (main)

{-| Shows number of daily page views of Wikipedia pages.

This example demonstrates:

  - Fetching and parsing data from a remote API.
  - Building a simple line chart.
  - Using [`Statistics.peaks`](https://package.elm-lang.org/packages/gampleman/elm-visualization/latest/Statistics#peaks)
    to show peaks in the dataset.
  - Some shenigans to get labels to position themselves reasonably.

Based on a [notebook](https://observablehq.com/@yurivish/peak-detection) by Yuri Vishnevsky.
@delay 6
@todo Interactive hover based tooltip with snapping to peaks.
@todo Interactive controls for tuning the peaks mechanism.

-}

import Axis
import Browser
import Color exposing (Color)
import DateFormat
import Example
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Http
import Iso8601
import Json.Decode as Decode exposing (Decoder)
import Path exposing (Path)
import Scale exposing (ContinuousScale)
import Shape
import Statistics
import Time
import TypedSvg exposing (defs, g, linearGradient, stop, svg, text_)
import TypedSvg.Attributes exposing (class, dy, fill, fontFamily, id, offset, stopColor, stroke, textAnchor, transform, viewBox, x1, x2, y1, y2)
import TypedSvg.Attributes.InPx exposing (fontSize, height, strokeWidth, width, x, y)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Paint(..), Transform(..), em, percent)
import Url.Builder



-- Constants


w : Float
w =
    990


h : Float
h =
    440


padding : Float
padding =
    40


beginning : Time.Posix
beginning =
    time "2018-01-01"


ending : Time.Posix
ending =
    time "2019-01-01"


initialQuery : String
initialQuery =
    "Cadbury Creme Egg"



-- Getting the data


type alias Data =
    List ( Time.Posix, Float )


type Granularity
    = Daily


granularityToString : Granularity -> String
granularityToString g =
    case g of
        Daily ->
            "daily"


timestamp : Time.Posix -> String
timestamp =
    DateFormat.format [ DateFormat.yearNumber, DateFormat.monthFixed, DateFormat.dayOfMonthFixed ] Time.utc


getData : String -> Time.Posix -> Time.Posix -> Granularity -> Cmd Msg
getData page start end granularity =
    Http.get
        { url =
            Url.Builder.crossOrigin "https://wikimedia.org"
                [ "api"
                , "rest_v1"
                , "metrics"
                , "pageviews"
                , "per-article"
                , "en.wikipedia"
                , "all-access"
                , "user"
                , normalizeQuery page
                , granularityToString granularity
                , timestamp start
                , timestamp end
                ]
                []
        , expect = Http.expectJson GotData (decoder start end)
        }


{-| Wikipedia pages want spaces replaced by underscores.
-}
normalizeQuery : String -> String
normalizeQuery =
    String.replace " " "_"


{-| The timestamp format used in the API is YYYYMMDDHH. However, in daily/monthly
granularity the hour is always `00`, so we ingore it and then rely on the Iso8601 parser to get the actual time.
-}
timestampDecoder : Decoder Time.Posix
timestampDecoder =
    Decode.string
        |> Decode.andThen
            (\tstr ->
                case Iso8601.toTime (String.left 8 tstr) of
                    Ok t ->
                        Decode.succeed t

                    Err e ->
                        Decode.fail "Couldn't parse time"
            )


decoder : Time.Posix -> Time.Posix -> Decoder Data
decoder start end =
    Decode.field "items"
        (Decode.list
            (Decode.map2 Tuple.pair (Decode.field "timestamp" timestampDecoder) (Decode.field "views" Decode.float))
        )


{-| Helper to make time literals.
-}
time : String -> Time.Posix
time =
    Iso8601.toTime >> Result.withDefault (Time.millisToPosix 0)



-- Visualization


mainColor : Color
mainColor =
    Color.rgb 0.2 0.1 0.95


{-| Generates the curve that represents the data.
-}
line : ContinuousScale Float -> Data -> Path
line yScale data =
    data
        |> List.map (\( x, y ) -> Just ( Scale.convert xScale x, Scale.convert yScale y ))
        |> Shape.line Shape.monotoneInXCurve


{-| Generates the curve under the data, which we fill with a subtle gradient. This is here simply for effect.
-}
area : ContinuousScale Float -> Data -> Path
area yScale data =
    data
        |> List.map
            (\( x, y ) ->
                Just
                    ( ( Scale.convert xScale x, Tuple.first (Scale.rangeExtent yScale) )
                    , ( Scale.convert xScale x, Scale.convert yScale y )
                    )
            )
        |> Shape.area Shape.linearCurve


{-| This scale transforms times into horizontal position.
-}
xScale : ContinuousScale Time.Posix
xScale =
    Scale.time Time.utc ( padding, w - padding ) ( beginning, ending )


{-| This is a path representing the litle arrows pointing out the peaks.
-}
arrow : String
arrow =
    "M2.73484 7.26517C2.88128 7.41161 3.11872 7.41161 3.26517 7.26517L5.65165 4.87868C5.7981 4.73223 5.7981 4.4948 5.65165 4.34835C5.5052 4.2019 5.26777 4.2019 5.12132 4.34835L3 6.46967L0.87868 4.34835C0.732233 4.2019 0.494796 4.2019 0.34835 4.34835C0.201903 4.4948 0.201903 4.73223 0.34835 4.87868L2.73484 7.26517ZM2.625 1.63918e-08L2.625 7L3.375 7L3.375 -1.63918e-08L2.625 1.63918e-08Z"


{-| Reneders the little callouts for the peaks.
-}
peaksView : ContinuousScale Float -> Data -> List (Svg Msg)
peaksView yScale data =
    data
        |> Statistics.peaks Tuple.second { lookaround = 5, sensitivity = 2, coallesce = 15 }
        |> List.map
            (\( x, y ) ->
                let
                    xpos =
                        Scale.convert xScale x

                    anchor =
                        if xpos - padding < 50 then
                            AnchorStart

                        else if xpos + padding > w - 50 then
                            AnchorEnd

                        else
                            AnchorMiddle
                in
                g [ transform [ Translate (xpos - 3) (Scale.convert yScale y - 12) ] ]
                    [ TypedSvg.path [ TypedSvg.Attributes.d arrow, fill (Paint Color.red) ] []
                    , TypedSvg.text_ [ fontSize 11, fontFamily [ "sans-serif" ], textAnchor anchor, TypedSvg.Attributes.InPx.y -5 ]
                        [ text
                            (DateFormat.format
                                [ DateFormat.dayOfMonthNumber
                                , DateFormat.text " "
                                , DateFormat.monthNameFull
                                ]
                                Time.utc
                                x
                            )
                        ]
                    ]
            )


chart : Data -> Svg Msg
chart data =
    let
        yScale =
            data
                |> List.map Tuple.second
                |> List.maximum
                |> Maybe.withDefault 0.01
                |> max 0.01
                |> Tuple.pair 0
                |> Scale.linear ( h - padding, padding )
    in
    svg [ viewBox 0 0 w h, width w, height h ]
        [ defs []
            [ linearGradient [ id "gradient", x1 (percent 0), y1 (percent 0), x2 (percent 0), y2 (percent 100) ]
                [ stop [ offset "0%", stopColor "#599EFF" ] []
                , stop [ offset "100%", stopColor "#EFF6FF" ] []
                ]
            ]
        , Path.element (area yScale data) [ fill (Reference "gradient") ]
        , Path.element (line yScale data) [ stroke (Paint mainColor), fill PaintNone ]
        , g [ transform [ Translate 0 (h - padding) ] ]
            [ Axis.bottom [ Axis.tickCount 2 ] xScale ]
        , g [ transform [ Translate (padding - 1) 0 ] ]
            [ Axis.left [ Axis.tickCount 5 ] yScale ]
        , g [] <| peaksView yScale data
        ]



-- View


view : Model -> Html Msg
view { data, query } =
    Html.div []
        [ Html.node "style" [] [ Html.text css ]
        , Html.h1 []
            [ Html.text "Daily views of "
            , Html.input
                [ Html.Attributes.type_ "text"
                , Html.Attributes.value query
                , Html.Events.onInput TypedIntoField
                ]
                []
            , Html.text " on Wikipedia "
            , Html.button [ Html.Events.onClick Submitted ] [ Html.text "Submit" ]
            ]
        , case data of
            Loading ->
                Example.loading []

            Error e ->
                Html.div [ Html.Attributes.style "margin" "20px" ]
                    [ Example.error (Just Submitted) e
                    ]

            Success d ->
                chart d
        ]


css : String
css =
    """
    body {
        font: 16px -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif;
    }
    h1 {
        margin-left: 20px;
        font-size: 16px;
    }
    h1 input[type=text], h1 button {
        font-size: 32px;
    }
    """



-- Application


type Msg
    = GotData (Result Http.Error Data)
    | TypedIntoField String
    | Submitted


type alias Model =
    { data : Remote Data
    , query : String
    }


type Remote a
    = Loading
    | Error Http.Error
    | Success a



-- Init


init : () -> ( Model, Cmd Msg )
init () =
    ( { data = Loading, query = initialQuery }, getData initialQuery beginning ending Daily )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotData (Ok result) ->
            ( { model | data = Success result }, Cmd.none )

        GotData (Err e) ->
            ( { model | data = Error e }, Cmd.none )

        TypedIntoField s ->
            ( { model | query = s }, Cmd.none )

        Submitted ->
            ( { model | data = Loading }, getData model.query beginning ending Daily )



-- TEA


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
