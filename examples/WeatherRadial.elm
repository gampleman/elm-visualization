module WeatherRadial exposing (main)

{-| Visualizes a whole year of weather in any city in the world.

I've always loved the [Weather Radials poster](http://www.weather-radials.com),
but also would have preferred if the cities they chose were places I have lived or
visited. This example lets you do just that - pick a city and get your own weather radial.
You can even copy the SVG code and make your own poster!

This is a very complex example, so if you are just getting started with elm-visualization
you may want to study the other examples first. This example uses 4 different APIs to gather
its data:

1.  Geocoding is provided by [Open Meteo](https://open-meteo.com/en/docs/geocoding-api) and it
    allows us to send a query for a location and get a latitude, longitude and elevation point back.
2.  We then use their [Historical Weather API](https://open-meteo.com/en/docs/historical-weather-api)
    to fetch 1 year of data for the specified location.
3.  Then we use the [Wikidata SPARQL service](https://www.wikidata.org/wiki/Wikidata:SPARQL_query_service/Wikidata_Query_Help)
    to run a rather elaborate query to try to find weather related wikipedia pages concerning the current country and time span.
4.  Finally we use the [Wikipedia Extracts API](https://www.mediawiki.org/w/api.php?action=help&modules=query%2Bextracts)
    to get a 1 sentence summary of those pages to use as annotations.

The resulting annotations are then filtered to find the most relevant looking ones based on detected
peaks in the weather data. (BTW: if you have a better idea how to find and/or fetch better annotation
data, please let me know - last time I did any SPARQL was in University).

Finally we use a small force simulation to position the annotations around the radial to help prevent
elements overlapping.

@category Advanced

-}

import Browser
import Color exposing (Color)
import Dict exposing (Dict)
import Example
import Force
import Html exposing (Html)
import Html.Attributes as A exposing (class)
import Html.Events
import Html.Lazy
import Http
import Iso8601
import Json.Decode as Decode exposing (Decoder)
import LowLevel.Command as C
import Path
import Process
import Regex
import Scale exposing (ContinuousScale, SequentialScale)
import Scale.Color
import Statistics
import SubPath
import Task
import Time
import TypedSvg exposing (g)
import TypedSvg.Attributes exposing (dy, fill, fontFamily, stroke, strokeDasharray, strokeLinecap, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, dx, fontSize, height, r, strokeWidth, width, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (text)
import TypedSvg.Types exposing (AnchorAlignment(..), Paint(..), StrokeLinecap(..), Transform(..), em)
import Url



-- Constants


w : Float
w =
    990


h : Float
h =
    504


radius : Float
radius =
    min w h / 2



-- Types


type Loading a
    = Loading
    | Error Http.Error
    | Success a


type alias Model =
    { weather : Loading WeatherData
    , annotations : Loading (List RawAnnotation)
    , name : String
    , location : Location
    , geocodingResults : Loading (List GeocodingResult)
    , editing : Bool
    , nameInput : String
    }


type alias WeatherData =
    { timestamps : List Time.Posix
    , temperatureMin : List Float
    , temperatureMax : List Float
    , temperatureMean : List Float
    , precipitationSum : List Float
    , timeZone : Time.Zone
    }


type alias GeocodingResult =
    { name : String, country : String, admin1 : String, location : Location }


type alias RawAnnotation =
    { name : String
    , start : Time.Posix
    , end : Time.Posix
    , wikipedia : String
    , description : String
    }


type alias Annotation =
    { name : String
    , start : Time.Posix
    , end : Time.Posix
    , wikipedia : String
    , phenomenon : Phenomenon
    , anchor : Float
    , description : String
    }


type Phenomenon
    = Temperature
    | Precipitation


type alias Location =
    { lat : Float, lng : Float, elevation : Float }


type Msg
    = GotGeocoding (Result Http.Error (List GeocodingResult))
    | GotWeather (Result Http.Error WeatherData)
    | GotAnnotations (Result Http.Error (List RawAnnotation))
    | EditedInput String
    | Editing Bool
    | FetchGeocoding String
    | SelectedLocation String String Location



-- Entrypoint


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }



-- HTTP & Decoders


getGeocoding : String -> Cmd Msg
getGeocoding city =
    Http.get
        { url = "https://geocoding-api.open-meteo.com/v1/search?name=" ++ Url.percentEncode city ++ "&count=10&language=en&format=json"
        , expect = Http.expectJson GotGeocoding decodeGeocoding
        }


decodeWithDefault : a -> Decoder a -> Decoder a
decodeWithDefault val dec =
    Decode.oneOf
        [ dec
        , Decode.succeed val
        ]


decodeGeocoding : Decoder (List { name : String, country : String, admin1 : String, location : Location })
decodeGeocoding =
    Decode.field "results"
        (Decode.list
            (Decode.map6
                (\name country admin1 lat lng elevation ->
                    { name = name, country = country, admin1 = admin1, location = { lat = lat, lng = lng, elevation = elevation } }
                )
                (Decode.field "name" Decode.string)
                (Decode.field "country_code" Decode.string)
                (Decode.oneOf
                    [ Decode.field "admin1" Decode.string
                    , Decode.field "admin2" Decode.string
                    , Decode.field "admin3" Decode.string
                    , Decode.field "admin4" Decode.string
                    , Decode.succeed ""
                    ]
                )
                (Decode.field "latitude" Decode.float)
                (Decode.field "longitude" Decode.float)
                (decodeWithDefault 0 (Decode.field "elevation" Decode.float))
            )
        )


getWeather : Location -> Cmd Msg
getWeather location =
    Http.get
        { url = "https://archive-api.open-meteo.com/v1/archive?latitude=" ++ String.fromFloat location.lat ++ "&longitude=" ++ String.fromFloat location.lng ++ "&elevation=" ++ String.fromFloat location.elevation ++ "&start_date=2022-01-01&end_date=2022-12-31&daily=temperature_2m_max,temperature_2m_min,temperature_2m_mean,precipitation_sum&timezone=auto"
        , expect = Http.expectJson GotWeather decoderWeather
        }


decoderWeather : Decoder WeatherData
decoderWeather =
    Decode.map6
        (\timestamps temperatureMin temperatureMax temperatureMean precipitationSum utcOffsetSec ->
            { timestamps = timestamps, temperatureMin = temperatureMin, temperatureMax = temperatureMax, temperatureMean = temperatureMean, precipitationSum = precipitationSum, timeZone = Time.customZone (utcOffsetSec // 60) [] }
        )
        (Decode.at [ "daily", "time" ] (Decode.list Iso8601.decoder))
        (Decode.at [ "daily", "temperature_2m_min" ] (Decode.list Decode.float))
        (Decode.at [ "daily", "temperature_2m_max" ] (Decode.list Decode.float))
        (Decode.at [ "daily", "temperature_2m_mean" ] (Decode.list Decode.float))
        (Decode.at [ "daily", "precipitation_sum" ] (Decode.list Decode.float))
        (Decode.field "utc_offset_seconds" Decode.int)


jsonResolver : Decoder a -> Http.Resolver Http.Error a
jsonResolver decoder =
    Http.stringResolver
        (\res ->
            case res of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata _ ->
                    Err (Http.BadStatus metadata.statusCode)

                Http.GoodStatus_ _ body ->
                    Result.mapError (Decode.errorToString >> Http.BadBody) (Decode.decodeString decoder body)
        )


getAnnotations : String -> Cmd Msg
getAnnotations country =
    Http.task
        { url = "https://query.wikidata.org/sparql?format=json&query=" ++ Url.percentEncode (annotationsQuery country)
        , resolver = jsonResolver decoderAnnotations
        , method = "GET"
        , headers = []
        , body = Http.emptyBody
        , timeout = Nothing
        }
        |> Task.andThen
            (\anno ->
                Http.task
                    { url = "https://en.wikipedia.org/w/api.php?origin=*&format=json&action=query&prop=extracts&exintro&explaintext&redirects=1&exsentences=1&titles=" ++ String.join "|" (List.map (.name >> Url.percentEncode) anno)
                    , resolver = jsonResolver decoderExtracts
                    , method = "GET"
                    , headers = []
                    , body = Http.emptyBody
                    , timeout = Nothing
                    }
                    |> Task.map (extractDescriptions anno)
            )
        |> Task.attempt GotAnnotations


extractDescriptions : List RawAnnotation -> ( Dict String String, Dict String String ) -> List RawAnnotation
extractDescriptions anno ( extracts, redirects ) =
    List.map
        (\a ->
            { a
                | description =
                    Dict.get (followRedirects a.name redirects) extracts
                        |> Maybe.withDefault ""
                        |> Regex.replace (Regex.fromString "\\(.*?\\)\\w*" |> Maybe.withDefault Regex.never) (always "")
            }
        )
        anno


followRedirects : String -> Dict String String -> String
followRedirects title redirects =
    case Dict.get title redirects of
        Just newTitle ->
            followRedirects newTitle redirects

        Nothing ->
            title


annotationsQuery : String -> String
annotationsQuery countryCode =
    """PREFIX wd: <http://www.wikidata.org/entity/>
PREFIX wdt: <http://www.wikidata.org/prop/direct/>
PREFIX schema: <http://schema.org/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT DISTINCT ?eventLabel ?startDate ?endDate ?article
WHERE {
  # Lookup for Wikidata entity based on FIPS country code
  ?country wdt:P297 \"""" ++ countryCode ++ """".
  
  # Find events related to weather (Metereological Phenomenon) with a start date
  ?event wdt:P31/wdt:P279* wd:Q16332653;
         wdt:P580 ?startDate. hint:Prior hint:rangeSafe true.
  
  # Filter events such that the date range is in the year of interest
  FILTER (?startDate >= "2022-01-01"^^xsd:dateTime && ?startDate <= "2022-12-31"^^xsd:dateTime)
  OPTIONAL { ?event wdt:P582 ?endDate.
             FILTER (?endDate <= "2022-12-31"^^xsd:dateTime) }
  
  # Grab label and description
  #?event schema:description ?eventDescription.
  #FILTER(LANG(?eventDescription) = "en")
  #?event rdfs:label ?eventLabel.
  #FILTER(LANG(?eventLabel) = "en")
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en" . }
  # Only match events that happened in the country
  ?event wdt:P17 ?country.
  
  # Grab wikipedia link
  ?article schema:about ?event ; schema:isPartOf <https://en.wikipedia.org/>.
  
  # Figure out outgoing links 
  # We use this to sort by "importance", however this slows the query down considerably.
  #?event wikibase:statements ?outcoming .
}
#ORDER BY DESC (?outcoming)
LIMIT 20
"""


decoderAnnotations : Decoder (List RawAnnotation)
decoderAnnotations =
    Decode.at [ "results", "bindings" ]
        (Decode.list
            (Decode.map4
                (\name start end wikipedia ->
                    { name = name, start = start, end = Maybe.withDefault (Time.millisToPosix (Time.posixToMillis start + 2 * 24 * 60 * 60 * 1000)) end, wikipedia = wikipedia, description = "" }
                )
                (Decode.at [ "eventLabel", "value" ] Decode.string)
                (Decode.at [ "startDate", "value" ] Iso8601.decoder)
                (Decode.maybe (Decode.at [ "endDate", "value" ] Iso8601.decoder))
                (Decode.at [ "article", "value" ] Decode.string)
            )
        )


decoderExtracts : Decoder ( Dict String String, Dict String String )
decoderExtracts =
    Decode.map2 Tuple.pair
        (Decode.at
            [ "query", "pages" ]
            (Decode.keyValuePairs (Decode.map2 Tuple.pair (Decode.field "title" Decode.string) (Decode.field "extract" Decode.string)) |> Decode.map (List.map Tuple.second >> Dict.fromList))
        )
        (Decode.maybe
            (Decode.at [ "query", "redirects" ]
                (Decode.list
                    (Decode.map2 Tuple.pair
                        (Decode.field "from" Decode.string)
                        (Decode.field "to" Decode.string)
                    )
                    |> Decode.map Dict.fromList
                )
            )
            |> Decode.map (Maybe.withDefault Dict.empty)
        )



-- Initialization


initialLocation : Location
initialLocation =
    -- London
    { elevation = 25, lat = 51.50853, lng = -0.12574 }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { weather = Loading
      , annotations =
            -- We hardcode the initial annotations to make the first load fast
            -- since we have a hardcoded initial location
            Success
                [ { end = Time.millisToPosix 1644969600000
                  , name = "Storm Eunice"
                  , start = Time.millisToPosix 1644796800000
                  , wikipedia = "https://en.wikipedia.org/wiki/Storm_Eunice"
                  , description = "Storm Eunice was an intense extratropical cyclone that was part of the 2021–2022 European windstorm season."
                  }
                , { end = Time.millisToPosix 1658275200000
                  , name = "2022 United Kingdom heat wave"
                  , start = Time.millisToPosix 1657238400000
                  , wikipedia = "https://en.wikipedia.org/wiki/2022_United_Kingdom_heat_waves"
                  , description = "The 2022 United Kingdom heatwaves were part of several heatwaves across Europe and North Africa."
                  }
                , { end = Time.millisToPosix 1664582400000
                  , name = "Hurricane Ian"
                  , start = Time.millisToPosix 1663891200000
                  , wikipedia = "https://en.wikipedia.org/wiki/Hurricane_Ian"
                  , description = "Hurricane Ian was a deadly and extremely destructive Category 5 Atlantic hurricane, which was the third-costliest weather disaster on record, the deadliest hurricane to strike the state of Florida since the 1935 Labor Day hurricane, and the strongest hurricane to make landfall in Florida since Michael in 2018."
                  }
                ]
      , name = "London"
      , location = initialLocation
      , geocodingResults = Success []
      , editing = False
      , nameInput = ""
      }
    , getWeather initialLocation
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotWeather d ->
            ( { model | weather = loadingFromResult d }, Cmd.none )

        GotGeocoding g ->
            ( { model | geocodingResults = loadingFromResult g }, Cmd.none )

        GotAnnotations d ->
            ( { model | annotations = loadingFromResult d }, Cmd.none )

        EditedInput str ->
            ( { model | nameInput = str }, Task.perform (always (FetchGeocoding str)) (Process.sleep 1000) )

        Editing editing ->
            ( { model | editing = editing }, Cmd.none )

        FetchGeocoding str ->
            if String.length str > 2 && str == model.nameInput then
                ( model, getGeocoding str )

            else
                ( model, Cmd.none )

        SelectedLocation name country loc ->
            ( { model | name = name, location = loc, weather = Loading, annotations = Loading, nameInput = "", geocodingResults = Success [], editing = False }
            , Cmd.batch [ getWeather loc, getAnnotations country ]
            )


loadingFromResult : Result Http.Error a -> Loading a
loadingFromResult res =
    case res of
        Ok a ->
            Success a

        Err e ->
            Error e


filterAnnotations : WeatherData -> List RawAnnotation -> List Annotation
filterAnnotations weather annotations =
    let
        getPeaks phenomenon fn series =
            Statistics.peaks (Tuple.second >> fn)
                { lookaround = 2, coallesce = 3, sensitivity = 1.4 }
                (List.map2 Tuple.pair
                    weather.timestamps
                    series
                )
                |> List.map (\( t, prec ) -> ( Time.posixToMillis t, phenomenon, prec ))

        peaks =
            getPeaks Precipitation identity weather.precipitationSum
                ++ getPeaks Temperature identity weather.temperatureMax
                ++ getPeaks Temperature negate weather.temperatureMin

        containsPeak candidates anno =
            case candidates of
                [] ->
                    []

                ( x, phenomenon, anchor ) :: xs ->
                    if Time.posixToMillis anno.start < x && Time.posixToMillis anno.end > x && not (phenomenon == Temperature && anchor > 5 && anchor < 12) then
                        [ { name = anno.name, start = anno.start, end = anno.end, wikipedia = anno.wikipedia, phenomenon = phenomenon, anchor = anchor, description = anno.description } ]

                    else
                        containsPeak xs anno
    in
    annotations
        |> List.sortBy (.start >> Time.posixToMillis)
        |> List.concatMap (containsPeak peaks)
        |> List.take 4


view : Model -> Html Msg
view model =
    Html.main_ [ class "main" ]
        [ Html.node "style" [] [ Html.text css ]
        , case model.weather of
            Loading ->
                Example.loading []

            Error e ->
                Example.error Nothing e

            Success data ->
                Html.Lazy.lazy3 viewRadial
                    model.name
                    data
                    (case model.annotations of
                        Success anno ->
                            anno

                        _ ->
                            []
                    )
        , viewSidebar model
        , viewLegend
        , viewAnnotationsControl model.annotations
        ]


temperatureScale : ContinuousScale Float
temperatureScale =
    Scale.linear ( 0, radius ) ( -40, 50 )


colorScale : SequentialScale Color
colorScale =
    Scale.sequential
        (\v ->
            -- if v > 0.4 && v < 0.6 then
            Scale.Color.spectralInterpolator (1 - v)
                |> Color.toHsla
                |> (\r -> { r | lightness = r.lightness * 0.7 })
                |> Color.fromHsla
         -- else
         -- Scale.Color.spectralInterpolator (1 - v)
        )
        ( -20, 40 )
        |> Scale.clamp


precipitationScale : ContinuousScale Float
precipitationScale =
    Scale.radial ( 0, 100 ) ( 0, 300 )


makeTimeScale : WeatherData -> ContinuousScale Time.Posix
makeTimeScale data =
    Statistics.extentBy Time.posixToMillis data.timestamps
        |> Maybe.withDefault ( Time.millisToPosix 0, Time.millisToPosix 0 )
        |> Scale.time data.timeZone ( -pi / 2, 1.5 * pi )


polarToCartesian : Float -> Float -> { x : Float, y : Float }
polarToCartesian angle radius_ =
    { x = radius + radius_ * cos angle, y = radius + radius_ * sin angle }


viewRadial : String -> WeatherData -> List RawAnnotation -> Html msg
viewRadial name weather rawAnnotations =
    let
        timeScale =
            makeTimeScale weather

        axisColor =
            Color.gray

        annotations =
            filterAnnotations weather rawAnnotations

        ( offsetX, positionedAnnotations ) =
            positionAnnotations
                (List.map
                    (\a ->
                        { annotation = a
                        , initial =
                            polarToCartesian
                                (Scale.convert timeScale (Time.millisToPosix (Time.posixToMillis a.start + (Time.posixToMillis a.end - Time.posixToMillis a.start) // 2)))
                                radius
                        }
                    )
                    annotations
                )

        ticks =
            Scale.ticks temperatureScale 9
                |> List.drop 2
                |> List.take 7
    in
    TypedSvg.svg [ width w, height h, viewBox 0 0 w h, fontSize 10, fontFamily [ "DIN Alternate", "-apple-system-short-caption1 ", "-apple-system", "BlinkMacSystemFont", "sans-serif" ] ]
        [ g [ transform [ Translate offsetX 0 ] ]
            [ ticks
                |> List.map
                    (\t ->
                        TypedSvg.circle [ r (Scale.convert temperatureScale t), cx radius, cy radius, stroke (Paint axisColor), fill PaintNone ] []
                    )
                |> g []
            , g []
                [ TypedSvg.line [ y1 (radius - 50), x1 radius, y2 5, x2 radius, stroke (Paint Color.black) ] []
                , TypedSvg.text_
                    [ y 5
                    , x radius
                    , dy (em 0.67)
                    , dx 5
                    , fill (Paint Color.black)
                    ]
                    [ text "2022" ]
                ]
            , Scale.ticks timeScale 12
                |> List.concatMap
                    (\t ->
                        if Scale.tickFormat timeScale 12 t == "2022" then
                            []

                        else
                            let
                                coords =
                                    polarToCartesian (Scale.convert timeScale t) radius

                                offset =
                                    polarToCartesian (Scale.convert timeScale t) (radius - 20)
                            in
                            [ TypedSvg.line [ x1 radius, y1 radius, x2 coords.x, y2 coords.y, stroke (Paint Color.white), strokeWidth 5 ] []
                            , TypedSvg.line [ x1 offset.x, y1 offset.y, x2 coords.x, y2 coords.y, stroke (Paint axisColor) ] []
                            , TypedSvg.text_
                                [ x coords.x
                                , y coords.y
                                , transform [ Rotate ((Scale.convert timeScale t + pi / 2) / degrees 1) coords.x coords.y ]
                                , dy (em 0.67)
                                , dx 5
                                , fill (Paint axisColor)
                                ]
                                [ text (String.toUpper (String.left 3 (Scale.tickFormat timeScale 12 t))) ]
                            ]
                    )
                |> g []
            , ticks
                |> List.indexedMap
                    (\i t ->
                        if modBy 2 i == 0 then
                            g []
                                [ TypedSvg.rect
                                    [ y (radius + Scale.convert temperatureScale t - 6)
                                    , x (radius - 20)
                                    , width 40
                                    , height 12
                                    , fill (Paint Color.white)
                                    ]
                                    []
                                , TypedSvg.text_
                                    [ y (radius + Scale.convert temperatureScale t)
                                    , x radius
                                    , dy (em 0.32)
                                    , textAnchor AnchorMiddle
                                    , fill (Paint axisColor)
                                    ]
                                    [ text (Scale.tickFormat temperatureScale 8 t ++ "℃") ]
                                , TypedSvg.rect
                                    [ y (radius - Scale.convert temperatureScale t - 6)
                                    , x (radius - 20)
                                    , width 40
                                    , height 12
                                    , fill (Paint Color.white)
                                    ]
                                    []
                                , TypedSvg.text_
                                    [ y (radius - Scale.convert temperatureScale t)
                                    , x radius
                                    , dy (em 0.32)
                                    , textAnchor AnchorMiddle
                                    , fill (Paint axisColor)
                                    ]
                                    [ text (Scale.tickFormat temperatureScale 8 t ++ "℃") ]
                                ]

                        else
                            text ""
                    )
                |> g []
            , g []
                (List.map3
                    (\time precipitation avg ->
                        let
                            coords =
                                polarToCartesian (Scale.convert timeScale time) (Scale.convert temperatureScale avg)
                        in
                        TypedSvg.circle
                            [ cx coords.x
                            , cy coords.y
                            , r (Scale.convert precipitationScale precipitation)
                            , fill (Paint (Color.rgba 0.5 0.6 0.95 0.2))
                            ]
                            []
                    )
                    weather.timestamps
                    weather.precipitationSum
                    weather.temperatureMean
                )
            , g []
                (List.map4
                    (\time min avg max ->
                        let
                            top =
                                polarToCartesian (Scale.convert timeScale time) (Scale.convert temperatureScale max)

                            bottom =
                                polarToCartesian (Scale.convert timeScale time) (Scale.convert temperatureScale min)
                        in
                        TypedSvg.line
                            [ x1 bottom.x
                            , y1 bottom.y
                            , x2 top.x
                            , y2 top.y
                            , strokeWidth 2
                            , stroke (Paint (Scale.convert colorScale avg))
                            ]
                            []
                    )
                    weather.timestamps
                    weather.temperatureMin
                    weather.temperatureMean
                    weather.temperatureMax
                )
            , TypedSvg.text_
                [ x radius
                , y radius
                , dy (em 0.32)
                , textAnchor AnchorMiddle
                , fill (Paint (Color.rgb 0.3 0.3 0.3))
                , fontSize 40
                , fontFamily [ "DIN Condensed", "-apple-system-short-caption1 ", "-apple-system", "BlinkMacSystemFont", "sans-serif" ]
                ]
                [ text (String.toUpper name) ]
            ]
        , g []
            (positionedAnnotations
                |> List.map
                    (\pos ->
                        let
                            midpoint =
                                Time.millisToPosix (Time.posixToMillis pos.annotation.start + (Time.posixToMillis pos.annotation.end - Time.posixToMillis pos.annotation.start) // 2)

                            radius_ =
                                Scale.convert temperatureScale pos.annotation.anchor + 5

                            start =
                                polarToCartesian (Scale.convert timeScale pos.annotation.start) radius_

                            end =
                                polarToCartesian (Scale.convert timeScale pos.annotation.end) radius_

                            coords =
                                polarToCartesian (Scale.convert timeScale midpoint) radius_

                            outerCoords =
                                polarToCartesian (Scale.convert timeScale midpoint) (max (Scale.convert temperatureScale pos.annotation.anchor + 20) (radius - 40))

                            color =
                                case pos.annotation.phenomenon of
                                    Temperature ->
                                        Scale.convert colorScale pos.annotation.anchor

                                    Precipitation ->
                                        Color.rgba 0.5 0.6 0.95 1
                        in
                        g []
                            [ Path.element
                                [ SubPath.with (C.MoveTo ( offsetX + start.x, start.y ))
                                    [ C.arcTo
                                        [ { radii = ( radius_, radius_ )
                                          , xAxisRotate = 0
                                          , arcFlag = C.smallestArc
                                          , direction = C.counterClockwise
                                          , target = ( offsetX + end.x, end.y )
                                          }
                                        ]
                                    ]
                                , SubPath.with (C.MoveTo ( offsetX + coords.x, coords.y ))
                                    [ C.lineTo [ ( offsetX + outerCoords.x, outerCoords.y ), ( pos.x, pos.y ) ]
                                    ]
                                ]
                                [ strokeWidth 3
                                , strokeDasharray "1,6"
                                , strokeLinecap StrokeLinecapRound
                                , stroke (Paint color)
                                , fill PaintNone
                                ]
                            , TypedSvg.Core.foreignObject
                                [ x
                                    (if pos.x < offsetX + radius then
                                        pos.x - 205

                                     else
                                        pos.x + 5
                                    )
                                , y (pos.y - 16)
                                , width 200
                                , height 100
                                , A.style "background" "white"
                                , if pos.x < offsetX + radius then
                                    A.style "text-align" "right"

                                  else
                                    A.style "text-align" "left"
                                ]
                                [ Html.div [ class "annotation" ]
                                    [ Html.h3 []
                                        [ Html.a [ A.href pos.annotation.wikipedia, A.style "color" (Color.toCssString color) ]
                                            [ Html.text pos.annotation.name
                                            ]
                                        ]
                                    , Html.p [] [ Html.text pos.annotation.description ]
                                    ]
                                ]
                            ]
                    )
            )
        ]


viewLegend : Html msg
viewLegend =
    Html.div [ class "legend" ]
        [ Html.h2 [] [ Html.text "About" ]
        , Html.p [] [ Html.text "This visualization consists of ", Html.strong [] [ Html.text "365 lines" ], Html.text ", one for each day of the year. The first is 1st of January at the top, days continue ", Html.strong [] [ Html.text "clockwise" ] ]
        , Html.p [] [ Html.strong [] [ Html.text "Precipitation" ], Html.text " (rain or snow) is represented as blue circle (more rain = bigger circle) and placed on the center of the day’s temperature line." ]
        , Html.p [] [ Html.text "The closer a temperature line is positioned to the center of a circle, the colder the minimum temperature of the day. The further out, the warmer the daily maximum temperature. The color represents the daily mean temperature." ]
        ]


positionAnnotations : List { annotation : Annotation, initial : { x : Float, y : Float } } -> ( Float, List { x : Float, y : Float, annotation : Annotation } )
positionAnnotations annotations =
    let
        defaultMain =
            { x = w / 2
            , y = h / 2
            , vx = 0
            , vy = 0
            , id = "main"
            , value = Nothing
            }

        legend =
            { x = w
            , y = h
            , vx = 0
            , vy = 0
            , id = "legend"
            , value = Nothing
            }

        offset =
            w / 2 - radius

        initialEntities =
            defaultMain
                :: legend
                :: List.concat
                    (List.indexedMap
                        (\i a ->
                            [ { x = a.initial.x + offset
                              , y = a.initial.y
                              , vx = 0
                              , vy = 0
                              , id = "anno-" ++ String.fromInt i
                              , value = Just a.annotation
                              }
                            ]
                        )
                        annotations
                    )

        state =
            Force.simulation
                [ Force.customCollision { iterations = 1, strength = 0.5 } (( "main", radius ) :: ( "legend", 300 ) :: List.indexedMap (\i _ -> ( "anno-" ++ String.fromInt i, 120 )) annotations)
                , Force.towardsX [ { node = "main", target = radius, strength = 0.01 } ]
                ]

        go i st ents =
            if i > 0 then
                let
                    ( newState, newEnts ) =
                        Force.tick st ents
                in
                go (i - 1)
                    newState
                    (List.map
                        (\newEnt ->
                            if newEnt.id == "main" then
                                { newEnt | y = h / 2, vy = 0 }

                            else if newEnt.id == "legend" then
                                legend

                            else
                                { newEnt
                                    | vy =
                                        if newEnt.y > h then
                                            -100

                                        else if newEnt.y < 0 then
                                            100

                                        else
                                            newEnt.vy
                                    , y = clamp 0 h newEnt.y
                                    , x = clamp 110 (w - 110) newEnt.x
                                }
                        )
                        newEnts
                    )

            else
                let
                    mainEnt =
                        List.filter (\a -> a.id == "main") ents
                            |> List.head
                            |> Maybe.withDefault defaultMain
                in
                ( mainEnt.x - radius
                , List.filterMap
                    (\a ->
                        Maybe.map
                            (\v ->
                                let
                                    xD =
                                        mainEnt.x - a.x

                                    yD =
                                        mainEnt.y - a.y

                                    largestComponent =
                                        max xD yD

                                    scaledX =
                                        xD / largestComponent

                                    scaledY =
                                        yD / largestComponent

                                    scaledLength =
                                        sqrt (scaledX * scaledX + scaledY * scaledY)
                                in
                                { x = a.x + 100 * (scaledX / scaledLength), y = a.y + 100 * (scaledY / scaledLength), annotation = v }
                            )
                            a.value
                    )
                    ents
                )
    in
    go 30 state initialEntities


viewSidebar : Model -> Html Msg
viewSidebar model =
    if model.editing then
        Html.div [ class "form" ]
            [ Html.h1 [] [ Html.text "Choose a city" ]
            , Html.button [ Html.Events.onClick (Editing False), class "close-button" ] [ Html.text "Close" ]
            , Html.input
                [ A.type_ "text"
                , A.placeholder "Type a city..."
                , A.value model.nameInput
                , Html.Events.onInput EditedInput
                ]
                []
            , case model.geocodingResults of
                Loading ->
                    Example.loading []

                Error e ->
                    Example.error Nothing e

                Success data ->
                    Html.ul []
                        (List.map
                            (\gc ->
                                Html.li []
                                    [ Html.button [ Html.Events.onClick (SelectedLocation gc.name gc.country gc.location) ]
                                        [ Html.img
                                            [ A.src ("https://hatscripts.github.io/circle-flags/flags/" ++ String.toLower gc.country ++ ".svg")
                                            , A.title gc.country
                                            , A.width 14
                                            ]
                                            []
                                        , Html.strong [] [ Html.text gc.name ]
                                        , Html.span [] [ Html.text gc.admin1 ]
                                        ]
                                    ]
                            )
                            data
                        )
            ]

    else
        Html.button [ Html.Events.onClick (Editing True), class "edit-button" ] [ Html.text "Pick a city" ]


viewAnnotationsControl : Loading (List RawAnnotation) -> Html Msg
viewAnnotationsControl l =
    Html.div [ class "annotations-control" ]
        [ case l of
            Loading ->
                Html.text "Loading annotations..."

            Error _ ->
                Html.text "Failed to load annotations"

            Success [] ->
                Html.text "Annotations not available for this location"

            Success _ ->
                Html.text "✔︎"
        ]


css : String
css =
    """
body {
    font-family: "DIN Alternate", -apple-system-short-caption1, -apple-system, "BlinkMacSystemFont", sans-serif;
}

.main {
    width: """ ++ String.fromFloat w ++ """px;
    height: """ ++ String.fromFloat h ++ """px;
    overflow: hidden;
    position: relative;
}

.edit-button {
    position: absolute;
    top: 0;
    right: 0;
}

.close-button {
    position: absolute;
    top: 0;
    right: 0;
}

.form {
    width: 300px;
    position: absolute;
    top: 0;
    right: 0;
    bottom: 0;
    background: rgba(255, 255, 255,0.6);
    z-index: 2;
    backdrop-filter: blur(1px);
}

.form input {
    width: 95%;
}

.form ul {
    list-style: none;
    margin: 0;
    padding: 0;
}

.form li {
    margin-bottom: 5px;
    margin-top: 5px;
}

.form li button {
    width: 100%;
    cursor: pointer;
    width: 100%;
    background: none;
    border: none;
    display: flex;
    gap: 4px;
}

.form li button:hover {
    background: rgba(200, 200, 200, 0.4);
}

.legend {
    font-size: 10px;
    position: absolute;
    right: 0;
    bottom: 0;
    width: 200px;
    color: gray;
}
.legend strong {
    color: black;
}
.annotation h3 {
    font-size: 16px;
    margin: 0;
}
.annotation h3 a {
    text-decoration: none;
}

.annotations-control {
    position: absolute;
    top: 0;
    left: 0;
    font-size: 9px;
    color: gray;
}
"""
