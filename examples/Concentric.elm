module Concentric exposing (main)

{-| Having fun with arcs and animations.

@category Art

-}

import Browser
import Browser.Events
import Color exposing (Color)
import Path
import Random
import Scale exposing (defaultBandConfig)
import Scale.Color
import Shape exposing (defaultPieConfig)
import TypedSvg exposing (g, rect, svg)
import TypedSvg.Attributes exposing (fill, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (height, width)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (Paint(..), Transform(..))


type alias Model =
    { data : List (List Float), frame : Int }


type Msg
    = Generated (List (List Float))
    | Tick Int


w : Float
w =
    900


h : Float
h =
    450


arcWidth : Float
arcWidth =
    20


radius : Float
radius =
    (max w h / 2) + 50


scale =
    Scale.band { defaultBandConfig | paddingInner = 0.3 } ( arcWidth, radius ) (List.range 0 (floor (radius / arcWidth)))


colorScale =
    Scale.sequential Scale.Color.plasmaInterpolator ( 0, 1 )


getColor : Int -> Int -> Color
getColor ring value =
    let
        seed =
            Random.initialSeed ((ring + 1) * (value + 1))

        gen =
            Random.map (Scale.convert colorScale) (Random.float 0.2 0.7)

        ( val, _ ) =
            Random.step gen seed
    in
    val


tau =
    pi * 2


view : Model -> Svg msg
view model =
    svg [ viewBox 0 0 w h ]
        [ rect [ width w, height h, fill (Paint (Color.rgb255 50 50 50)) ] []
        , model.data
            |> List.indexedMap
                (\index data ->
                    if model.frame > index * 100 then
                        data
                            |> Shape.pie
                                { defaultPieConfig
                                    | innerRadius = Scale.convert scale index
                                    , outerRadius = Scale.convert scale index + Scale.bandwidth scale
                                    , padAngle = 0.03
                                    , cornerRadius = arcWidth
                                    , sortingFn = \_ _ -> EQ
                                    , startAngle = toFloat index
                                    , endAngle = toFloat index + tau
                                }
                            |> List.indexedMap
                                (\subIndex datum ->
                                    Path.element (Shape.arc datum)
                                        [ fill (Paint (getColor index subIndex))
                                        , transform [ Rotate (toFloat model.frame / 100 * alternaring index) 0 0 ]
                                        ]
                                )
                            |> g []

                    else
                        text ""
                )
            |> g
                [ transform [ Translate (w / 2) (h / 2) ]
                ]
        ]


alternaring v =
    toFloat ((modBy 2 v * 2) - 1)


dataGenerator =
    Random.list (floor (radius / arcWidth))
        (Random.int 2 10
            |> Random.andThen (\len -> Random.list len (Random.float 0.1 5))
        )


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init () =
    ( { data = []
      , frame = 0
      }
    , Random.generate Generated dataGenerator
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Generated data ->
            ( { model | data = data }, Cmd.none )

        Tick i ->
            ( { model | frame = model.frame + i }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onAnimationFrameDelta (round >> Tick)
