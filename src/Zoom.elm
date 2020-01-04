module Zoom exposing
    ( Zoom, init, scaleExtent, translateExtent
    , OnZoom, update, subscriptions
    , transform, asRecord
    , events, onDoubleClick, onWheel, onDrag, onGesture, onTouch
    )

{-| This module implements a convenient abstraction for panning and zooming:
it lets the user focus on a regions of interest in a visualization.
It is quite intuitive in that dragging the mouse coresponds to panning,
mouse wheel zooming, and touch interactions are also supported.

The implementation is agnostic about the DOM, so it can be used with HTML, SVG, or WebGL.


## Setting up zooming in your app

While there are many ways to use this module, a typical setup will look like this:

    import Zoom exposing (OnZoom, Zoom)

    type alias Model =
        { zoom : Zoom
        }

    type Msg
        = ZoomMsg OnZoom

    -- ...

Next, initialize and configure the zoom model:

    init : () -> ( Model, Cmd Msg )
    init () =
        ( { zoom = Zoom.init { width = width, height = height } }
        , Cmd.none
        )

Note that you will need to provide the width and height of the element that you want to setup the zooming behavior for. If you don't know this information, then you might want to use [`Browser.Dom.getElement`](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Dom#getElement) to get it.

Next setup a subscription:

    subscriptions : Model -> Sub Msg
    subscriptions model =
        Zoom.subscriptions model.zoom ZoomMsg

Then handle update:

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            ZoomMsg zoomMsg ->
                ( { model
                    | zoom = Zoom.update zoomMsg zoom.model
                  }
                , Cmd.none
                )

Finally, set up your view:

    view : Model -> Html Msg
    view model =
        svg
            (A.width width
                :: A.height height
                :: Zoom.tranfrom model.zoom
                :: Zoom.events model.zoom ZoomMsg
            )
            [ myChildrenElements ]


## Configuring the zoom behavior

@docs Zoom, init, scaleExtent, translateExtent


## Updating the zoom

@docs OnZoom, update, subscriptions


## View

@docs transform, asRecord


## Events

@docs events, onDoubleClick, onWheel, onDrag, onGesture, onTouch

-}

import Browser.Events
import Html.Attributes exposing (style)
import Json.Decode as D exposing (Decoder)
import Svg exposing (Attribute, Svg)
import Svg.Attributes as A
import Svg.Events exposing (custom, preventDefaultOn)
import Time
import Zoom.Interpolation
import Zoom.Matrix as Matrix exposing (Matrix2x3)
import Zoom.Transform as Transform exposing (Transform)


{-| This type will go into your model as it stores the internal state and configuration necessary to support the various user interactions.
-}
type Zoom
    = Zoom
        { transform : Transform
        , extent : Extent
        , drag : Maybe { matrix : Maybe Matrix2x3, current : ( Float, Float ) }
        , gestureLastScale : Float
        , touchStarted : Bool
        , touches : TouchState
        , translateExtent : Extent
        , scaleExtent : ( Float, Float )
        , transition : Maybe { duration : Float, elapsed : Float, interpolation : Float -> Transform }
        }


type TouchState
    = NoTouches
    | OneFinger TrackedTouch
    | TwoFingers TrackedTouch TrackedTouch


{-| Returns the actual transform for the view relative to the top left corner. You can then use these numbers to transform the view as necessary.
-}
asRecord : Zoom -> { scale : Float, translate : { x : Float, y : Float } }
asRecord (Zoom zoom) =
    { scale = zoom.transform.k, translate = { x = zoom.transform.x, y = zoom.transform.y } }


type alias Rect =
    { x : Float, y : Float, width : Float, height : Float }


{-| This is the Msg type used. You will want to pass these to `Zoom.update`.

Note that when handling these messages, it is also extremely likely that the zoom transform has somehow changed,
so you can also use that place in your update function to react to that. For example in a map application, you may
want to fetch a different tile based on the zoom level:

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            Zoomed onZoom ->
                let
                    oldTransform =
                        Zoom.asRecord model.zoom

                    newZoom =
                        Zoom.update onZoom model.zoom

                    newTransform =
                        Zoom.asRecord newZoom

                    cmd =
                        if toTileCoords oldTransform /= toTileCoords newTransform then
                            fetchTile (toTileCoords newTransform)

                        else
                            Cmd.none
                in
                ( { model | zoom = newZoom }, cmd )

-}
type OnZoom
    = DoubleClicked Bool ( Float, Float )
    | MouseDown ( Float, Float ) (Maybe Matrix2x3)
    | MouseMove ( Float, Float )
    | MouseUp
    | Wheeled Float ( Float, Float )
    | GestureStarted
    | Gestured Float ( Float, Float )
    | TouchStarted (List Touch)
    | TouchStartedExpired
    | TouchMoved (List Touch)
    | TouchEnded (List Touch)
    | Tick Float


type alias Touch =
    { position : ( Float, Float ), identifier : Int }


type alias TrackedTouch =
    { position : ( Float, Float ), previous : ( Float, Float ), identifier : Int }


touchDelay : Float
touchDelay =
    500


infinity =
    1 / 0


{-| Creates a brand new `Zoom`. You have to pass in the dimensions (in pixels) of the element that you want to make zoom-eable.
-}
init : { width : Float, height : Float } -> Zoom
init { width, height } =
    Zoom
        { transform = Transform.identity
        , extent = ( ( 0, 0 ), ( width, height ) )
        , drag = Nothing
        , gestureLastScale = 0
        , touchStarted = False
        , touches = NoTouches
        , translateExtent = ( ( -infinity, -infinity ), ( infinity, infinity ) )
        , scaleExtent = ( 0, infinity )
        , transition = Nothing
        }


{-| Allows you to set a boundary where a user will be able to pan to. The format is `((top, left), (bottom, right))`.

Typically you will want to set this to `((0, 0), (width, height))`, however you can restrict it however you like. For example maps typically only restrict vertical movement, but not horizontal movement.

-}
translateExtent : ( ( Float, Float ), ( Float, Float ) ) -> Zoom -> Zoom
translateExtent extent (Zoom zoom) =
    Zoom { zoom | translateExtent = extent }


{-| Allows you to set a minimum and maximum scale that the user will be able to zoom to.

Typically there is only limited resolution to the data, so setting the maximum such that the maximum resolution is comfortably visible (remember accessibility - some people will want to zoom a fair bit more than you might find necessary) would be a good idea.

The miminum zoom will always be asymptotically approaching zero, but setting a higher number is good, because the view can be "lost" if it gets too small. Typically you would set it such that the whole dataset fits into the view.

-}
scaleExtent : Float -> Float -> Zoom -> Zoom
scaleExtent mn mx (Zoom zoom) =
    Zoom { zoom | scaleExtent = ( mn, mx ) }


{-| Sets up the event handlers necessary to support this behavior on various devices.

It is merely a convenience, implemented such:

    events zoom tagger =
        Zoom.onDoubleClick zoom tagger
            :: Zoom.onWheel zoom tagger
            :: Zoom.onDrag zoom tagger
            ++ Zoom.onGesture zoom tagger
            ++ Zoom.onTouch zoom tagger

So if you want to customize the user experience, you can for example omit the onWheel handler in your own defintion.

-}
events : Zoom -> (OnZoom -> msg) -> List (Attribute msg)
events zoom tagger =
    onDoubleClick zoom tagger
        :: onWheel zoom tagger
        :: onDrag zoom tagger
        ++ onGesture zoom tagger
        ++ onTouch zoom tagger


{-| Zooms in on double click, zooms out on double click while holding shift.
-}
onDoubleClick : Zoom -> (OnZoom -> msg) -> Attribute msg
onDoubleClick _ tagger =
    custom "dblclick"
        (D.map2
            (\shiftDown position ->
                { message = tagger (DoubleClicked shiftDown position)
                , stopPropagation = True
                , preventDefault = True
                }
            )
            (D.field "shiftKey" D.bool)
            decodeMousePosition
        )


{-| Zooms on mousewheel.
-}
onWheel : Zoom -> (OnZoom -> msg) -> Attribute msg
onWheel _ tagger =
    custom "wheel"
        (D.map3
            (\deltaY deltaMode position ->
                { message =
                    tagger
                        (Wheeled
                            (-deltaY
                                * (if deltaMode == 0 then
                                    0.002

                                   else if deltaMode == 1 then
                                    0.05

                                   else
                                    1
                                  )
                            )
                            position
                        )
                , stopPropagation = True
                , preventDefault = True
                }
            )
            (D.field "deltaY" D.float)
            (D.field "deltaMode" D.int)
            decodeMousePosition
        )


{-| Allows panning on mouse drag.
-}
onDrag : Zoom -> (OnZoom -> msg) -> List (Attribute msg)
onDrag (Zoom { drag }) tagger =
    case drag of
        Nothing ->
            [ custom "mousedown"
                (D.map3
                    (\x y matrix ->
                        { message = tagger (MouseDown ( x, y ) matrix)
                        , stopPropagation = True
                        , preventDefault = False
                        }
                    )
                    (D.field "offsetX" D.float)
                    (D.field "offsetY" D.float)
                    decodeSVGTransformMatrix
                )
            ]

        Just _ ->
            []


{-| Supports pinch to zoom on desktop Safari.
-}
onGesture : Zoom -> (OnZoom -> msg) -> List (Attribute msg)
onGesture _ tagger =
    [ custom "gesturestart"
        (D.succeed
            { message = tagger GestureStarted
            , stopPropagation = True
            , preventDefault = True
            }
        )
    , custom "gesturechange"
        (D.map2
            (\scl position ->
                { message =
                    tagger (Gestured scl position)
                , stopPropagation = True
                , preventDefault = True
                }
            )
            (D.field "scale" D.float)
            decodeMousePosition
        )
    ]


{-| Supports pinch to zoom on mobile devices.
-}
onTouch : Zoom -> (OnZoom -> msg) -> List (Attribute msg)
onTouch (Zoom zoom) tagger =
    [ custom "touchstart"
        (D.map
            (\touches ->
                case ( zoom.touchStarted, touches, zoom.touches ) of
                    ( True, [ touch0 ], NoTouches ) ->
                        { message = tagger (DoubleClicked False touch0.position)
                        , stopPropagation = True
                        , preventDefault = True
                        }

                    _ ->
                        { message = tagger (TouchStarted touches)
                        , stopPropagation = True
                        , preventDefault = False
                        }
            )
            decodeTouches
        )
    , custom "touchmove"
        (D.map
            (\touches ->
                { message = tagger (TouchMoved touches)
                , stopPropagation = True
                , preventDefault = True
                }
            )
            decodeTouches
        )
    , custom "touchend"
        (D.map
            (\touches ->
                { message = tagger (TouchEnded touches)
                , stopPropagation = True
                , preventDefault = False
                }
            )
            decodeTouches
        )
    , custom "touchcancel"
        (D.map
            (\touches ->
                { message = tagger (TouchEnded touches)
                , stopPropagation = True
                , preventDefault = False
                }
            )
            decodeTouches
        )
    , style "touch-action" "none"
    , style "-webkit-tap-highlight-color" "rgba(0,0,0,0)"
    ]


normalizePointerPosition : ( Float, Float ) -> Maybe Matrix2x3 -> ( Float, Float )
normalizePointerPosition position maybeMatrix =
    case maybeMatrix of
        Just matrix ->
            Matrix.transform position matrix

        Nothing ->
            position


decodeMousePosition : Decoder ( Float, Float )
decodeMousePosition =
    D.map3
        (\maybeMatrix x y ->
            normalizePointerPosition ( x, y ) maybeMatrix
        )
        decodeSVGTransformMatrix
        (D.oneOf [ D.field "offsetX" D.float, D.field "clientX" D.float ])
        (D.oneOf [ D.field "offsetY" D.float, D.field "clientY" D.float ])


decodeSVGTransformMatrix : Decoder (Maybe Matrix2x3)
decodeSVGTransformMatrix =
    -- TODO Take into account offsets of viewBox
    -- TODO Check for aspect ratio, currentScale, currentTranslate, etc...
    D.oneOf
        [ D.map3
            (\viewBox width height ->
                Just ( ( viewBox.width / width, 0, 0 ), ( 0, viewBox.height / height, 0 ) )
            )
            (D.at [ "currentTarget", "viewBox", "baseVal" ] decodeRect)
            (D.at [ "currentTarget", "width", "baseVal", "value" ] D.float)
            (D.at [ "currentTarget", "height", "baseVal", "value" ] D.float)
        , D.succeed Nothing
        ]



{- FFS we need this shenigan to decode these bizzaro datastructures -}


listLike : Decoder a -> Decoder (List a)
listLike itemDecoder =
    let
        decodeN n =
            List.range 0 (n - 1)
                |> List.map decodeOne
                |> List.foldr (D.map2 (::)) (D.succeed [])

        decodeOne n =
            D.field (String.fromInt n) itemDecoder
    in
    D.field "length" D.int
        |> D.andThen decodeN


decodeTouches : Decoder (List Touch)
decodeTouches =
    D.andThen
        (\maybeMatrix ->
            D.map3
                (\x y identifier ->
                    { position = normalizePointerPosition ( x, y ) maybeMatrix, identifier = identifier }
                )
                (D.field "clientX" D.float)
                (D.field "clientY" D.float)
                (D.field "identifier" D.int)
                |> listLike
                |> D.field "changedTouches"
        )
        decodeSVGTransformMatrix


decodeRect : Decoder Rect
decodeRect =
    D.map4 Rect
        (D.field "x" D.float)
        (D.field "y" D.float)
        (D.field "width" D.float)
        (D.field "height" D.float)


{-| A convenience for setting up the `tranform` attribute for **SVG** elements.
-}
transform : Zoom -> Attribute msg
transform (Zoom zoom) =
    A.transform (Transform.toString zoom.transform)


{-| Subrscriptions are used for allowing drags to continue outside the element as well for animated zooms on double-click.
-}
subscriptions : Zoom -> (OnZoom -> msg) -> Sub msg
subscriptions (Zoom zoom) tagger =
    Sub.batch
        [ case zoom.drag of
            Just _ ->
                Sub.batch
                    [ Browser.Events.onMouseMove
                        (D.map2
                            (\x y ->
                                tagger (MouseMove ( x, y ))
                            )
                            (D.field "clientX" D.float)
                            (D.field "clientY" D.float)
                        )
                    , Browser.Events.onMouseUp (D.succeed (tagger MouseUp))
                    ]

            Nothing ->
                Sub.none
        , if zoom.touchStarted then
            Time.every touchDelay (always TouchStartedExpired >> tagger)

          else
            Sub.none
        , Maybe.map (always (Browser.Events.onAnimationFrameDelta (Tick >> tagger))) zoom.transition
            |> Maybe.withDefault Sub.none
        ]


negate : ( Float, Float ) -> ( Float, Float )
negate ( a, b ) =
    ( -a, -b )


schedule : Transform -> ( Float, Float ) -> Zoom -> Zoom
schedule btransform center (Zoom model) =
    let
        ( ( top, left ), ( bottom, right ) ) =
            model.extent

        w =
            max (bottom - top) (right - left)

        atransform =
            model.transform

        ( ax, ay ) =
            Transform.invert center atransform

        ( bx, by ) =
            Transform.invert center btransform

        ( dur, interp ) =
            Zoom.Interpolation.interpolate { cx = ax, cy = ay, size = w / atransform.k } { cx = bx, cy = by, size = w / btransform.k }
    in
    Zoom
        { model
            | transition =
                Just
                    { duration = dur
                    , elapsed = 0
                    , interpolation =
                        \t ->
                            if t == 1 then
                                btransform

                            else
                                let
                                    l =
                                        interp t

                                    k =
                                        w
                                            / l.size
                                in
                                { k = k, x = Tuple.first center - l.cx * k, y = Tuple.second center - l.cy * k }
                    }
        }


{-| This is what you need to set up in your update function.
-}
update : OnZoom -> Zoom -> Zoom
update msg (Zoom model) =
    case msg of
        DoubleClicked shiftKey position0 ->
            let
                position1 =
                    Transform.invert position0 model.transform

                k =
                    model.transform.k
                        * (if shiftKey then
                            0.5

                           else
                            2
                          )

                newTransform =
                    model.transform
                        |> scale model.scaleExtent k
                        |> translate position0 position1
                        |> constrain model.extent model.translateExtent
            in
            Zoom
                { model
                    | touchStarted = False
                }
                |> schedule newTransform position0

        MouseDown position matrix ->
            Zoom
                { model
                    | drag = Just { matrix = matrix, current = Transform.invert (normalizePointerPosition position matrix) model.transform }
                    , transition = Nothing
                }

        MouseMove position_ ->
            case model.drag of
                Just drag ->
                    let
                        position =
                            normalizePointerPosition position_ drag.matrix

                        trasform_ =
                            translate position drag.current model.transform
                    in
                    Zoom
                        { model
                            | drag = Just { drag | current = Transform.invert position trasform_ }
                            , transform = trasform_ |> constrain model.extent model.translateExtent
                            , transition = Nothing
                        }

                Nothing ->
                    Zoom model

        MouseUp ->
            Zoom { model | drag = Nothing }

        Wheeled delta position0 ->
            let
                position1 =
                    Transform.invert position0 model.transform

                k =
                    model.transform.k * (2 ^ delta)
            in
            Zoom
                { model
                    | transform =
                        model.transform
                            |> scale model.scaleExtent k
                            |> translate position0 position1
                            |> constrain model.extent model.translateExtent
                    , transition = Nothing
                }

        GestureStarted ->
            Zoom { model | gestureLastScale = 1 }

        Gestured scl position0 ->
            let
                position1 =
                    Transform.invert position0 model.transform
            in
            Zoom
                { model
                    | transform =
                        model.transform
                            |> Transform.scale (scl / model.gestureLastScale)
                            |> translate position0 position1
                            |> constrain model.extent model.translateExtent
                    , gestureLastScale = scl
                    , transition = Nothing
                }

        TouchStartedExpired ->
            Zoom { model | touchStarted = False }

        TouchStarted touches ->
            Zoom
                { model
                    | touches =
                        case ( model.touches, touches ) of
                            ( NoTouches, [ single ] ) ->
                                OneFinger { position = single.position, previous = Transform.invert single.position model.transform, identifier = single.identifier }

                            ( NoTouches, one :: two :: _ ) ->
                                TwoFingers { position = one.position, previous = Transform.invert one.position model.transform, identifier = one.identifier }
                                    { position = two.position, previous = Transform.invert two.position model.transform, identifier = two.identifier }

                            ( OneFinger id1, one :: _ ) ->
                                TwoFingers id1 { position = one.position, previous = Transform.invert one.position model.transform, identifier = one.identifier }

                            _ ->
                                model.touches
                    , touchStarted = True
                    , transition = Nothing
                }

        TouchMoved touches ->
            Zoom <|
                case model.touches of
                    OneFinger before ->
                        case findAssociatedTouch before touches of
                            Just after ->
                                { model
                                    | touchStarted = False
                                    , transform =
                                        translate after.position before.previous model.transform
                                            |> constrain model.extent model.translateExtent
                                    , touches = OneFinger { before | position = after.position }
                                    , transition = Nothing
                                }

                            Nothing ->
                                model

                    TwoFingers beforeTouch1 beforeTouch2 ->
                        let
                            after1 =
                                findAssociatedTouch beforeTouch1 touches
                                    |> Maybe.map .position
                                    |> Maybe.withDefault beforeTouch1.position

                            after2 =
                                findAssociatedTouch beforeTouch2 touches
                                    |> Maybe.map .position
                                    |> Maybe.withDefault beforeTouch2.position

                            before1 =
                                beforeTouch1.previous

                            before2 =
                                beforeTouch2.previous

                            distBefore =
                                dist2 before1 before2

                            distAfter =
                                dist2 after1 after2

                            t =
                                model.transform
                        in
                        { model
                            | touchStarted = False
                            , transform =
                                t
                                    |> scale model.scaleExtent (sqrt (distAfter / distBefore))
                                    |> translate (midpoint after1 after2) (midpoint before1 before2)
                                    |> constrain model.extent model.translateExtent
                            , touches = TwoFingers { beforeTouch1 | position = after1 } { beforeTouch2 | position = after2 }
                            , transition = Nothing
                        }

                    NoTouches ->
                        model

        TouchEnded touches ->
            Zoom
                { model
                    | touches =
                        case model.touches of
                            NoTouches ->
                                NoTouches

                            OneFinger touch1 ->
                                case findAssociatedTouch touch1 touches of
                                    Just _ ->
                                        NoTouches

                                    Nothing ->
                                        model.touches

                            TwoFingers before1 before2 ->
                                case ( findAssociatedTouch before1 touches, findAssociatedTouch before2 touches ) of
                                    ( Just _, Just _ ) ->
                                        NoTouches

                                    ( Just _, Nothing ) ->
                                        OneFinger before2

                                    ( Nothing, Just _ ) ->
                                        OneFinger before1

                                    ( Nothing, Nothing ) ->
                                        model.touches
                }

        Tick delta ->
            case model.transition of
                Just ({ duration, elapsed, interpolation } as transition) ->
                    Zoom
                        { model
                            | transform = interpolation (easingInOutCubic (clamp 0 1 ((elapsed + delta) / duration)))
                            , transition =
                                if elapsed + delta >= duration then
                                    Nothing

                                else
                                    Just { transition | elapsed = elapsed + delta }
                        }

                Nothing ->
                    Zoom model


dist2 : ( Float, Float ) -> ( Float, Float ) -> Float
dist2 ( x1, y1 ) ( x2, y2 ) =
    (x2 - x1) ^ 2 + (y2 - y1) ^ 2


midpoint : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
midpoint ( x1, y1 ) ( x2, y2 ) =
    ( (x1 + x2) / 2, (y1 + y2) / 2 )


findAssociatedTouch : { a | identifier : Int } -> List { b | identifier : Int } -> Maybe { b | identifier : Int }
findAssociatedTouch { identifier } =
    List.filter (\t -> t.identifier == identifier) >> List.head


scale : ( Float, Float ) -> Float -> Transform -> Transform
scale ( mi, mx ) k trfm =
    { trfm | k = clamp mi mx k }


translate : ( Float, Float ) -> ( Float, Float ) -> Transform -> Transform
translate ( position0x, position0y ) ( position1x, position1y ) trfm =
    { trfm | x = position0x - position1x * trfm.k, y = position0y - position1y * trfm.k }


type alias Extent =
    ( ( Float, Float ), ( Float, Float ) )


constrain : Extent -> Extent -> Transform -> Transform
constrain ( extentT, extentB ) ( translateExtentT, translateExtentB ) tsfm =
    let
        minus ( a, b ) ( c, d ) =
            ( c - a, d - b )

        ( dx0, dy0 ) =
            Transform.invert extentT tsfm |> minus translateExtentT

        ( dx1, dy1 ) =
            Transform.invert extentB tsfm |> minus translateExtentB

        ( minDX, minDY ) =
            ( min 0 dx0, min 0 dy0 )
    in
    Transform.translate
        ( if dx1 > dx0 then
            (dx0 + dx1) / 2

          else if minDX == 0 then
            max 0 dx1

          else
            minDX
        , if dy1 > dy0 then
            (dy0 + dy1) / 2

          else if minDY == 0 then
            max 0 dy1

          else
            minDY
        )
        tsfm


easingInOutCubic t =
    if t < 0.5 then
        ((t * 2) ^ 3) / 2

    else
        1 - 0.5 * (-2 * t + 2) ^ 3
