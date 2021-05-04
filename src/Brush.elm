module Brush exposing (..)

import Browser.Events
import Events
import Html exposing (th)
import Json.Decode as D exposing (Decoder)
import Svg exposing (Svg)
import Svg.Attributes as Attr exposing (pointerEvents)
import Svg.Events exposing (custom)
import Zoom.Matrix as Matrix exposing (Matrix2x3)


type OneDimensional
    = OneDimensional Never


type TwoDimensional
    = TwoDimensional Never


type alias Extent =
    { n : Float, s : Float, w : Float, e : Float }


type Brush dimension
    = Brush
        { drag :
            Maybe
                { matrix : Maybe Matrix2x3
                , origin : ( Float, Float )
                , current : ( Float, Float )
                , mode : Mode
                , lock : Lock
                , element : Element
                , initialSelection : Extent
                }
        , keysEnabled : Bool
        , shifting : Bool
        , x : Bool
        , y : Bool
        , selection : Maybe Extent
        , extent : Extent
        }


type Lock
    = NoLock
    | LockX
    | LockY


type Mode
    = Drag
    | Space
    | Handle
    | Center


type OnBrush
    = MouseDown Mode Bool Element ( Float, Float ) (Maybe Matrix2x3)
    | MouseMove ( Float, Float )
    | MouseUp
    | ShiftDown
    | AltDown
    | ShiftUp
    | AltUp


type Element
    = OverlayElement
    | SelectionElement
    | HandleElement Handle


type Handle
    = NHandle
    | WHandle
    | EHandle
    | SHandle
    | NWHandle
    | NEHandle
    | SWHandle
    | SEHandle


events : Element -> Brush dim -> (OnBrush -> msg) -> List (Svg.Attribute msg)
events element (Brush { drag, keysEnabled }) tagger =
    case drag of
        Nothing ->
            [ custom "mousedown"
                (D.map6
                    (\x y matrix meta alt shift ->
                        let
                            el =
                                if keysEnabled && meta then
                                    OverlayElement

                                else
                                    element

                            mode =
                                if el == SelectionElement then
                                    Drag

                                else if keysEnabled && alt then
                                    Center

                                else
                                    Handle
                        in
                        { message = tagger (MouseDown mode (keysEnabled && shift) element ( x, y ) matrix)
                        , stopPropagation = True
                        , preventDefault = False
                        }
                    )
                    (D.field "clientX" D.float)
                    (D.field "clientY" D.float)
                    Events.decodeSVGTransformMatrix
                    (D.field "metaKey" D.bool)
                    (D.field "altKey" D.bool)
                    (D.field "shiftKey" D.bool)
                )
            ]

        Just _ ->
            []


initX : { n : Float, s : Float, w : Float, e : Float } -> Brush OneDimensional
initX =
    init True False


initY : { n : Float, s : Float, w : Float, e : Float } -> Brush OneDimensional
initY =
    init False True


initXY : { n : Float, s : Float, w : Float, e : Float } -> Brush TwoDimensional
initXY =
    init True True


init : Bool -> Bool -> Extent -> Brush dimension
init x y extent =
    Brush { drag = Nothing, keysEnabled = True, shifting = False, x = x, y = y, selection = Nothing, extent = extent }


selection1d : Brush OneDimensional -> Maybe ( Float, Float )
selection1d (Brush { selection, x }) =
    Maybe.map
        (\{ n, w, s, e } ->
            if x then
                ( min e w, max e w )

            else
                ( min n s, max n s )
        )
        selection


selection2d : Brush TwoDimensional -> Maybe { n : Float, s : Float, w : Float, e : Float }
selection2d (Brush { selection }) =
    selection


{-| These encode how different elements behave with regards to different actions.
-}
toXSign : Bool -> Element -> Maybe Float
toXSign enabled element =
    if enabled then
        case element of
            OverlayElement ->
                Just 1

            SelectionElement ->
                Just 1

            HandleElement NHandle ->
                Nothing

            HandleElement WHandle ->
                Just -1

            HandleElement EHandle ->
                Just 1

            HandleElement SHandle ->
                Nothing

            HandleElement NWHandle ->
                Just -1

            HandleElement NEHandle ->
                Just 1

            HandleElement SWHandle ->
                Just -1

            HandleElement SEHandle ->
                Just 1

    else
        Nothing


toYSign : Bool -> Element -> Maybe Float
toYSign enabled element =
    if enabled then
        case element of
            OverlayElement ->
                Just 1

            SelectionElement ->
                Just 1

            HandleElement NHandle ->
                Just -1

            HandleElement WHandle ->
                Nothing

            HandleElement EHandle ->
                Nothing

            HandleElement SHandle ->
                Just 1

            HandleElement NWHandle ->
                Just -1

            HandleElement NEHandle ->
                Just -1

            HandleElement SWHandle ->
                Just 1

            HandleElement SEHandle ->
                Just 1

    else
        Nothing


hasMoved model =
    case model.drag of
        Just drag ->
            let
                position =
                    Events.normalizePointerPosition drag.current drag.matrix

                dx =
                    Tuple.first position - Tuple.first drag.origin

                dy =
                    Tuple.second position - Tuple.second drag.origin

                selection =
                    Maybe.withDefault model.extent model.selection

                { extent } =
                    model

                { initialSelection, element } =
                    drag

                newSelection =
                    case drag.mode of
                        Handle ->
                            let
                                xModSelection =
                                    if toXSign model.x element == Just -1 then
                                        { initialSelection | w = initialSelection.w + clamp (extent.w - initialSelection.w) (extent.e - initialSelection.w) dx }

                                    else if toXSign model.x element == Just 1 then
                                        { initialSelection | e = initialSelection.e + clamp (extent.w - initialSelection.e) (extent.e - initialSelection.e) dx }

                                    else
                                        initialSelection
                            in
                            if toYSign model.y element == Just -1 then
                                { xModSelection | n = initialSelection.n + clamp (extent.n - initialSelection.n) (extent.s - initialSelection.n) dy }

                            else if toYSign model.y element == Just 1 then
                                { xModSelection | s = initialSelection.s + clamp (extent.n - initialSelection.s) (extent.s - initialSelection.s) dy }

                            else
                                xModSelection

                        Center ->
                            let
                                deltaX =
                                    case toXSign model.x element of
                                        Just signX ->
                                            dx * signX

                                        Nothing ->
                                            0

                                deltaY =
                                    case toYSign model.y element of
                                        Just signY ->
                                            dy * signY

                                        Nothing ->
                                            0
                            in
                            { initialSelection
                                | w = clamp extent.w extent.e (initialSelection.w - deltaX)
                                , e = clamp extent.w extent.e (initialSelection.e + deltaX)
                                , n = clamp extent.n extent.s (initialSelection.n - deltaY)
                                , s = clamp extent.n extent.s (initialSelection.s + deltaY)
                            }

                        _ ->
                            let
                                deltaX =
                                    if model.x then
                                        clamp (extent.w - drag.initialSelection.w) (extent.e - drag.initialSelection.e) dx

                                    else
                                        0

                                deltaY =
                                    if model.y then
                                        clamp (extent.n - initialSelection.n) (extent.s - initialSelection.s) dy

                                    else
                                        0
                            in
                            { w = initialSelection.w + deltaX
                            , e = initialSelection.e + deltaX
                            , s = initialSelection.s + deltaY
                            , n = initialSelection.n + deltaY
                            }

                normalize sel =
                    { n = min sel.n sel.s
                    , s = max sel.n sel.s
                    , w = min sel.w sel.e
                    , e = max sel.w sel.e
                    }
            in
            { model
                | selection =
                    Just
                        (case Debug.log "lock" drag.lock of
                            NoLock ->
                                normalize newSelection

                            LockX ->
                                normalize { newSelection | w = selection.w, e = selection.e }

                            LockY ->
                                normalize { newSelection | n = selection.n, s = selection.s }
                        )
            }

        Nothing ->
            model


update : OnBrush -> Brush dim -> Brush dim
update msg (Brush model) =
    Brush <|
        case msg of
            MouseDown mode shifting element position matrix ->
                let
                    ( px, py ) =
                        Events.normalizePointerPosition position matrix

                    selection =
                        if element == OverlayElement then
                            { n =
                                if not model.y then
                                    model.extent.n

                                else
                                    py
                            , w =
                                if not model.x then
                                    model.extent.w

                                else
                                    px
                            , s =
                                if not model.y then
                                    model.extent.s

                                else
                                    py
                            , e =
                                if not model.x then
                                    model.extent.e

                                else
                                    px
                            }

                        else
                            Maybe.withDefault model.extent model.selection
                in
                { model
                    | drag =
                        Just
                            { matrix = matrix
                            , current = ( px, py )
                            , origin = ( px, py )
                            , mode = mode
                            , lock = NoLock
                            , initialSelection = selection
                            , element = element
                            }
                    , shifting = shifting
                    , selection = Just selection
                }

            MouseMove position_ ->
                hasMoved
                    { model
                        | drag =
                            Maybe.map
                                (\drag ->
                                    { drag
                                        | current = position_
                                        , lock =
                                            if drag.lock == NoLock && model.shifting then
                                                if abs (Tuple.first position_ - Tuple.first drag.current) > abs (Tuple.second position_ - Tuple.second drag.current) then
                                                    LockY

                                                else
                                                    LockX

                                            else
                                                drag.lock
                                    }
                                )
                                model.drag
                    }

            MouseUp ->
                { model
                    | drag = Nothing
                    , selection =
                        Maybe.andThen
                            (\selection ->
                                if selection.n == selection.s || selection.e == selection.w then
                                    Nothing

                                else
                                    Just selection
                            )
                            model.selection
                }

            ShiftDown ->
                { model | shifting = Debug.log "shifting" (model.x && model.y) }

            ShiftUp ->
                hasMoved { model | shifting = False, drag = Maybe.map (\drag -> { drag | lock = NoLock }) model.drag }

            AltDown ->
                case model.drag of
                    Just drag ->
                        if drag.mode == Handle then
                            let
                                position =
                                    Events.normalizePointerPosition drag.current drag.matrix

                                dx =
                                    Tuple.first position - Tuple.first drag.origin

                                dy =
                                    Tuple.second position - Tuple.second drag.origin

                                withSign toSign val fn =
                                    case toSign drag.element of
                                        Just sign ->
                                            fn sign (Maybe.map val model.selection |> Maybe.withDefault (val drag.initialSelection))

                                        Nothing ->
                                            Maybe.map val model.selection |> Maybe.withDefault (val drag.initialSelection)
                            in
                            hasMoved
                                { model
                                    | drag =
                                        Just
                                            { drag
                                                | mode = Center
                                                , initialSelection =
                                                    { n = withSign (toYSign model.y) .n (\sign n -> n + dy * sign)
                                                    , s = withSign (toYSign model.y) .s (\sign s -> s - dy * sign)
                                                    , e = withSign (toXSign model.x) .e (\sign e -> e - dx * sign)
                                                    , w = withSign (toXSign model.x) .w (\sign w -> w + dx * sign)
                                                    }
                                            }
                                }

                        else
                            model

                    Nothing ->
                        model

            AltUp ->
                case model.drag of
                    Just drag ->
                        if drag.mode == Center then
                            let
                                resetWithSign toSign cmp getter =
                                    case toSign drag.element of
                                        Just sign ->
                                            if cmp sign 0 then
                                                getter (Maybe.withDefault model.extent model.selection)

                                            else
                                                getter drag.initialSelection

                                        Nothing ->
                                            getter drag.initialSelection
                            in
                            hasMoved
                                { model
                                    | drag =
                                        Just
                                            { drag
                                                | mode = Handle
                                                , initialSelection =
                                                    { n = resetWithSign (toYSign model.y) (>) .n
                                                    , s = resetWithSign (toYSign model.y) (<) .s
                                                    , e = resetWithSign (toXSign model.x) (<) .e
                                                    , w = resetWithSign (toXSign model.x) (>) .w
                                                    }
                                            }
                                }

                        else
                            model

                    Nothing ->
                        model


subscriptions : Brush dim -> (OnBrush -> msg) -> Sub msg
subscriptions (Brush brush) tagger =
    Sub.batch
        [ case brush.drag of
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
                    , Browser.Events.onKeyDown
                        (D.andThen
                            (\key ->
                                case key of
                                    "Shift" ->
                                        if brush.x && brush.y then
                                            D.succeed (tagger ShiftDown)

                                        else
                                            D.fail ""

                                    "Alt" ->
                                        D.succeed (tagger AltDown)

                                    -- "Space" ->
                                    --     D.succeed (tagger SpaceDown)
                                    _ ->
                                        D.fail ""
                            )
                            (D.field "key" D.string)
                        )
                    , Browser.Events.onKeyUp
                        (D.andThen
                            (\key ->
                                case key of
                                    "Shift" ->
                                        D.succeed (tagger ShiftUp)

                                    "Alt" ->
                                        D.succeed (tagger AltUp)

                                    -- "Space" ->
                                    --     D.succeed (tagger SpaceDown)
                                    _ ->
                                        D.fail ""
                            )
                            (D.field "key" D.string)
                        )
                    ]

            Nothing ->
                Sub.none
        ]



--- View


type Attribute
    = Attribute


elementToCursor : Element -> String
elementToCursor element =
    case element of
        OverlayElement ->
            "crosshair"

        SelectionElement ->
            "move"

        HandleElement NHandle ->
            "ns-resize"

        HandleElement WHandle ->
            "ew-resize"

        HandleElement EHandle ->
            "ew-resize"

        HandleElement SHandle ->
            "ns-resize"

        HandleElement NWHandle ->
            "nwse-resize"

        HandleElement NEHandle ->
            "nesw-resize"

        HandleElement SWHandle ->
            "nesw-resize"

        HandleElement SEHandle ->
            "nwse-resize"


view : List Attribute -> (OnBrush -> msg) -> Brush dim -> Svg msg
view attrs tagger (Brush model) =
    let
        opts =
            {}

        handleSize =
            6

        handleSizeHalf =
            handleSize / 2

        rect tipe attributes { n, w, e, s } =
            Svg.rect
                ((Attr.x (String.fromFloat w)
                    :: Attr.y (String.fromFloat n)
                    :: Attr.width (String.fromFloat (e - w))
                    :: Attr.height (String.fromFloat (s - n))
                    :: attributes
                 )
                    ++ events tipe (Brush model) tagger
                )
                []

        handle tipe x y =
            Svg.rect
                ([ Attr.x (String.fromFloat (x - handleSizeHalf))
                 , Attr.y (String.fromFloat (y - handleSizeHalf))
                 , Attr.width (String.fromFloat handleSize)
                 , Attr.height (String.fromFloat handleSize)
                 , Attr.cursor (elementToCursor (HandleElement tipe))
                 ]
                    ++ events (HandleElement tipe) (Brush model) tagger
                )
                []
    in
    Svg.g [ Attr.fill "none", Maybe.map (always (Attr.pointerEvents "none")) model.drag |> Maybe.withDefault (Attr.pointerEvents "all") ]
        (rect OverlayElement [ Attr.cursor (Maybe.map (.element >> elementToCursor) model.drag |> Maybe.withDefault "crosshair"), Attr.pointerEvents "all" ] model.extent
            :: (case model.selection of
                    Just selection ->
                        rect SelectionElement [ Attr.cursor "move", Attr.fill "#777", Attr.fillOpacity "0.3", Attr.stroke "white", Attr.shapeRendering "crispEdges" ] selection
                            :: (if model.x then
                                    [ rect (HandleElement EHandle)
                                        [ Attr.cursor "ew-resize" ]
                                        { n = selection.n - handleSizeHalf
                                        , s = selection.s + handleSizeHalf
                                        , e = selection.e + handleSizeHalf
                                        , w = selection.e - handleSizeHalf
                                        }
                                    , rect (HandleElement WHandle)
                                        [ Attr.cursor "ew-resize" ]
                                        { n = selection.n - handleSizeHalf
                                        , s = selection.s + handleSizeHalf
                                        , e = selection.w + handleSizeHalf
                                        , w = selection.w - handleSizeHalf
                                        }
                                    ]

                                else
                                    []
                               )
                            ++ (if model.y then
                                    [ rect (HandleElement NHandle)
                                        [ Attr.cursor "ns-resize" ]
                                        { n = selection.n - handleSizeHalf
                                        , s = selection.n + handleSizeHalf
                                        , e = selection.e + handleSizeHalf
                                        , w = selection.w - handleSizeHalf
                                        }
                                    , rect (HandleElement SHandle)
                                        [ Attr.cursor "ns-resize" ]
                                        { n = selection.s - handleSizeHalf
                                        , s = selection.s + handleSizeHalf
                                        , e = selection.e + handleSizeHalf
                                        , w = selection.w - handleSizeHalf
                                        }
                                    ]

                                else
                                    []
                               )
                            ++ (if model.x && model.y then
                                    [ handle NWHandle selection.w selection.n
                                    , handle NEHandle selection.e selection.n
                                    , handle SWHandle selection.w selection.s
                                    , handle SEHandle selection.e selection.s
                                    ]

                                else
                                    []
                               )

                    Nothing ->
                        []
               )
        )
