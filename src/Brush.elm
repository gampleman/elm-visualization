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
    { top : Float, bottom : Float, left : Float, right : Float }


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


initX : Extent -> Brush OneDimensional
initX =
    init True False


initY : Extent -> Brush OneDimensional
initY =
    init False True


initXY : Extent -> Brush TwoDimensional
initXY =
    init True True


init : Bool -> Bool -> Extent -> Brush dimension
init x y extent =
    Brush { drag = Nothing, keysEnabled = True, shifting = False, x = x, y = y, selection = Nothing, extent = extent }


selection1d : Brush OneDimensional -> Maybe ( Float, Float )
selection1d (Brush { selection, x }) =
    Maybe.map
        (\{ top, left, bottom, right } ->
            if x then
                ( min right left, max right left )

            else
                ( min top bottom, max top bottom )
        )
        selection


selection2d : Brush TwoDimensional -> Maybe Extent
selection2d (Brush { selection }) =
    selection


type TransitionOption
    = Instantly


instantly : TransitionOption
instantly =
    Instantly


setSelection1d : TransitionOption -> ( Float, Float ) -> Brush OneDimensional -> Brush OneDimensional
setSelection1d _ ( a, b ) (Brush model) =
    if model.x then
        Brush
            { model
                | selection =
                    Just
                        { left = clamp model.extent.left a b
                        , right = clamp a b model.extent.right
                        , top = model.extent.top
                        , bottom = model.extent.bottom
                        }
            }

    else
        Brush
            { model
                | selection =
                    Just
                        { left = model.extent.left
                        , right = model.extent.right
                        , top = clamp model.extent.top a b
                        , bottom = clamp a b model.extent.bottom
                        }
            }


setSelection2d : TransitionOption -> Extent -> Brush TwoDimensional -> Brush TwoDimensional
setSelection2d _ sel (Brush model) =
    Brush
        { model
            | selection =
                Just
                    { left = max sel.left model.extent.left
                    , right = min sel.left model.extent.left
                    , top = max model.extent.top sel.top
                    , bottom = min model.extent.bottom sel.bottom
                    }
        }


clearSelection : Brush dim -> Brush dim
clearSelection (Brush model) =
    Brush { model | selection = Nothing }


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
                                        { initialSelection | left = initialSelection.left + clamp (extent.left - initialSelection.left) (extent.right - initialSelection.left) dx }

                                    else if toXSign model.x element == Just 1 then
                                        { initialSelection | right = initialSelection.right + clamp (extent.left - initialSelection.right) (extent.right - initialSelection.right) dx }

                                    else
                                        initialSelection
                            in
                            if toYSign model.y element == Just -1 then
                                { xModSelection | top = initialSelection.top + clamp (extent.top - initialSelection.top) (extent.bottom - initialSelection.top) dy }

                            else if toYSign model.y element == Just 1 then
                                { xModSelection | bottom = initialSelection.bottom + clamp (extent.top - initialSelection.bottom) (extent.bottom - initialSelection.bottom) dy }

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
                                | left = clamp extent.left extent.right (initialSelection.left - deltaX)
                                , right = clamp extent.left extent.right (initialSelection.right + deltaX)
                                , top = clamp extent.top extent.bottom (initialSelection.top - deltaY)
                                , bottom = clamp extent.top extent.bottom (initialSelection.bottom + deltaY)
                            }

                        _ ->
                            let
                                deltaX =
                                    if model.x then
                                        clamp (extent.left - drag.initialSelection.left) (extent.right - drag.initialSelection.right) dx

                                    else
                                        0

                                deltaY =
                                    if model.y then
                                        clamp (extent.top - initialSelection.top) (extent.bottom - initialSelection.bottom) dy

                                    else
                                        0
                            in
                            { left = initialSelection.left + deltaX
                            , right = initialSelection.right + deltaX
                            , bottom = initialSelection.bottom + deltaY
                            , top = initialSelection.top + deltaY
                            }

                normalize sel =
                    { top = min sel.top sel.bottom
                    , bottom = max sel.top sel.bottom
                    , left = min sel.left sel.right
                    , right = max sel.left sel.right
                    }
            in
            { model
                | selection =
                    Just
                        (case drag.lock of
                            NoLock ->
                                normalize newSelection

                            LockX ->
                                normalize { newSelection | left = selection.left, right = selection.right }

                            LockY ->
                                normalize { newSelection | top = selection.top, bottom = selection.bottom }
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
                            { top =
                                if not model.y then
                                    model.extent.top

                                else
                                    py
                            , left =
                                if not model.x then
                                    model.extent.left

                                else
                                    px
                            , bottom =
                                if not model.y then
                                    model.extent.bottom

                                else
                                    py
                            , right =
                                if not model.x then
                                    model.extent.right

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
                                if selection.top == selection.bottom || selection.right == selection.left then
                                    Nothing

                                else
                                    Just selection
                            )
                            model.selection
                }

            ShiftDown ->
                { model | shifting = model.x && model.y }

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
                                                    { top = withSign (toYSign model.y) .top (\sign n -> n + dy * sign)
                                                    , bottom = withSign (toYSign model.y) .bottom (\sign s -> s - dy * sign)
                                                    , right = withSign (toXSign model.x) .right (\sign e -> e - dx * sign)
                                                    , left = withSign (toXSign model.x) .left (\sign w -> w + dx * sign)
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
                                                    { top = resetWithSign (toYSign model.y) (>) .top
                                                    , bottom = resetWithSign (toYSign model.y) (<) .bottom
                                                    , right = resetWithSign (toXSign model.x) (<) .right
                                                    , left = resetWithSign (toXSign model.x) (>) .left
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

        rect tipe attributes { top, left, right, bottom } =
            Svg.rect
                ((Attr.x (String.fromFloat left)
                    :: Attr.y (String.fromFloat top)
                    :: Attr.width (String.fromFloat (right - left))
                    :: Attr.height (String.fromFloat (bottom - top))
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
                                        { top = selection.top - handleSizeHalf
                                        , bottom = selection.bottom + handleSizeHalf
                                        , right = selection.right + handleSizeHalf
                                        , left = selection.right - handleSizeHalf
                                        }
                                    , rect (HandleElement WHandle)
                                        [ Attr.cursor "ew-resize" ]
                                        { top = selection.top - handleSizeHalf
                                        , bottom = selection.bottom + handleSizeHalf
                                        , right = selection.left + handleSizeHalf
                                        , left = selection.left - handleSizeHalf
                                        }
                                    ]

                                else
                                    []
                               )
                            ++ (if model.y then
                                    [ rect (HandleElement NHandle)
                                        [ Attr.cursor "ns-resize" ]
                                        { top = selection.top - handleSizeHalf
                                        , bottom = selection.top + handleSizeHalf
                                        , right = selection.right + handleSizeHalf
                                        , left = selection.left - handleSizeHalf
                                        }
                                    , rect (HandleElement SHandle)
                                        [ Attr.cursor "ns-resize" ]
                                        { top = selection.bottom - handleSizeHalf
                                        , bottom = selection.bottom + handleSizeHalf
                                        , right = selection.right + handleSizeHalf
                                        , left = selection.left - handleSizeHalf
                                        }
                                    ]

                                else
                                    []
                               )
                            ++ (if model.x && model.y then
                                    [ handle NWHandle selection.left selection.top
                                    , handle NEHandle selection.right selection.top
                                    , handle SWHandle selection.left selection.bottom
                                    , handle SEHandle selection.right selection.bottom
                                    ]

                                else
                                    []
                               )

                    Nothing ->
                        []
               )
        )
