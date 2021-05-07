module Brush exposing
    ( Brush, OneDimensional, TwoDimensional
    , initX, initY, initXY, Extent, keyboardModifiersEnabled
    , selection1d, selection2d
    , OnBrush, update, subscriptions
    , setSelection1d, setSelection2d, clearSelection, TransitionOption, instantly
    , view, Attribute, selectedArea, handleSize
    , bottomHandle, leftHandle, rightHandle, topHandle, topLeftHandle, topRightHandle, bottomLeftHandle, bottomRightHandle
    )

{-| Brushing is the interactive specification a one- or two-dimensional selected region using a pointing gesture, such as by clicking and dragging the mouse. Brushing is often used to select discrete elements, such as dots in a scatterplot or files on a desktop. It can also be used to zoom-in to a region of interest, or to select continuous regions for cross-filtering data.

This module implements brushing for mouse events using SVG. Click and drag on the brush selection to translate the selection. Click and drag on one of the selection handles to move the corresponding edge (or edges) of the selection. Click and drag on the invisible overlay to define a new brush selection, or click anywhere within the brushable region while holding down the META (⌘) key. Holding down the ALT (⌥) key while moving the brush causes it to reposition around its center.

@docs Brush, OneDimensional, TwoDimensional


## Configuring the Brush behavior

Initializing the brush always requires you to specify in local coordinates the rectangular region where the brush will be active.

@docs initX, initY, initXY, Extent, keyboardModifiersEnabled


## Querying the brush state

@docs selection1d, selection2d


## Updating the Brush

@docs OnBrush, update, subscriptions


## Manipulating the Selection

@docs setSelection1d, setSelection2d, clearSelection, TransitionOption, instantly


## View

@docs view, Attribute, selectedArea, handleSize

The handle customization functions take a suggested extent for where you should draw them.
This is basically the line/point they represent extended by `handleSize / 2` in each direction.
However, you do not need to abide by these dimensions exactly, you can render much larger or smaller objects there.

The second argument is a suggested cursor, which you may want to place in the `cursor` attribute.

@docs bottomHandle, leftHandle, rightHandle, topHandle, topLeftHandle, topRightHandle, bottomLeftHandle, bottomRightHandle

-}

import Browser.Events
import Events
import Json.Decode as D
import Svg exposing (Svg)
import Svg.Attributes as Attr
import Svg.Events exposing (custom)


{-| -}
type OneDimensional
    = OneDimensional Never


{-| -}
type TwoDimensional
    = TwoDimensional Never


{-| Defines a rectangular region.
-}
type alias Extent =
    { top : Float, bottom : Float, left : Float, right : Float }


{-| Encapsulates all the data we need to maintain the state of the brush. The dimension type variable can either be `OneDimensional` or `TwoDimensional`. This allows us to share a lot of the implementation details as well as implement generic UI customizations over brushes regardless of their dimensionality.

You will typically want to store this in your model.

-}
type Brush dimension
    = Brush
        { drag :
            Maybe
                { origin : ( Float, Float )
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


{-| This is the Msg type that this module uses for communicating between the update and the view.

Note that when handling these messages, it is also extremely likely that the brush selection has somehow changed, so you can also use that place in your update function to react to that.

-}
type OnBrush
    = MouseDown Mode Bool Element ( Float, Float )
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
    [ custom "touchstart"
        (D.andThen
            (\touches ->
                case touches of
                    { position } :: _ ->
                        let
                            mode =
                                if element == SelectionElement then
                                    Drag

                                else
                                    Handle
                        in
                        D.succeed
                            { message = tagger (MouseDown mode False element position)
                            , stopPropagation = True
                            , preventDefault = False
                            }

                    [] ->
                        D.fail ""
            )
            Events.decodeTouches
        )
    , custom "touchmove"
        (D.andThen
            (\touches ->
                case touches of
                    { position } :: _ ->
                        D.succeed
                            { message = tagger (MouseMove position)
                            , stopPropagation = True
                            , preventDefault = True
                            }

                    [] ->
                        D.fail ""
            )
            Events.decodeTouches
        )
    , custom "touchend"
        (D.succeed
            { message = tagger MouseUp
            , stopPropagation = True
            , preventDefault = True
            }
        )
    ]
        ++ (case drag of
                Nothing ->
                    [ custom "mousedown"
                        (D.map4
                            (\pos meta alt shift ->
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
                                { message = tagger (MouseDown mode (keysEnabled && shift) element pos)
                                , stopPropagation = True
                                , preventDefault = False
                                }
                            )
                            Events.decodeMousePosition
                            (D.field "metaKey" D.bool)
                            (D.field "altKey" D.bool)
                            (D.field "shiftKey" D.bool)
                        )
                    ]

                Just _ ->
                    []
           )


{-| Initializes a brush that allows brusing in the X axis.
-}
initX : Extent -> Brush OneDimensional
initX =
    init True False


{-| Initializes a brush that allows brusing in the Y axis.
-}
initY : Extent -> Brush OneDimensional
initY =
    init False True


{-| Initializes a two dimensional brush.
-}
initXY : Extent -> Brush TwoDimensional
initXY =
    init True True


init : Bool -> Bool -> Extent -> Brush dimension
init x y extent =
    Brush { drag = Nothing, keysEnabled = True, shifting = False, x = x, y = y, selection = Nothing, extent = extent }


{-| By default the brush will use the meta/alt and shift keys to change behavior. You can disable this with this function.
-}
keyboardModifiersEnabled : Bool -> Brush dimension -> Brush dimension
keyboardModifiersEnabled enabled (Brush model) =
    Brush { model | keysEnabled = enabled }


{-| Exposes the selection for a single dimensional brush, where the first number should always be less than the second.
-}
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


{-| Exposes the selection for a two dimensional brush.
-}
selection2d : Brush TwoDimensional -> Maybe Extent
selection2d (Brush { selection }) =
    selection


{-| -}
type TransitionOption
    = Instantly


{-| Perfom the update to the brush instantly, rather than with an animation (animations are not supported yet.)
-}
instantly : TransitionOption
instantly =
    Instantly


{-| Programatically set the selection of the Brush.
-}
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


{-| Programatically set the selection of the Brush (in two dimensions).
-}
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


{-| Clears the selection programmatically.

     brush
       |> Brush.clearSelection
       |> Brush.selection1d --> Nothing

-}
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
                    drag.current

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


{-| Call this in your update function to make the brush work!
-}
update : OnBrush -> Brush dim -> Brush dim
update msg (Brush model) =
    Brush <|
        case msg of
            MouseDown mode shifting element ( px, py ) ->
                let
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
                            { current = ( px, py )
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
                                    drag.current

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


{-| Don't forget the subscriptions, otherwise drag gestures won't work!
-}
subscriptions : Brush dim -> (OnBrush -> msg) -> Sub msg
subscriptions (Brush brush) tagger =
    Sub.batch
        [ case brush.drag of
            Just _ ->
                Sub.batch
                    [ Browser.Events.onMouseMove
                        (D.map
                            (MouseMove >> tagger)
                            Events.decodeMousePosition
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


{-| This is a type used to customize the view function of this module. However most of the functions that produce the type
may appear to also consume it. However, this is not the case, the functions take VirtualDom attributes, but produce this Attribute type.
-}
type Attribute msg
    = HandleSize Float
    | LeftHandle (Extent -> String -> List (Svg.Attribute msg) -> Svg msg)
    | RightHandle (Extent -> String -> List (Svg.Attribute msg) -> Svg msg)
    | TopHandle (Extent -> String -> List (Svg.Attribute msg) -> Svg msg)
    | BottomHandle (Extent -> String -> List (Svg.Attribute msg) -> Svg msg)
    | TopLeftHandle (Extent -> String -> List (Svg.Attribute msg) -> Svg msg)
    | TopRightHandle (Extent -> String -> List (Svg.Attribute msg) -> Svg msg)
    | BottomLeftHandle (Extent -> String -> List (Svg.Attribute msg) -> Svg msg)
    | BottomRightHandle (Extent -> String -> List (Svg.Attribute msg) -> Svg msg)
    | SelectedArea (Extent -> String -> List (Svg.Attribute msg) -> Svg msg)


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


{-| The number in pixels which determines the size of the handles.
-}
handleSize : Float -> Attribute msg
handleSize =
    HandleSize


{-| Customise how to render the left handle.
-}
leftHandle : (Extent -> String -> List (Svg.Attribute msg) -> Svg msg) -> Attribute msg
leftHandle =
    LeftHandle


{-| Customise how to render the right handle.
-}
rightHandle : (Extent -> String -> List (Svg.Attribute msg) -> Svg msg) -> Attribute msg
rightHandle =
    RightHandle


{-| Customise how to render the top handle.
-}
topHandle : (Extent -> String -> List (Svg.Attribute msg) -> Svg msg) -> Attribute msg
topHandle =
    TopHandle


{-| Customise how to render the bottom handle.
-}
bottomHandle : (Extent -> String -> List (Svg.Attribute msg) -> Svg msg) -> Attribute msg
bottomHandle =
    BottomHandle


{-| Customise how to render the top left handle. Only applies to a 2D brush.
-}
topLeftHandle : (Extent -> String -> List (Svg.Attribute msg) -> Svg msg) -> Attribute msg
topLeftHandle =
    TopLeftHandle


{-| Customise how to render the top right handle. Only applies to a 2D brush.
-}
topRightHandle : (Extent -> String -> List (Svg.Attribute msg) -> Svg msg) -> Attribute msg
topRightHandle =
    TopRightHandle


{-| Customise how to render the bottom left handle. Only applies to a 2D brush.
-}
bottomLeftHandle : (Extent -> String -> List (Svg.Attribute msg) -> Svg msg) -> Attribute msg
bottomLeftHandle =
    BottomLeftHandle


{-| Customise how to render the bottom right handle. Only applies to a 2D brush.
-}
bottomRightHandle : (Extent -> String -> List (Svg.Attribute msg) -> Svg msg) -> Attribute msg
bottomRightHandle =
    BottomRightHandle


{-| Customize rendering for the rectangular region that is the selection.

The first argument is the actual coordinates of the selection, the second is the suggested cursor ("move"),
and finally the event handlers.

The default version renderes a `<rect fill="#777" fill-opacity="0.3" stroke="white" shape-rendering="crispEdges">`.

-}
selectedArea : (Extent -> String -> List (Svg.Attribute msg) -> Svg msg) -> Attribute msg
selectedArea =
    SelectedArea


{-| Actually renders the the brush selection widget. You can customise the appearance by passing in functions to render the individual pieces.

The actual widget consists of:

1.  An overlay invisible rectange which covers the interactive area.
2.  The selection rectangle.
3.  Handles in each direction the brush supports being dragged to.

-}
view : List (Attribute msg) -> (OnBrush -> msg) -> Brush dim -> Svg msg
view attrs tagger (Brush model) =
    let
        opts =
            List.foldr
                (\attr options ->
                    case attr of
                        HandleSize size ->
                            { options | handleSize = size }

                        LeftHandle handle ->
                            { options | leftHandle = handle }

                        RightHandle handle ->
                            { options | rightHandle = handle }

                        TopHandle handle ->
                            { options | topHandle = handle }

                        BottomHandle handle ->
                            { options | bottomHandle = handle }

                        TopLeftHandle handle ->
                            { options | topLeftHandle = handle }

                        TopRightHandle handle ->
                            { options | topRightHandle = handle }

                        BottomLeftHandle handle ->
                            { options | bottomLeftHandle = handle }

                        BottomRightHandle handle ->
                            { options | bottomRightHandle = handle }

                        SelectedArea fn ->
                            { options | selection = fn }
                )
                { handleSize = 6
                , leftHandle = defaultHandleImpl
                , rightHandle = defaultHandleImpl
                , topHandle = defaultHandleImpl
                , bottomHandle = defaultHandleImpl
                , topLeftHandle = defaultHandleImpl
                , topRightHandle = defaultHandleImpl
                , bottomLeftHandle = defaultHandleImpl
                , bottomRightHandle = defaultHandleImpl
                , selection = defaultSelectedAreaImpl
                }
                attrs

        defaultHandleImpl : Extent -> String -> List (Svg.Attribute msg) -> Svg msg
        defaultHandleImpl { top, left, right, bottom } cursor attributes =
            Svg.rect
                (Attr.x (String.fromFloat left)
                    :: Attr.y (String.fromFloat top)
                    :: Attr.width (String.fromFloat (right - left))
                    :: Attr.height (String.fromFloat (bottom - top))
                    :: Attr.cursor cursor
                    :: Attr.fill "none"
                    :: attributes
                )
                []

        defaultSelectedAreaImpl { top, left, right, bottom } cursor attributes =
            Svg.rect
                (Attr.x (String.fromFloat left)
                    :: Attr.y (String.fromFloat top)
                    :: Attr.width (String.fromFloat (right - left))
                    :: Attr.height (String.fromFloat (bottom - top))
                    :: Attr.cursor cursor
                    :: Attr.fill "#777"
                    :: Attr.fillOpacity "0.3"
                    :: Attr.stroke "white"
                    :: Attr.shapeRendering "crispEdges"
                    :: attributes
                )
                []

        defaultOverlayImpl { top, left, right, bottom } cursor attributes =
            Svg.rect
                (Attr.x (String.fromFloat left)
                    :: Attr.y (String.fromFloat top)
                    :: Attr.width (String.fromFloat (right - left))
                    :: Attr.height (String.fromFloat (bottom - top))
                    :: Attr.cursor cursor
                    :: Attr.fill "none"
                    :: Attr.pointerEvents "all"
                    :: attributes
                )
                []

        handleSizeHalf =
            opts.handleSize / 2

        cornerHandle fn tipe x y =
            fn
                { left = x - handleSizeHalf
                , right = x + handleSizeHalf
                , top = y - handleSizeHalf
                , bottom = y + handleSizeHalf
                }
                (elementToCursor (HandleElement tipe))
                (events (HandleElement tipe) (Brush model) tagger)
    in
    Svg.g [ Maybe.map (always (Attr.pointerEvents "none")) model.drag |> Maybe.withDefault (Attr.pointerEvents "all") ]
        (defaultOverlayImpl model.extent (Maybe.map (.element >> elementToCursor) model.drag |> Maybe.withDefault "crosshair") (events OverlayElement (Brush model) tagger)
            :: (case model.selection of
                    Just selection ->
                        opts.selection selection "move" (events SelectionElement (Brush model) tagger)
                            :: (if model.x then
                                    [ opts.rightHandle
                                        { top = selection.top - handleSizeHalf
                                        , bottom = selection.bottom + handleSizeHalf
                                        , right = selection.right + handleSizeHalf
                                        , left = selection.right - handleSizeHalf
                                        }
                                        "ew-resize"
                                        (events (HandleElement EHandle) (Brush model) tagger)
                                    , opts.leftHandle
                                        { top = selection.top - handleSizeHalf
                                        , bottom = selection.bottom + handleSizeHalf
                                        , right = selection.left + handleSizeHalf
                                        , left = selection.left - handleSizeHalf
                                        }
                                        "ew-resize"
                                        (events (HandleElement WHandle) (Brush model) tagger)
                                    ]

                                else
                                    []
                               )
                            ++ (if model.y then
                                    [ opts.topHandle
                                        { top = selection.top - handleSizeHalf
                                        , bottom = selection.top + handleSizeHalf
                                        , right = selection.right + handleSizeHalf
                                        , left = selection.left - handleSizeHalf
                                        }
                                        "ns-resize"
                                        (events (HandleElement NHandle) (Brush model) tagger)
                                    , opts.bottomHandle
                                        { top = selection.bottom - handleSizeHalf
                                        , bottom = selection.bottom + handleSizeHalf
                                        , right = selection.right + handleSizeHalf
                                        , left = selection.left - handleSizeHalf
                                        }
                                        "ns-resize"
                                        (events (HandleElement SHandle) (Brush model) tagger)
                                    ]

                                else
                                    []
                               )
                            ++ (if model.x && model.y then
                                    [ cornerHandle opts.topLeftHandle NWHandle selection.left selection.top
                                    , cornerHandle opts.topRightHandle NEHandle selection.right selection.top
                                    , cornerHandle opts.bottomLeftHandle SWHandle selection.left selection.bottom
                                    , cornerHandle opts.bottomRightHandle SEHandle selection.right selection.bottom
                                    ]

                                else
                                    []
                               )

                    Nothing ->
                        []
               )
        )
