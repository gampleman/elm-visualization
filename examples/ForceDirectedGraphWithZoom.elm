module ForceDirectedGraphWithZoom exposing (main)

{-| This example demonstrates a force directed graph with zoom and drag
functionality.

@delay 5
@category Advanced

-}

import Browser
import Browser.Dom as Dom
import Browser.Events as Events
import Color
import Force
import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import Html exposing (div)
import Html.Attributes exposing (style)
import Html.Events.Extra.Mouse as Mouse
import Json.Decode as Decode
import Task
import TypedSvg exposing (circle, defs, g, line, marker, polygon, rect, svg, text_, title)
import TypedSvg.Attributes as Attrs exposing (class, cursor, fill, fontSize, id, markerEnd, markerHeight, markerWidth, orient, pointerEvents, points, refX, refY, stroke)
import TypedSvg.Attributes.InPx exposing (cx, cy, dx, dy, r, strokeWidth, x1, x2, y1, y2)
import TypedSvg.Core exposing (Attribute, Svg, text)
import TypedSvg.Types exposing (AlignmentBaseline(..), AnchorAlignment(..), Cursor(..), Length(..), Paint(..))
import Zoom exposing (OnZoom, Zoom)



-- Constants


elementId : String
elementId =
    "exercise-graph"


edgeColor : Paint
edgeColor =
    Paint <| Color.rgb255 180 180 180



-- Types


type Msg
    = DragAt ( Float, Float )
    | DragEnd ( Float, Float )
    | DragStart NodeId ( Float, Float )
    | ReceiveElementPosition (Result Dom.Error Dom.Element)
    | Resize
    | Tick
    | ZoomMsg OnZoom


{-| In order to correctly calculate the node positions, we need to know the
coordinates of the svg element. The simulation is started when we
receive them.
-}
type Model
    = Init (Graph Entity ())
    | Ready ReadyState


type alias ReadyState =
    { drag : Maybe Drag
    , graph : Graph Entity ()
    , simulation : Force.State NodeId
    , zoom : Zoom

    -- The position and dimensions of the svg element.
    , element : Element

    -- If you immediately show the graph when moving from `Init` to `Ready`,
    -- you will briefly see the nodes in the upper left corner before the first
    -- simulation tick positions them in the center. To avoid this sudden jump,
    -- `showGraph` is initialized with `False` and set to `True` with the first
    -- `Tick`.
    , showGraph : Bool
    }


type alias Drag =
    { current : ( Float, Float )
    , index : NodeId
    , start : ( Float, Float )
    }


type alias Element =
    { height : Float
    , width : Float
    , x : Float
    , y : Float
    }


type alias Entity =
    Force.Entity NodeId { value : String }



-- Init


{-| We initialize the graph here, but we don't start the simulation yet, because
we first need the position and dimensions of the svg element to calculate the
correct node positions and the center force.
-}
init : () -> ( Model, Cmd Msg )
init _ =
    let
        graph : Graph Entity ()
        graph =
            Graph.mapContexts initNode graphData
    in
    ( Init graph, getElementPosition )


{-| The graph data we defined at the end of the module has the type
`Graph String ()`. We have to convert it into a `Graph Entity ()`.
`Force.Entity` is an extensible record which includes the coordinates for the
node.
-}
initNode : NodeContext String () -> NodeContext Entity ()
initNode ctx =
    { node =
        { label = Force.entity ctx.node.id ctx.node.label
        , id = ctx.node.id
        }
    , incoming = ctx.incoming
    , outgoing = ctx.outgoing
    }


{-| Initializes the simulation by setting the forces for the graph.
-}
initSimulation : Graph Entity () -> Float -> Float -> Force.State NodeId
initSimulation graph width height =
    let
        link : { c | from : a, to : b } -> ( a, b )
        link { from, to } =
            ( from, to )
    in
    Force.simulation
        [ -- Defines the force that pulls connected nodes together. You can use
          -- `Force.customLinks` if you need to adjust the distance and
          -- strength.
          Force.links <| List.map link <| Graph.edges graph

        -- Defines the force that pushes the nodes apart. The default strength
        -- is `-30`, but since we are drawing fairly large circles for each
        -- node, we need to increase the repulsion by decreasing the strength to
        -- `-150`.
        , Force.manyBodyStrength -150 <| List.map .id <| Graph.nodes graph
        , Force.collision 21 <| List.map .id <| Graph.nodes graph

        -- Defines the force that pulls nodes to a center. We set the center
        -- coordinates to the center of the svg viewport.
        , Force.center (width / 2) (height / 2)
        ]
        |> Force.iterations 400


{-| Initializes the zoom and sets a minimum and maximum zoom level.

You can also use `Zoom.translateExtent` to restrict the area in which the user
may drag, but since the graph is larger than the viewport and the exact
dimensions depend on the data and the final layout, you would either need to use
some kind of heuristic for the final dimensions here, or you would have to let
the simulation play out (or use `Force.computeSimulate` to calculate it at
once), find the min and max x and y positions of the graph nodes and use those
values to set the translate extent.

-}
initZoom : Element -> Zoom
initZoom element =
    Zoom.init { width = element.width, height = element.height }
        |> Zoom.scaleExtent 0.1 2



-- Subscriptions


{-| We have three groups of subscriptions:

1.  Mouse events, to handle mouse interaction with the nodes.
2.  A subscription on the animation frame, to trigger simulation ticks.
3.  Browser resizes, to update the zoom state and the position of the nodes
    when the size and position of the svg viewport change.

We want to make sure that we only subscribe to mouse events while there is
a mouse interaction in progress, and that we only subscribe to
`Browser.Events.onAnimationFrame` while the simulation is in progress.

-}
subscriptions : Model -> Sub Msg
subscriptions model =
    let
        dragSubscriptions : Sub Msg
        dragSubscriptions =
            Sub.batch
                [ Events.onMouseMove
                    (Decode.map (.clientPos >> DragAt) Mouse.eventDecoder)
                , Events.onMouseUp
                    (Decode.map (.clientPos >> DragEnd) Mouse.eventDecoder)
                , Events.onAnimationFrame (always Tick)
                ]

        readySubscriptions : ReadyState -> Sub Msg
        readySubscriptions { drag, simulation, zoom } =
            Sub.batch
                [ Zoom.subscriptions zoom ZoomMsg
                , case drag of
                    Nothing ->
                        if Force.isCompleted simulation then
                            Sub.none

                        else
                            Events.onAnimationFrame (always Tick)

                    Just _ ->
                        dragSubscriptions
                ]
    in
    Sub.batch
        [ case model of
            Init _ ->
                Sub.none

            Ready state ->
                readySubscriptions state
        , Events.onResize (\_ _ -> Resize)
        ]



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( Tick, Ready state ) ->
            handleTick state

        ( Tick, Init _ ) ->
            ( model, Cmd.none )

        ( DragAt xy, Ready state ) ->
            handleDragAt xy state

        ( DragAt _, Init _ ) ->
            ( model, Cmd.none )

        ( DragEnd xy, Ready state ) ->
            case state.drag of
                Just { index } ->
                    ( Ready
                        { state
                            | drag = Nothing
                            , graph = updateNodePosition index xy state
                        }
                    , Cmd.none
                    )

                Nothing ->
                    ( Ready state, Cmd.none )

        ( DragEnd _, Init _ ) ->
            ( model, Cmd.none )

        ( DragStart index xy, Ready state ) ->
            ( Ready
                { state
                    | drag =
                        Just
                            { start = xy
                            , current = xy
                            , index = index
                            }
                }
            , Cmd.none
            )

        ( DragStart _ _, Init _ ) ->
            ( model, Cmd.none )

        ( ReceiveElementPosition (Ok { element }), Init graph ) ->
            -- When we get the svg element position and dimensions, we are
            -- ready to initialize the simulation and the zoom, but we cannot
            -- show the graph yet. If we did, we would see a noticable jump.
            ( Ready
                { drag = Nothing
                , element = element
                , graph = graph
                , showGraph = False
                , simulation =
                    initSimulation
                        graph
                        element.width
                        element.height
                , zoom = initZoom element
                }
            , Cmd.none
            )

        ( ReceiveElementPosition (Ok { element }), Ready state ) ->
            ( Ready
                { drag = state.drag
                , element = element
                , graph = state.graph
                , showGraph = True
                , simulation =
                    initSimulation
                        state.graph
                        element.width
                        element.height
                , zoom = initZoom element
                }
            , Cmd.none
            )

        ( ReceiveElementPosition (Err _), _ ) ->
            ( model, Cmd.none )

        ( Resize, _ ) ->
            ( model, getElementPosition )

        ( ZoomMsg zoomMsg, Ready state ) ->
            ( Ready { state | zoom = Zoom.update zoomMsg state.zoom }
            , Cmd.none
            )

        ( ZoomMsg _, Init _ ) ->
            ( model, Cmd.none )


handleDragAt : ( Float, Float ) -> ReadyState -> ( Model, Cmd Msg )
handleDragAt xy ({ drag, simulation } as state) =
    case drag of
        Just { start, index } ->
            ( Ready
                { state
                    | drag =
                        Just
                            { start = start
                            , current = xy
                            , index = index
                            }
                    , graph = updateNodePosition index xy state
                    , simulation = Force.reheat simulation
                }
            , Cmd.none
            )

        Nothing ->
            ( Ready state, Cmd.none )


handleTick : ReadyState -> ( Model, Cmd Msg )
handleTick state =
    let
        ( newSimulation, list ) =
            Force.tick state.simulation <|
                List.map .label <|
                    Graph.nodes state.graph
    in
    case state.drag of
        Nothing ->
            ( Ready
                { state
                    | graph = updateGraphWithList state.graph list
                    , showGraph = True
                    , simulation = newSimulation
                }
            , Cmd.none
            )

        Just { current, index } ->
            ( Ready
                { state
                    | graph =
                        Graph.update index
                            (Maybe.map
                                (updateNode
                                    (shiftPosition
                                        state.zoom
                                        ( state.element.x
                                        , state.element.y
                                        )
                                        current
                                    )
                                )
                            )
                            (updateGraphWithList state.graph list)
                    , showGraph = True
                    , simulation = newSimulation
                }
            , Cmd.none
            )


updateNode :
    ( Float, Float )
    -> NodeContext Entity ()
    -> NodeContext Entity ()
updateNode ( x, y ) nodeCtx =
    let
        nodeValue =
            nodeCtx.node.label
    in
    updateContextWithValue nodeCtx { nodeValue | x = x, y = y }


updateNodePosition : NodeId -> ( Float, Float ) -> ReadyState -> Graph Entity ()
updateNodePosition index xy state =
    Graph.update
        index
        (Maybe.map
            (updateNode
                (shiftPosition
                    state.zoom
                    ( state.element.x, state.element.y )
                    xy
                )
            )
        )
        state.graph


updateContextWithValue :
    NodeContext Entity ()
    -> Entity
    -> NodeContext Entity ()
updateContextWithValue nodeCtx value =
    let
        node =
            nodeCtx.node
    in
    { nodeCtx | node = { node | label = value } }


updateGraphWithList : Graph Entity () -> List Entity -> Graph Entity ()
updateGraphWithList =
    let
        graphUpdater value =
            Maybe.map (\ctx -> updateContextWithValue ctx value)
    in
    List.foldr (\node graph -> Graph.update node.id (graphUpdater node) graph)


{-| The mouse events for drag start, drag at and drag end read the client
position of the cursor, which is relative to the browser viewport. However,
the node positions are relative to the svg viewport. This function adjusts the
coordinates accordingly. It also takes the current zoom level and position
into consideration.
-}
shiftPosition : Zoom -> ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
shiftPosition zoom ( elementX, elementY ) ( clientX, clientY ) =
    let
        zoomRecord =
            Zoom.asRecord zoom
    in
    ( (clientX - zoomRecord.translate.x - elementX) / zoomRecord.scale
    , (clientY - zoomRecord.translate.y - elementY) / zoomRecord.scale
    )



-- View


view : Model -> Svg Msg
view model =
    let
        zoomEvents : List (Attribute Msg)
        zoomEvents =
            case model of
                Init _ ->
                    []

                Ready { zoom } ->
                    Zoom.events zoom ZoomMsg

        zoomTransformAttr : Attribute Msg
        zoomTransformAttr =
            case model of
                Init _ ->
                    class []

                Ready { zoom } ->
                    Zoom.transform zoom
    in
    div
        [ style "width" "80%"
        , style "height" "400px"
        , style "margin" "50px auto"
        , style "border" "2px solid rgba(0, 0, 0, 0.85)"
        ]
        [ svg
            [ id elementId
            , Attrs.width <| Percent 100
            , Attrs.height <| Percent 100
            ]
            [ defs [] [ arrowhead ]
            , -- This transparent rectangle is placed in the background as a
              -- target for the zoom events. Note that the zoom transformation
              -- are not applied to this rectangle, but to group that contains
              -- the actual graph.
              rect
                ([ Attrs.width <| Percent 100
                 , Attrs.height <| Percent 100
                 , fill <| Paint <| Color.rgba 0 0 0 0
                 , cursor CursorMove
                 ]
                    ++ zoomEvents
                )
                []
            , g
                [ zoomTransformAttr ]
                [ renderGraph model ]
            ]
        ]


renderGraph : Model -> Svg Msg
renderGraph model =
    case model of
        Init _ ->
            text ""

        Ready { graph, showGraph } ->
            if showGraph then
                g
                    []
                    [ Graph.edges graph
                        |> List.map (linkElement graph)
                        |> g [ class [ "links" ] ]
                    , Graph.nodes graph
                        |> List.map nodeElement
                        |> g [ class [ "nodes" ] ]
                    ]

            else
                text ""


{-| Draws a single vertex (node).
-}
nodeElement : Node Entity -> Svg Msg
nodeElement node =
    g [ class [ "node" ] ]
        [ circle
            [ r 20
            , strokeWidth 3
            , fill (Paint Color.yellow)
            , stroke (Paint Color.black)
            , cursor CursorPointer

            -- The coordinates are initialized and updated by `Force.simulation`
            -- and `Force.tick`, respectively.
            , cx node.label.x
            , cy node.label.y

            -- Add event handler for starting a drag on the node.
            , onMouseDown node.id
            ]
            [ title [] [ text node.label.value ] ]
        , text_
            [ -- Align text label at the center of the circle.
              dx <| node.label.x
            , dy <| node.label.y
            , Attrs.alignmentBaseline AlignmentMiddle
            , Attrs.textAnchor AnchorMiddle

            -- styling
            , fontSize <| Px 8
            , fill (Paint Color.black)

            -- Setting pointer events to none allows the user to click on the
            -- element behind the text, so in this case the circle. If you
            -- position the text label outside of the circle, you also should
            -- do this, so that drag and zoom operations are not interrupted
            -- when the cursor is above the text.
            , pointerEvents "none"
            ]
            [ text node.label.value ]
        ]


{-| This function draws the lines between the vertices.
-}
linkElement : Graph Entity () -> Edge () -> Svg msg
linkElement graph edge =
    let
        source =
            Maybe.withDefault (Force.entity 0 "") <|
                Maybe.map (.node >> .label) <|
                    Graph.get edge.from graph

        target =
            Maybe.withDefault (Force.entity 0 "") <|
                Maybe.map (.node >> .label) <|
                    Graph.get edge.to graph
    in
    line
        [ x1 source.x
        , y1 source.y
        , x2 target.x
        , y2 target.y
        , strokeWidth 1
        , stroke edgeColor
        , markerEnd "url(#arrowhead)"
        ]
        []



-- Definitions


{-| This is the definition of the arrow head that is displayed at the end of
the edges.

It is a child of the svg `defs` element and can be referenced by its id with
`url(#arrowhead)`.

-}
arrowhead : Svg msg
arrowhead =
    marker
        [ id "arrowhead"
        , orient "auto"
        , markerWidth <| Px 8.0
        , markerHeight <| Px 6.0
        , refX "29"
        , refY "3"
        ]
        [ polygon
            [ points [ ( 0, 0 ), ( 8, 3 ), ( 0, 6 ) ]
            , fill edgeColor
            ]
            []
        ]



-- Events and tasks


{-| This is the event handler that handles clicks on the vertices (nodes).

The event catches the `clientPos`, which is a tuple with the
`MouseEvent.clientX` and `MouseEvent.clientY` values. These coordinates are
relative to the client area (browser viewport).

If the graph is positioned anywhere else than at the coordinates `(0, 0)`, the
svg element position must be subtracted when setting the node position. This is
handled in the update function by calling the `shiftPosition` function.

-}
onMouseDown : NodeId -> Attribute Msg
onMouseDown index =
    Mouse.onDown (.clientPos >> DragStart index)


{-| This function returns a command to retrieve the position of the svg element.
-}
getElementPosition : Cmd Msg
getElementPosition =
    Task.attempt ReceiveElementPosition (Dom.getElement elementId)



-- Main


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Data


{-| This is the dataset for the graph.

The names are random. The edges of the dataset are derived from
<http://konect.uni-koblenz.de/networks/moreno_highschool>.

-}
graphData : Graph String ()
graphData =
    Graph.fromNodeLabelsAndEdgePairs
        [ "Seth"
        , "Wesley"
        , "Antony"
        , "Deshawn"
        , "Grant"
        , "Zander"
        , "Peter"
        , "Ean"
        , "Camden"
        , "Jacob"
        , "Javon"
        , "Ace"
        , "Joe"
        , "Wyatt"
        , "Nicolas"
        , "Ibrahim"
        , "Kaiden"
        , "Branson"
        , "Jefferson"
        , "Douglas"
        , "Trenton"
        , "Chandler"
        , "Alexis"
        , "David"
        , "Johnathon"
        , "Lincoln"
        , "Jerry"
        , "Bradley"
        , "Darion"
        , "Devyn"
        , "Emanuel"
        , "Charles"
        , "Saul"
        , "Paxton"
        , "Raymond"
        , "Messiah"
        , "Chance"
        , "Beau"
        , "Addison"
        , "Quincy"
        , "Armando"
        , "Albert"
        , "Mathew"
        , "Martin"
        , "Matteo"
        , "Mekhi"
        , "Dale"
        , "Ramiro"
        , "Shamar"
        , "Timothy"
        , "Junior"
        , "Reuben"
        , "Sheldon"
        , "Mauricio"
        , "Dylan"
        , "Hudson"
        , "Fisher"
        , "Luis"
        , "Kyan"
        , "Graham"
        , "Jayvion"
        , "Eddie"
        , "Zion"
        , "Yair"
        , "Frank"
        , "Lukas"
        , "Vance"
        , "Anthony"
        , "Leon"
        ]
        [ ( 0, 1 )
        , ( 0, 2 )
        , ( 0, 3 )
        , ( 0, 4 )
        , ( 0, 5 )
        , ( 0, 6 )
        , ( 7, 3 )
        , ( 7, 4 )
        , ( 7, 8 )
        , ( 9, 2 )
        , ( 9, 8 )
        , ( 10, 11 )
        , ( 10, 12 )
        , ( 10, 13 )
        , ( 10, 14 )
        , ( 10, 15 )
        , ( 10, 16 )
        , ( 16, 14 )
        , ( 16, 10 )
        , ( 16, 15 )
        , ( 17, 18 )
        , ( 17, 19 )
        , ( 17, 20 )
        , ( 17, 4 )
        , ( 17, 21 )
        , ( 17, 22 )
        , ( 23, 18 )
        , ( 23, 19 )
        , ( 23, 3 )
        , ( 22, 18 )
        , ( 22, 1 )
        , ( 22, 19 )
        , ( 22, 24 )
        , ( 8, 25 )
        , ( 8, 20 )
        , ( 8, 3 )
        , ( 8, 4 )
        , ( 8, 26 )
        , ( 8, 27 )
        , ( 28, 13 )
        , ( 28, 14 )
        , ( 11, 12 )
        , ( 11, 14 )
        , ( 11, 29 )
        , ( 11, 10 )
        , ( 11, 30 )
        , ( 11, 31 )
        , ( 11, 32 )
        , ( 11, 33 )
        , ( 25, 20 )
        , ( 25, 3 )
        , ( 25, 4 )
        , ( 18, 19 )
        , ( 18, 20 )
        , ( 18, 3 )
        , ( 18, 4 )
        , ( 18, 16 )
        , ( 18, 17 )
        , ( 1, 18 )
        , ( 1, 3 )
        , ( 1, 4 )
        , ( 2, 0 )
        , ( 2, 25 )
        , ( 2, 20 )
        , ( 2, 3 )
        , ( 12, 13 )
        , ( 12, 34 )
        , ( 12, 15 )
        , ( 12, 35 )
        , ( 12, 36 )
        , ( 19, 0 )
        , ( 19, 18 )
        , ( 19, 4 )
        , ( 19, 23 )
        , ( 19, 22 )
        , ( 13, 11 )
        , ( 13, 12 )
        , ( 13, 14 )
        , ( 14, 11 )
        , ( 14, 25 )
        , ( 14, 12 )
        , ( 14, 13 )
        , ( 14, 37 )
        , ( 14, 10 )
        , ( 14, 16 )
        , ( 14, 8 )
        , ( 20, 25 )
        , ( 20, 14 )
        , ( 20, 3 )
        , ( 20, 4 )
        , ( 20, 38 )
        , ( 20, 17 )
        , ( 20, 8 )
        , ( 3, 20 )
        , ( 3, 4 )
        , ( 3, 38 )
        , ( 3, 26 )
        , ( 3, 5 )
        , ( 3, 6 )
        , ( 4, 20 )
        , ( 4, 3 )
        , ( 4, 24 )
        , ( 4, 38 )
        , ( 4, 26 )
        , ( 39, 40 )
        , ( 39, 15 )
        , ( 39, 31 )
        , ( 39, 32 )
        , ( 39, 33 )
        , ( 39, 41 )
        , ( 39, 35 )
        , ( 39, 42 )
        , ( 39, 43 )
        , ( 39, 36 )
        , ( 39, 44 )
        , ( 39, 45 )
        , ( 46, 24 )
        , ( 46, 26 )
        , ( 47, 48 )
        , ( 47, 49 )
        , ( 47, 50 )
        , ( 47, 51 )
        , ( 47, 52 )
        , ( 47, 53 )
        , ( 47, 40 )
        , ( 47, 34 )
        , ( 47, 30 )
        , ( 37, 13 )
        , ( 37, 51 )
        , ( 37, 54 )
        , ( 37, 40 )
        , ( 37, 15 )
        , ( 37, 16 )
        , ( 24, 4 )
        , ( 24, 38 )
        , ( 24, 53 )
        , ( 21, 18 )
        , ( 21, 29 )
        , ( 21, 52 )
        , ( 21, 38 )
        , ( 21, 30 )
        , ( 48, 51 )
        , ( 48, 54 )
        , ( 48, 55 )
        , ( 56, 28 )
        , ( 56, 39 )
        , ( 56, 54 )
        , ( 56, 40 )
        , ( 56, 31 )
        , ( 49, 47 )
        , ( 49, 21 )
        , ( 49, 29 )
        , ( 49, 52 )
        , ( 49, 38 )
        , ( 49, 53 )
        , ( 49, 15 )
        , ( 49, 35 )
        , ( 29, 52 )
        , ( 29, 34 )
        , ( 29, 30 )
        , ( 29, 15 )
        , ( 50, 53 )
        , ( 50, 57 )
        , ( 51, 48 )
        , ( 51, 55 )
        , ( 52, 21 )
        , ( 52, 49 )
        , ( 52, 29 )
        , ( 52, 34 )
        , ( 52, 30 )
        , ( 52, 15 )
        , ( 54, 37 )
        , ( 54, 48 )
        , ( 54, 56 )
        , ( 54, 40 )
        , ( 38, 3 )
        , ( 38, 4 )
        , ( 38, 24 )
        , ( 38, 53 )
        , ( 38, 26 )
        , ( 38, 6 )
        , ( 53, 24 )
        , ( 53, 21 )
        , ( 53, 50 )
        , ( 53, 38 )
        , ( 53, 57 )
        , ( 53, 58 )
        , ( 53, 27 )
        , ( 40, 37 )
        , ( 40, 48 )
        , ( 40, 51 )
        , ( 40, 54 )
        , ( 34, 12 )
        , ( 34, 29 )
        , ( 34, 30 )
        , ( 34, 15 )
        , ( 34, 36 )
        , ( 30, 21 )
        , ( 30, 52 )
        , ( 30, 34 )
        , ( 30, 15 )
        , ( 30, 36 )
        , ( 15, 29 )
        , ( 15, 34 )
        , ( 15, 30 )
        , ( 15, 36 )
        , ( 59, 60 )
        , ( 59, 31 )
        , ( 61, 62 )
        , ( 61, 26 )
        , ( 61, 5 )
        , ( 61, 6 )
        , ( 61, 63 )
        , ( 61, 27 )
        , ( 57, 50 )
        , ( 57, 53 )
        , ( 57, 62 )
        , ( 57, 26 )
        , ( 57, 5 )
        , ( 57, 6 )
        , ( 57, 58 )
        , ( 60, 59 )
        , ( 60, 55 )
        , ( 60, 32 )
        , ( 60, 33 )
        , ( 55, 48 )
        , ( 55, 61 )
        , ( 55, 62 )
        , ( 55, 31 )
        , ( 55, 26 )
        , ( 55, 32 )
        , ( 55, 33 )
        , ( 55, 5 )
        , ( 55, 6 )
        , ( 62, 60 )
        , ( 62, 26 )
        , ( 62, 32 )
        , ( 62, 33 )
        , ( 62, 5 )
        , ( 62, 6 )
        , ( 62, 27 )
        , ( 62, 45 )
        , ( 31, 39 )
        , ( 31, 60 )
        , ( 31, 32 )
        , ( 31, 33 )
        , ( 31, 36 )
        , ( 26, 3 )
        , ( 26, 4 )
        , ( 26, 61 )
        , ( 26, 55 )
        , ( 26, 5 )
        , ( 26, 6 )
        , ( 26, 27 )
        , ( 32, 60 )
        , ( 32, 55 )
        , ( 32, 62 )
        , ( 32, 31 )
        , ( 32, 33 )
        , ( 32, 6 )
        , ( 32, 36 )
        , ( 33, 31 )
        , ( 33, 32 )
        , ( 33, 6 )
        , ( 33, 42 )
        , ( 33, 27 )
        , ( 5, 61 )
        , ( 5, 62 )
        , ( 5, 26 )
        , ( 5, 6 )
        , ( 5, 42 )
        , ( 5, 44 )
        , ( 5, 27 )
        , ( 5, 45 )
        , ( 6, 61 )
        , ( 6, 62 )
        , ( 6, 32 )
        , ( 6, 33 )
        , ( 6, 5 )
        , ( 64, 35 )
        , ( 64, 65 )
        , ( 64, 66 )
        , ( 64, 36 )
        , ( 64, 27 )
        , ( 64, 45 )
        , ( 63, 61 )
        , ( 63, 62 )
        , ( 63, 26 )
        , ( 63, 6 )
        , ( 63, 67 )
        , ( 63, 27 )
        , ( 63, 45 )
        , ( 68, 69 )
        , ( 68, 43 )
        , ( 58, 53 )
        , ( 58, 55 )
        , ( 58, 62 )
        , ( 58, 5 )
        , ( 58, 6 )
        , ( 58, 64 )
        , ( 58, 36 )
        , ( 41, 68 )
        , ( 41, 69 )
        , ( 41, 35 )
        , ( 41, 43 )
        , ( 41, 27 )
        , ( 41, 45 )
        , ( 69, 68 )
        , ( 69, 43 )
        , ( 69, 67 )
        , ( 35, 41 )
        , ( 35, 43 )
        , ( 35, 27 )
        , ( 42, 67 )
        , ( 42, 66 )
        , ( 42, 44 )
        , ( 42, 27 )
        , ( 42, 45 )
        , ( 65, 67 )
        , ( 65, 66 )
        , ( 65, 44 )
        , ( 65, 27 )
        , ( 65, 45 )
        , ( 43, 41 )
        , ( 43, 35 )
        , ( 43, 66 )
        , ( 43, 36 )
        , ( 43, 27 )
        , ( 43, 45 )
        , ( 67, 42 )
        , ( 67, 65 )
        , ( 67, 66 )
        , ( 67, 44 )
        , ( 67, 27 )
        , ( 67, 45 )
        , ( 66, 41 )
        , ( 66, 42 )
        , ( 66, 65 )
        , ( 66, 67 )
        , ( 66, 44 )
        , ( 66, 27 )
        , ( 66, 45 )
        , ( 36, 56 )
        , ( 36, 69 )
        , ( 36, 35 )
        , ( 36, 43 )
        , ( 36, 27 )
        , ( 36, 45 )
        , ( 44, 42 )
        , ( 44, 65 )
        , ( 44, 67 )
        , ( 44, 66 )
        , ( 44, 45 )
        , ( 27, 42 )
        , ( 27, 67 )
        , ( 27, 66 )
        , ( 27, 44 )
        , ( 27, 45 )
        , ( 45, 64 )
        , ( 45, 35 )
        , ( 45, 42 )
        , ( 45, 65 )
        , ( 45, 67 )
        , ( 45, 66 )
        , ( 45, 44 )
        , ( 45, 27 )
        ]



{- {"delay": 5} -}
