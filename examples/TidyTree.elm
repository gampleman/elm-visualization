module TidyTree exposing (main)

{-| Lays out a tree diagram using the Tidy algorithm. We scale the icons for the nodes proportionally to a quantitative dimension.

Since the Tree is quite dense and complex, we add a zoom behavior to allow the use to explore the leaf nodes more easily. Clicking on a non-leaf node zooms the tree to its subtree.

@category Advanced

-}

import Browser
import Color
import Curve
import Hierarchy
import List.Extra
import Path
import Shape
import Tree exposing (Tree)
import TypedSvg exposing (g, rect, svg)
import TypedSvg.Attributes exposing (dy, fill, href, id, pointerEvents, stroke, style, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (fontSize, height, width, x, y)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Events
import TypedSvg.Types exposing (AnchorAlignment(..), ClipPath(..), Paint(..), Transform(..), em)
import Zoom exposing (Zoom)


w : Float
w =
    990


h : Float
h =
    504


padding : Float
padding =
    30


layedOut : Tree Datum
layedOut =
    Hierarchy.tidy
        [ Hierarchy.nodeSize
            (\{ kind, size } ->
                case kind of
                    File ->
                        ( sqrt size / 1.1, sqrt size * 1.1 )

                    Directory ->
                        ( sqrt size * 1.05, sqrt size / 1.05 )
            )
        , Hierarchy.parentChildMargin 900
        , Hierarchy.peerMargin 60
        , Hierarchy.size (w - padding * 2) (h - padding * 2)
        ]
        tree
        |> Tree.sumUp
            (\n ->
                { x = n.x
                , y = n.y
                , width = n.width
                , height = n.height
                , node = n.node
                , bbox = { x = n.x, y = n.y, width = n.width, height = n.height }
                }
            )
            (\n c ->
                { x = n.x
                , y = n.y
                , width = n.width
                , height = n.height
                , node = n.node
                , bbox = List.foldl (\item t -> maxBBoxes item.bbox t) { x = n.x, y = n.y, width = n.width, height = n.height } c
                }
            )


maxBBoxes :
    { x : Float, y : Float, width : Float, height : Float }
    -> { x : Float, y : Float, width : Float, height : Float }
    -> { x : Float, y : Float, width : Float, height : Float }
maxBBoxes a b =
    let
        x =
            min a.x b.x

        y =
            min a.y b.y
    in
    { x = x
    , y = y
    , width = max (a.x + a.width) (b.x + b.width) - x
    , height = max (a.y + a.height) (b.y + b.height) - y
    }


type alias Datum =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    , node : { size : Float, name : String, kind : Kind }
    , bbox : { x : Float, y : Float, width : Float, height : Float }
    }


type Msg
    = ZoomMsg Zoom.OnZoom
    | Click Datum


main : Program () Zoom Msg
main =
    Browser.element
        { init =
            \() ->
                ( Zoom.init { width = w, height = h }
                    |> Zoom.scaleExtent 1 100
                    |> Zoom.translateExtent ( ( 0, 0 ), ( w, h ) )
                , Cmd.none
                )
        , update =
            \msg model ->
                case msg of
                    ZoomMsg m ->
                        ( Zoom.update m model, Cmd.none )

                    Click t ->
                        ( performZoom t model, Cmd.none )
        , subscriptions = \model -> Zoom.subscriptions model ZoomMsg
        , view = view
        }


performZoom : Datum -> Zoom -> Zoom
performZoom t =
    let
        p =
            ( t.x + t.width / 2, t.y + t.height / 2 )

        xScale =
            (w - 2 * padding) / t.bbox.width

        yScale =
            (h - 2 * padding) / t.bbox.height

        scale =
            min xScale yScale

        tx =
            (padding + t.bbox.x) * -scale

        ty =
            (padding + t.bbox.y) * -scale

        translate =
            if xScale > yScale then
                { x = w / 2 - scale * t.bbox.width / 2 + tx, y = padding + ty }

            else
                { x = padding + tx, y = h / 2 - scale * t.bbox.height / 2 + ty }
    in
    Zoom.setTransform (Zoom.animatedAround p) { scale = scale, translate = translate }


view : Zoom -> Svg Msg
view zoom =
    svg [ viewBox 0 0 w h ]
        [ rect
            (width w
                :: height h
                :: fill (Paint (Color.rgba 0 0 0 0))
                :: Zoom.events zoom ZoomMsg
            )
            []
        , g [ Zoom.transform zoom ]
            [ g [ transform [ Translate padding padding ] ]
                [ layedOut
                    |> Tree.links
                    |> List.map
                        (\( from, to ) ->
                            Shape.bumpYCurve
                                [ ( from.x + from.width / 2, from.y + from.height )
                                , ( to.x + to.width / 2, to.y + to.height * 0.1 )
                                ]
                        )
                    |> (\p ->
                            Path.element p
                                [ fill PaintNone
                                , stroke (Paint (Color.rgb 0.3 0.3 0.3))
                                , style "vector-effect: non-scaling-stroke"
                                , pointerEvents "none"
                                ]
                       )
                , layedOut
                    |> Tree.toList
                    |> List.map label
                    |> g []
                ]
            ]
        ]


label : Datum -> Svg Msg
label item =
    case item.node.kind of
        File ->
            let
                corner =
                    min item.width item.height * 0.2
            in
            g [ transform [ Translate item.x item.y ] ]
                [ Path.element
                    [ Curve.linearClosed
                        [ ( 0, item.height )
                        , ( 0, 0 )
                        , ( item.width - corner, 0 )
                        , ( item.width, corner )
                        , ( item.width, item.height )
                        ]
                    , Curve.linear
                        [ ( item.width - corner, 0 )
                        , ( item.width - corner, corner )
                        , ( item.width, corner )
                        ]
                    ]
                    [ fill (Paint (Color.rgb 1 1 0.8))
                    , stroke (Paint Color.black)
                    , style "vector-effect: non-scaling-stroke"
                    , id ("path-" ++ item.node.name)
                    ]
                , TypedSvg.title [] [ TypedSvg.Core.text (item.node.name ++ " - " ++ String.fromFloat item.node.size ++ " loc") ]
                , TypedSvg.clipPath [ id ("clip-" ++ item.node.name) ]
                    [ TypedSvg.use [ href ("#path-" ++ item.node.name) ] []
                    ]
                , item.node.name
                    |> String.split "."
                    |> List.reverse
                    |> List.head
                    |> Maybe.withDefault item.node.name
                    |> String.foldl
                        (\c ( word, soFar ) ->
                            if Char.isUpper c then
                                ( [ c ]
                                , case word of
                                    [] ->
                                        soFar

                                    _ ->
                                        String.fromList (List.reverse word) :: soFar
                                )

                            else
                                ( c :: word, soFar )
                        )
                        ( [], [] )
                    |> (\( word, soFar ) -> String.fromList (List.reverse word) :: soFar)
                    |> List.reverse
                    |> List.indexedMap
                        (\i st ->
                            TypedSvg.tspan
                                [ x (item.width * 0.05)
                                , TypedSvg.Attributes.y (em (2 + toFloat i * 0.9))
                                ]
                                [ TypedSvg.Core.text st ]
                        )
                    |> TypedSvg.text_
                        [ fontSize (sqrt item.node.size / 130)
                        , pointerEvents "none"
                        , TypedSvg.Attributes.clipPath (ClipPathFunc ("url(#clip-" ++ item.node.name ++ ")"))
                        ]
                ]

        Directory ->
            g []
                [ Path.element
                    [ Curve.linearClosed
                        [ ( item.x, item.y + item.height )
                        , ( item.x, item.y + item.height * 0.1 )
                        , ( item.x + item.width * 0.05, item.y )
                        , ( item.x + item.width * 0.4, item.y )
                        , ( item.x + item.width * 0.45, item.y + item.height * 0.1 )
                        , ( item.x + item.width, item.y + item.height * 0.1 )
                        , ( item.x + item.width, item.y + item.height )
                        ]
                    ]
                    [ fill (Paint (Color.rgb 0.8 0.9 1))
                    , stroke (Paint Color.black)
                    , style "vector-effect: non-scaling-stroke"
                    , TypedSvg.Events.onClick (Click item)
                    ]
                , TypedSvg.title [] [ TypedSvg.Core.text (item.node.name ++ " - " ++ String.fromFloat item.node.size ++ " loc") ]
                , TypedSvg.text_
                    [ textAnchor AnchorMiddle
                    , fontSize (sqrt item.node.size / 110)
                    , pointerEvents "none"
                    , x (item.x + item.width / 2)
                    , y (item.y + item.height / 2)
                    , dy (em 0.3)
                    ]
                    [ TypedSvg.Core.text (String.split "." item.node.name |> List.Extra.last |> Maybe.withDefault "") ]
                ]


type Kind
    = File
    | Directory


tree : Tree { name : String, kind : Kind, size : Float }
tree =
    Tree.stratifyWithPath
        { path = \item -> String.split "." item.name
        , createMissingNode = \path -> { name = String.join "." path, size = 0 }
        }
        data
        |> Result.withDefault (Tree.singleton { name = "flare", size = 0 })
        |> Tree.sumUp (\n -> { name = n.name, size = n.size, kind = File })
            (\node children ->
                { name = node.name, size = List.sum (List.map .size children), kind = Directory }
            )
        |> Tree.sortBy (Tree.label >> .name)


data : List { name : String, size : Float }
data =
    [ { name = "flare.analytics.cluster.AgglomerativeCluster", size = 3938 }
    , { name = "flare.analytics.cluster.CommunityStructure", size = 3812 }
    , { name = "flare.analytics.cluster.HierarchicalCluster", size = 6714 }
    , { name = "flare.analytics.cluster.MergeEdge", size = 743 }
    , { name = "flare.analytics.graph.BetweennessCentrality", size = 3534 }
    , { name = "flare.analytics.graph.LinkDistance", size = 5731 }
    , { name = "flare.analytics.graph.MaxFlowMinCut", size = 7840 }
    , { name = "flare.analytics.graph.ShortestPaths", size = 5914 }
    , { name = "flare.analytics.graph.SpanningTree", size = 3416 }
    , { name = "flare.analytics.optimization.AspectRatioBanker", size = 7074 }
    , { name = "flare.animate.Easing", size = 17010 }
    , { name = "flare.animate.FunctionSequence", size = 5842 }
    , { name = "flare.animate.interpolate.ArrayInterpolator", size = 1983 }
    , { name = "flare.animate.interpolate.ColorInterpolator", size = 2047 }
    , { name = "flare.animate.interpolate.DateInterpolator", size = 1375 }
    , { name = "flare.animate.interpolate.Interpolator", size = 8746 }
    , { name = "flare.animate.interpolate.MatrixInterpolator", size = 2202 }
    , { name = "flare.animate.interpolate.NumberInterpolator", size = 1382 }
    , { name = "flare.animate.interpolate.ObjectInterpolator", size = 1629 }
    , { name = "flare.animate.interpolate.PointInterpolator", size = 1675 }
    , { name = "flare.animate.interpolate.RectangleInterpolator", size = 2042 }
    , { name = "flare.animate.ISchedulable", size = 1041 }
    , { name = "flare.animate.Parallel", size = 5176 }
    , { name = "flare.animate.Pause", size = 449 }
    , { name = "flare.animate.Scheduler", size = 5593 }
    , { name = "flare.animate.Sequence", size = 5534 }
    , { name = "flare.animate.Transition", size = 9201 }
    , { name = "flare.animate.Transitioner", size = 19975 }
    , { name = "flare.animate.TransitionEvent", size = 1116 }
    , { name = "flare.animate.Tween", size = 6006 }
    , { name = "flare.data.converters.Converters", size = 721 }
    , { name = "flare.data.converters.DelimitedTextConverter", size = 4294 }
    , { name = "flare.data.converters.GraphMLConverter", size = 9800 }
    , { name = "flare.data.converters.IDataConverter", size = 1314 }
    , { name = "flare.data.converters.JSONConverter", size = 2220 }
    , { name = "flare.data.DataField", size = 1759 }
    , { name = "flare.data.DataSchema", size = 2165 }
    , { name = "flare.data.DataSet", size = 586 }
    , { name = "flare.data.DataSource", size = 3331 }
    , { name = "flare.data.DataTable", size = 772 }
    , { name = "flare.data.DataUtil", size = 3322 }
    , { name = "flare.display.DirtySprite", size = 8833 }
    , { name = "flare.display.LineSprite", size = 1732 }
    , { name = "flare.display.RectSprite", size = 3623 }
    , { name = "flare.display.TextSprite", size = 10066 }
    , { name = "flare.flex.FlareVis", size = 4116 }
    , { name = "flare.physics.DragForce", size = 1082 }
    , { name = "flare.physics.GravityForce", size = 1336 }
    , { name = "flare.physics.IForce", size = 319 }
    , { name = "flare.physics.NBodyForce", size = 10498 }
    , { name = "flare.physics.Particle", size = 2822 }
    , { name = "flare.physics.Simulation", size = 9983 }
    , { name = "flare.physics.Spring", size = 2213 }
    , { name = "flare.physics.SpringForce", size = 1681 }
    , { name = "flare.query.AggregateExpression", size = 1616 }
    , { name = "flare.query.And", size = 1027 }
    , { name = "flare.query.Arithmetic", size = 3891 }
    , { name = "flare.query.Average", size = 891 }
    , { name = "flare.query.BinaryExpression", size = 2893 }
    , { name = "flare.query.Comparison", size = 5103 }
    , { name = "flare.query.CompositeExpression", size = 3677 }
    , { name = "flare.query.Count", size = 781 }
    , { name = "flare.query.DateUtil", size = 4141 }
    , { name = "flare.query.Distinct", size = 933 }
    , { name = "flare.query.Expression", size = 5130 }
    , { name = "flare.query.ExpressionIterator", size = 3617 }
    , { name = "flare.query.Fn", size = 3240 }
    , { name = "flare.query.If", size = 2732 }
    , { name = "flare.query.IsA", size = 2039 }
    , { name = "flare.query.Literal", size = 1214 }
    , { name = "flare.query.Match", size = 3748 }
    , { name = "flare.query.Maximum", size = 843 }
    , { name = "flare.query.methods.add", size = 593 }
    , { name = "flare.query.methods.and", size = 330 }
    , { name = "flare.query.methods.average", size = 287 }
    , { name = "flare.query.methods.count", size = 277 }
    , { name = "flare.query.methods.distinct", size = 292 }
    , { name = "flare.query.methods.div", size = 595 }
    , { name = "flare.query.methods.eq", size = 594 }
    , { name = "flare.query.methods.fn", size = 460 }
    , { name = "flare.query.methods.gt", size = 603 }
    , { name = "flare.query.methods.gte", size = 625 }
    , { name = "flare.query.methods.iff", size = 748 }
    , { name = "flare.query.methods.isa", size = 461 }
    , { name = "flare.query.methods.lt", size = 597 }
    , { name = "flare.query.methods.lte", size = 619 }
    , { name = "flare.query.methods.max", size = 283 }
    , { name = "flare.query.methods.min", size = 283 }
    , { name = "flare.query.methods.mod", size = 591 }
    , { name = "flare.query.methods.mul", size = 603 }
    , { name = "flare.query.methods.neq", size = 599 }
    , { name = "flare.query.methods.not", size = 386 }
    , { name = "flare.query.methods.or", size = 323 }
    , { name = "flare.query.methods.orderby", size = 307 }
    , { name = "flare.query.methods.range", size = 772 }
    , { name = "flare.query.methods.select", size = 296 }
    , { name = "flare.query.methods.stddev", size = 363 }
    , { name = "flare.query.methods.sub", size = 600 }
    , { name = "flare.query.methods.sum", size = 280 }
    , { name = "flare.query.methods.update", size = 307 }
    , { name = "flare.query.methods.variance", size = 335 }
    , { name = "flare.query.methods.where", size = 299 }
    , { name = "flare.query.methods.xor", size = 354 }
    , { name = "flare.query.methods._", size = 264 }
    , { name = "flare.query.Minimum", size = 843 }
    , { name = "flare.query.Not", size = 1554 }
    , { name = "flare.query.Or", size = 970 }
    , { name = "flare.query.Query", size = 13896 }
    , { name = "flare.query.Range", size = 1594 }
    , { name = "flare.query.StringUtil", size = 4130 }
    , { name = "flare.query.Sum", size = 791 }
    , { name = "flare.query.Variable", size = 1124 }
    , { name = "flare.query.Variance", size = 1876 }
    , { name = "flare.query.Xor", size = 1101 }
    , { name = "flare.scale.IScaleMap", size = 2105 }
    , { name = "flare.scale.LinearScale", size = 1316 }
    , { name = "flare.scale.LogScale", size = 3151 }
    , { name = "flare.scale.OrdinalScale", size = 3770 }
    , { name = "flare.scale.QuantileScale", size = 2435 }
    , { name = "flare.scale.QuantitativeScale", size = 4839 }
    , { name = "flare.scale.RootScale", size = 1756 }
    , { name = "flare.scale.Scale", size = 4268 }
    , { name = "flare.scale.ScaleType", size = 1821 }
    , { name = "flare.scale.TimeScale", size = 5833 }
    , { name = "flare.util.Arrays", size = 8258 }
    , { name = "flare.util.Colors", size = 10001 }
    , { name = "flare.util.Dates", size = 8217 }
    , { name = "flare.util.Displays", size = 12555 }
    , { name = "flare.util.Filter", size = 2324 }
    , { name = "flare.util.Geometry", size = 10993 }
    , { name = "flare.util.heap.FibonacciHeap", size = 9354 }
    , { name = "flare.util.heap.HeapNode", size = 1233 }
    , { name = "flare.util.IEvaluable", size = 335 }
    , { name = "flare.util.IPredicate", size = 383 }
    , { name = "flare.util.IValueProxy", size = 874 }
    , { name = "flare.util.math.DenseMatrix", size = 3165 }
    , { name = "flare.util.math.IMatrix", size = 2815 }
    , { name = "flare.util.math.SparseMatrix", size = 3366 }
    , { name = "flare.util.Maths", size = 17705 }
    , { name = "flare.util.Orientation", size = 1486 }
    , { name = "flare.util.palette.ColorPalette", size = 6367 }
    , { name = "flare.util.palette.Palette", size = 1229 }
    , { name = "flare.util.palette.ShapePalette", size = 2059 }
    , { name = "flare.util.palette.SizePalette", size = 2291 }
    , { name = "flare.util.Property", size = 5559 }
    , { name = "flare.util.Shapes", size = 19118 }
    , { name = "flare.util.Sort", size = 6887 }
    , { name = "flare.util.Stats", size = 6557 }
    , { name = "flare.util.Strings", size = 22026 }
    , { name = "flare.vis.axis.Axes", size = 1302 }
    , { name = "flare.vis.axis.Axis", size = 24593 }
    , { name = "flare.vis.axis.AxisGridLine", size = 652 }
    , { name = "flare.vis.axis.AxisLabel", size = 636 }
    , { name = "flare.vis.axis.CartesianAxes", size = 6703 }
    , { name = "flare.vis.controls.AnchorControl", size = 2138 }
    , { name = "flare.vis.controls.ClickControl", size = 3824 }
    , { name = "flare.vis.controls.Control", size = 1353 }
    , { name = "flare.vis.controls.ControlList", size = 4665 }
    , { name = "flare.vis.controls.DragControl", size = 2649 }
    , { name = "flare.vis.controls.ExpandControl", size = 2832 }
    , { name = "flare.vis.controls.HoverControl", size = 4896 }
    , { name = "flare.vis.controls.IControl", size = 763 }
    , { name = "flare.vis.controls.PanZoomControl", size = 5222 }
    , { name = "flare.vis.controls.SelectionControl", size = 7862 }
    , { name = "flare.vis.controls.TooltipControl", size = 8435 }
    , { name = "flare.vis.data.Data", size = 20544 }
    , { name = "flare.vis.data.DataList", size = 19788 }
    , { name = "flare.vis.data.DataSprite", size = 10349 }
    , { name = "flare.vis.data.EdgeSprite", size = 3301 }
    , { name = "flare.vis.data.NodeSprite", size = 19382 }
    , { name = "flare.vis.data.render.ArrowType", size = 698 }
    , { name = "flare.vis.data.render.EdgeRenderer", size = 5569 }
    , { name = "flare.vis.data.render.IRenderer", size = 353 }
    , { name = "flare.vis.data.render.ShapeRenderer", size = 2247 }
    , { name = "flare.vis.data.ScaleBinding", size = 11275 }
    , { name = "flare.vis.data.Tree", size = 7147 }
    , { name = "flare.vis.data.TreeBuilder", size = 9930 }
    , { name = "flare.vis.events.DataEvent", size = 2313 }
    , { name = "flare.vis.events.SelectionEvent", size = 1880 }
    , { name = "flare.vis.events.TooltipEvent", size = 1701 }
    , { name = "flare.vis.events.VisualizationEvent", size = 1117 }
    , { name = "flare.vis.legend.Legend", size = 20859 }
    , { name = "flare.vis.legend.LegendItem", size = 4614 }
    , { name = "flare.vis.legend.LegendRange", size = 10530 }
    , { name = "flare.vis.operator.distortion.BifocalDistortion", size = 4461 }
    , { name = "flare.vis.operator.distortion.Distortion", size = 6314 }
    , { name = "flare.vis.operator.distortion.FisheyeDistortion", size = 3444 }
    , { name = "flare.vis.operator.encoder.ColorEncoder", size = 3179 }
    , { name = "flare.vis.operator.encoder.Encoder", size = 4060 }
    , { name = "flare.vis.operator.encoder.PropertyEncoder", size = 4138 }
    , { name = "flare.vis.operator.encoder.ShapeEncoder", size = 1690 }
    , { name = "flare.vis.operator.encoder.SizeEncoder", size = 1830 }
    , { name = "flare.vis.operator.filter.FisheyeTreeFilter", size = 5219 }
    , { name = "flare.vis.operator.filter.GraphDistanceFilter", size = 3165 }
    , { name = "flare.vis.operator.filter.VisibilityFilter", size = 3509 }
    , { name = "flare.vis.operator.IOperator", size = 1286 }
    , { name = "flare.vis.operator.label.Labeler", size = 9956 }
    , { name = "flare.vis.operator.label.RadialLabeler", size = 3899 }
    , { name = "flare.vis.operator.label.StackedAreaLabeler", size = 3202 }
    , { name = "flare.vis.operator.layout.AxisLayout", size = 6725 }
    , { name = "flare.vis.operator.layout.BundledEdgeRouter", size = 3727 }
    , { name = "flare.vis.operator.layout.CircleLayout", size = 9317 }
    , { name = "flare.vis.operator.layout.CirclePackingLayout", size = 12003 }
    , { name = "flare.vis.operator.layout.DendrogramLayout", size = 4853 }
    , { name = "flare.vis.operator.layout.ForceDirectedLayout", size = 8411 }
    , { name = "flare.vis.operator.layout.IcicleTreeLayout", size = 4864 }
    , { name = "flare.vis.operator.layout.IndentedTreeLayout", size = 3174 }
    , { name = "flare.vis.operator.layout.Layout", size = 7881 }
    , { name = "flare.vis.operator.layout.NodeLinkTreeLayout", size = 12870 }
    , { name = "flare.vis.operator.layout.PieLayout", size = 2728 }
    , { name = "flare.vis.operator.layout.RadialTreeLayout", size = 12348 }
    , { name = "flare.vis.operator.layout.RandomLayout", size = 870 }
    , { name = "flare.vis.operator.layout.StackedAreaLayout", size = 9121 }
    , { name = "flare.vis.operator.layout.TreeMapLayout", size = 9191 }
    , { name = "flare.vis.operator.Operator", size = 2490 }
    , { name = "flare.vis.operator.OperatorList", size = 5248 }
    , { name = "flare.vis.operator.OperatorSequence", size = 4190 }
    , { name = "flare.vis.operator.OperatorSwitch", size = 2581 }
    , { name = "flare.vis.operator.SortOperator", size = 2023 }
    , { name = "flare.vis.Visualization", size = 16540 }
    ]
