module Treemap exposing (main)

import Html
import Hierarchy exposing (Hierarchy(..))
import Hierarchy.Treemap
import List.Extra
import TypedSvg exposing (g, rect, svg)
import TypedSvg.Attributes exposing (class, fill, transform, viewBox, stroke)
import TypedSvg.Attributes.InPx exposing (height, width, x, y)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Paint(..), Transform(..))

w = 1152
h = 1152

main = svg [ viewBox 0 0 w h] tree

tree = Hierarchy.stratifyWithPath { path = \item -> String.split "." item.name |> List.Extra.uncons |> Maybe.withDefault ("flare", []), 
    createMissingNode = \(root, path) -> { name = String.join "." (root :: path), size = Nothing} } data
    |> Result.withDefault (Hierarchy {name = "flare", size = Nothing} [])
    |> (Hierarchy.mapWithContextBottomUp (\{node, children} ->
        {name = node.name 
        , size = case node.size of 
            Just s -> s
            Nothing -> List.sum (List.map (\(Hierarchy n _) -> n.size) children)
        }
    ))
    |> Hierarchy.Treemap.layout { paddingInner = always 1
    , paddingLeft = always 1
    , paddingRight= always 1
    , paddingTop = always 1
    , paddingBottom = always 1
    , tile = Hierarchy.Treemap.sliceDice
    , value = .size
    , dimensions = (w, h)
    }
    |> Hierarchy.leaves
    |> List.map (\item -> 
        rect [ x (item.x), y (item.y ), width item.width, height item.height ] [
                
            ]
    )


data =
    [   { name = "flare", size = Nothing }
    , { name = "flare.analytics", size = Nothing }
    , { name = "flare.analytics.cluster", size = Nothing }
    , { name = "flare.analytics.cluster.AgglomerativeCluster", size = Just 3938 }
    , { name = "flare.analytics.cluster.CommunityStructure", size = Just 3812 }
    , { name = "flare.analytics.cluster.HierarchicalCluster", size = Just 6714 }
    , { name = "flare.analytics.cluster.MergeEdge", size = Just 743 }
    , { name = "flare.analytics.graph", size = Nothing }
    , { name = "flare.analytics.graph.BetweennessCentrality", size = Just 3534 }
    , { name = "flare.analytics.graph.LinkDistance", size = Just 5731 }
    , { name = "flare.analytics.graph.MaxFlowMinCut", size = Just 7840 }
    , { name = "flare.analytics.graph.ShortestPaths", size = Just 5914 }
    , { name = "flare.analytics.graph.SpanningTree", size = Just 3416 }
    , { name = "flare.analytics.optimization", size = Nothing }
    , { name = "flare.analytics.optimization.AspectRatioBanker", size = Just 7074 }
    , { name = "flare.animate", size = Nothing }
    , { name = "flare.animate.Easing", size = Just 17010 }
    , { name = "flare.animate.FunctionSequence", size = Just 5842 }
    , { name = "flare.animate.interpolate", size = Nothing }
    , { name = "flare.animate.interpolate.ArrayInterpolator", size = Just 1983 }
    , { name = "flare.animate.interpolate.ColorInterpolator", size = Just 2047 }
    , { name = "flare.animate.interpolate.DateInterpolator", size = Just 1375 }
    , { name = "flare.animate.interpolate.Interpolator", size = Just 8746 }
    , { name = "flare.animate.interpolate.MatrixInterpolator", size = Just 2202 }
    , { name = "flare.animate.interpolate.NumberInterpolator", size = Just 1382 }
    , { name = "flare.animate.interpolate.ObjectInterpolator", size = Just 1629 }
    , { name = "flare.animate.interpolate.PointInterpolator", size = Just 1675 }
    , { name = "flare.animate.interpolate.RectangleInterpolator", size = Just 2042 }
    , { name = "flare.animate.ISchedulable", size = Just 1041 }
    , { name = "flare.animate.Parallel", size = Just 5176 }
    , { name = "flare.animate.Pause", size = Just 449 }
    , { name = "flare.animate.Scheduler", size = Just 5593 }
    , { name = "flare.animate.Sequence", size = Just 5534 }
    , { name = "flare.animate.Transition", size = Just 9201 }
    , { name = "flare.animate.Transitioner", size = Just 19975 }
    , { name = "flare.animate.TransitionEvent", size = Just 1116 }
    , { name = "flare.animate.Tween", size = Just 6006 }
    , { name = "flare.data", size = Nothing }
    , { name = "flare.data.converters", size = Nothing }
    , { name = "flare.data.converters.Converters", size = Just 721 }
    , { name = "flare.data.converters.DelimitedTextConverter", size = Just 4294 }
    , { name = "flare.data.converters.GraphMLConverter", size = Just 9800 }
    , { name = "flare.data.converters.IDataConverter", size = Just 1314 }
    , { name = "flare.data.converters.JSONConverter", size = Just 2220 }
    , { name = "flare.data.DataField", size = Just 1759 }
    , { name = "flare.data.DataSchema", size = Just 2165 }
    , { name = "flare.data.DataSet", size = Just 586 }
    , { name = "flare.data.DataSource", size = Just 3331 }
    , { name = "flare.data.DataTable", size = Just 772 }
    , { name = "flare.data.DataUtil", size = Just 3322 }
    , { name = "flare.display", size = Nothing }
    , { name = "flare.display.DirtySprite", size = Just 8833 }
    , { name = "flare.display.LineSprite", size = Just 1732 }
    , { name = "flare.display.RectSprite", size = Just 3623 }
    , { name = "flare.display.TextSprite", size = Just 10066 }
    , { name = "flare.flex", size = Nothing }
    , { name = "flare.flex.FlareVis", size = Just 4116 }
    , { name = "flare.physics", size = Nothing }
    , { name = "flare.physics.DragForce", size = Just 1082 }
    , { name = "flare.physics.GravityForce", size = Just 1336 }
    , { name = "flare.physics.IForce", size = Just 319 }
    , { name = "flare.physics.NBodyForce", size = Just 10498 }
    , { name = "flare.physics.Particle", size = Just 2822 }
    , { name = "flare.physics.Simulation", size = Just 9983 }
    , { name = "flare.physics.Spring", size = Just 2213 }
    , { name = "flare.physics.SpringForce", size = Just 1681 }
    , { name = "flare.query", size = Nothing }
    , { name = "flare.query.AggregateExpression", size = Just 1616 }
    , { name = "flare.query.And", size = Just 1027 }
    , { name = "flare.query.Arithmetic", size = Just 3891 }
    , { name = "flare.query.Average", size = Just 891 }
    , { name = "flare.query.BinaryExpression", size = Just 2893 }
    , { name = "flare.query.Comparison", size = Just 5103 }
    , { name = "flare.query.CompositeExpression", size = Just 3677 }
    , { name = "flare.query.Count", size = Just 781 }
    , { name = "flare.query.DateUtil", size = Just 4141 }
    , { name = "flare.query.Distinct", size = Just 933 }
    , { name = "flare.query.Expression", size = Just 5130 }
    , { name = "flare.query.ExpressionIterator", size = Just 3617 }
    , { name = "flare.query.Fn", size = Just 3240 }
    , { name = "flare.query.If", size = Just 2732 }
    , { name = "flare.query.IsA", size = Just 2039 }
    , { name = "flare.query.Literal", size = Just 1214 }
    , { name = "flare.query.Match", size = Just 3748 }
    , { name = "flare.query.Maximum", size = Just 843 }
    , { name = "flare.query.methods", size = Nothing }
    , { name = "flare.query.methods.add", size = Just 593 }
    , { name = "flare.query.methods.and", size = Just 330 }
    , { name = "flare.query.methods.average", size = Just 287 }
    , { name = "flare.query.methods.count", size = Just 277 }
    , { name = "flare.query.methods.distinct", size = Just 292 }
    , { name = "flare.query.methods.div", size = Just 595 }
    , { name = "flare.query.methods.eq", size = Just 594 }
    , { name = "flare.query.methods.fn", size = Just 460 }
    , { name = "flare.query.methods.gt", size = Just 603 }
    , { name = "flare.query.methods.gte", size = Just 625 }
    , { name = "flare.query.methods.iff", size = Just 748 }
    , { name = "flare.query.methods.isa", size = Just 461 }
    , { name = "flare.query.methods.lt", size = Just 597 }
    , { name = "flare.query.methods.lte", size = Just 619 }
    , { name = "flare.query.methods.max", size = Just 283 }
    , { name = "flare.query.methods.min", size = Just 283 }
    , { name = "flare.query.methods.mod", size = Just 591 }
    , { name = "flare.query.methods.mul", size = Just 603 }
    , { name = "flare.query.methods.neq", size = Just 599 }
    , { name = "flare.query.methods.not", size = Just 386 }
    , { name = "flare.query.methods.or", size = Just 323 }
    , { name = "flare.query.methods.orderby", size = Just 307 }
    , { name = "flare.query.methods.range", size = Just 772 }
    , { name = "flare.query.methods.select", size = Just 296 }
    , { name = "flare.query.methods.stddev", size = Just 363 }
    , { name = "flare.query.methods.sub", size = Just 600 }
    , { name = "flare.query.methods.sum", size = Just 280 }
    , { name = "flare.query.methods.update", size = Just 307 }
    , { name = "flare.query.methods.variance", size = Just 335 }
    , { name = "flare.query.methods.where", size = Just 299 }
    , { name = "flare.query.methods.xor", size = Just 354 }
    , { name = "flare.query.methods._", size = Just 264 }
    , { name = "flare.query.Minimum", size = Just 843 }
    , { name = "flare.query.Not", size = Just 1554 }
    , { name = "flare.query.Or", size = Just 970 }
    , { name = "flare.query.Query", size = Just 13896 }
    , { name = "flare.query.Range", size = Just 1594 }
    , { name = "flare.query.StringUtil", size = Just 4130 }
    , { name = "flare.query.Sum", size = Just 791 }
    , { name = "flare.query.Variable", size = Just 1124 }
    , { name = "flare.query.Variance", size = Just 1876 }
    , { name = "flare.query.Xor", size = Just 1101 }
    , { name = "flare.scale", size = Nothing }
    , { name = "flare.scale.IScaleMap", size = Just 2105 }
    , { name = "flare.scale.LinearScale", size = Just 1316 }
    , { name = "flare.scale.LogScale", size = Just 3151 }
    , { name = "flare.scale.OrdinalScale", size = Just 3770 }
    , { name = "flare.scale.QuantileScale", size = Just 2435 }
    , { name = "flare.scale.QuantitativeScale", size = Just 4839 }
    , { name = "flare.scale.RootScale", size = Just 1756 }
    , { name = "flare.scale.Scale", size = Just 4268 }
    , { name = "flare.scale.ScaleType", size = Just 1821 }
    , { name = "flare.scale.TimeScale", size = Just 5833 }
    , { name = "flare.util", size = Nothing }
    , { name = "flare.util.Arrays", size = Just 8258 }
    , { name = "flare.util.Colors", size = Just 10001 }
    , { name = "flare.util.Dates", size = Just 8217 }
    , { name = "flare.util.Displays", size = Just 12555 }
    , { name = "flare.util.Filter", size = Just 2324 }
    , { name = "flare.util.Geometry", size = Just 10993 }
    , { name = "flare.util.heap", size = Nothing }
    , { name = "flare.util.heap.FibonacciHeap", size = Just 9354 }
    , { name = "flare.util.heap.HeapNode", size = Just 1233 }
    , { name = "flare.util.IEvaluable", size = Just 335 }
    , { name = "flare.util.IPredicate", size = Just 383 }
    , { name = "flare.util.IValueProxy", size = Just 874 }
    , { name = "flare.util.math", size = Nothing }
    , { name = "flare.util.math.DenseMatrix", size = Just 3165 }
    , { name = "flare.util.math.IMatrix", size = Just 2815 }
    , { name = "flare.util.math.SparseMatrix", size = Just 3366 }
    , { name = "flare.util.Maths", size = Just 17705 }
    , { name = "flare.util.Orientation", size = Just 1486 }
    , { name = "flare.util.palette", size = Nothing }
    , { name = "flare.util.palette.ColorPalette", size = Just 6367 }
    , { name = "flare.util.palette.Palette", size = Just 1229 }
    , { name = "flare.util.palette.ShapePalette", size = Just 2059 }
    , { name = "flare.util.palette.SizePalette", size = Just 2291 }
    , { name = "flare.util.Property", size = Just 5559 }
    , { name = "flare.util.Shapes", size = Just 19118 }
    , { name = "flare.util.Sort", size = Just 6887 }
    , { name = "flare.util.Stats", size = Just 6557 }
    , { name = "flare.util.Strings", size = Just 22026 }
    , { name = "flare.vis", size = Nothing }
    , { name = "flare.vis.axis", size = Nothing }
    , { name = "flare.vis.axis.Axes", size = Just 1302 }
    , { name = "flare.vis.axis.Axis", size = Just 24593 }
    , { name = "flare.vis.axis.AxisGridLine", size = Just 652 }
    , { name = "flare.vis.axis.AxisLabel", size = Just 636 }
    , { name = "flare.vis.axis.CartesianAxes", size = Just 6703 }
    , { name = "flare.vis.controls", size = Nothing }
    , { name = "flare.vis.controls.AnchorControl", size = Just 2138 }
    , { name = "flare.vis.controls.ClickControl", size = Just 3824 }
    , { name = "flare.vis.controls.Control", size = Just 1353 }
    , { name = "flare.vis.controls.ControlList", size = Just 4665 }
    , { name = "flare.vis.controls.DragControl", size = Just 2649 }
    , { name = "flare.vis.controls.ExpandControl", size = Just 2832 }
    , { name = "flare.vis.controls.HoverControl", size = Just 4896 }
    , { name = "flare.vis.controls.IControl", size = Just 763 }
    , { name = "flare.vis.controls.PanZoomControl", size = Just 5222 }
    , { name = "flare.vis.controls.SelectionControl", size = Just 7862 }
    , { name = "flare.vis.controls.TooltipControl", size = Just 8435 }
    , { name = "flare.vis.data", size = Nothing }
    , { name = "flare.vis.data.Data", size = Just 20544 }
    , { name = "flare.vis.data.DataList", size = Just 19788 }
    , { name = "flare.vis.data.DataSprite", size = Just 10349 }
    , { name = "flare.vis.data.EdgeSprite", size = Just 3301 }
    , { name = "flare.vis.data.NodeSprite", size = Just 19382 }
    , { name = "flare.vis.data.render", size = Nothing }
    , { name = "flare.vis.data.render.ArrowType", size = Just 698 }
    , { name = "flare.vis.data.render.EdgeRenderer", size = Just 5569 }
    , { name = "flare.vis.data.render.IRenderer", size = Just 353 }
    , { name = "flare.vis.data.render.ShapeRenderer", size = Just 2247 }
    , { name = "flare.vis.data.ScaleBinding", size = Just 11275 }
    , { name = "flare.vis.data.Tree", size = Just 7147 }
    , { name = "flare.vis.data.TreeBuilder", size = Just 9930 }
    , { name = "flare.vis.events", size = Nothing }
    , { name = "flare.vis.events.DataEvent", size = Just 2313 }
    , { name = "flare.vis.events.SelectionEvent", size = Just 1880 }
    , { name = "flare.vis.events.TooltipEvent", size = Just 1701 }
    , { name = "flare.vis.events.VisualizationEvent", size = Just 1117 }
    , { name = "flare.vis.legend", size = Nothing }
    , { name = "flare.vis.legend.Legend", size = Just 20859 }
    , { name = "flare.vis.legend.LegendItem", size = Just 4614 }
    , { name = "flare.vis.legend.LegendRange", size = Just 10530 }
    , { name = "flare.vis.operator", size = Nothing }
    , { name = "flare.vis.operator.distortion", size = Nothing }
    , { name = "flare.vis.operator.distortion.BifocalDistortion", size = Just 4461 }
    , { name = "flare.vis.operator.distortion.Distortion", size = Just 6314 }
    , { name = "flare.vis.operator.distortion.FisheyeDistortion", size = Just 3444 }
    , { name = "flare.vis.operator.encoder", size = Nothing }
    , { name = "flare.vis.operator.encoder.ColorEncoder", size = Just 3179 }
    , { name = "flare.vis.operator.encoder.Encoder", size = Just 4060 }
    , { name = "flare.vis.operator.encoder.PropertyEncoder", size = Just 4138 }
    , { name = "flare.vis.operator.encoder.ShapeEncoder", size = Just 1690 }
    , { name = "flare.vis.operator.encoder.SizeEncoder", size = Just 1830 }
    , { name = "flare.vis.operator.filter", size = Nothing }
    , { name = "flare.vis.operator.filter.FisheyeTreeFilter", size = Just 5219 }
    , { name = "flare.vis.operator.filter.GraphDistanceFilter", size = Just 3165 }
    , { name = "flare.vis.operator.filter.VisibilityFilter", size = Just 3509 }
    , { name = "flare.vis.operator.IOperator", size = Just 1286 }
    , { name = "flare.vis.operator.label", size = Nothing }
    , { name = "flare.vis.operator.label.Labeler", size = Just 9956 }
    , { name = "flare.vis.operator.label.RadialLabeler", size = Just 3899 }
    , { name = "flare.vis.operator.label.StackedAreaLabeler", size = Just 3202 }
    , { name = "flare.vis.operator.layout", size = Nothing }
    , { name = "flare.vis.operator.layout.AxisLayout", size = Just 6725 }
    , { name = "flare.vis.operator.layout.BundledEdgeRouter", size = Just 3727 }
    , { name = "flare.vis.operator.layout.CircleLayout", size = Just 9317 }
    , { name = "flare.vis.operator.layout.CirclePackingLayout", size = Just 12003 }
    , { name = "flare.vis.operator.layout.DendrogramLayout", size = Just 4853 }
    , { name = "flare.vis.operator.layout.ForceDirectedLayout", size = Just 8411 }
    , { name = "flare.vis.operator.layout.IcicleTreeLayout", size = Just 4864 }
    , { name = "flare.vis.operator.layout.IndentedTreeLayout", size = Just 3174 }
    , { name = "flare.vis.operator.layout.Layout", size = Just 7881 }
    , { name = "flare.vis.operator.layout.NodeLinkTreeLayout", size = Just 12870 }
    , { name = "flare.vis.operator.layout.PieLayout", size = Just 2728 }
    , { name = "flare.vis.operator.layout.RadialTreeLayout", size = Just 12348 }
    , { name = "flare.vis.operator.layout.RandomLayout", size = Just 870 }
    , { name = "flare.vis.operator.layout.StackedAreaLayout", size = Just 9121 }
    , { name = "flare.vis.operator.layout.TreeMapLayout", size = Just 9191 }
    , { name = "flare.vis.operator.Operator", size = Just 2490 }
    , { name = "flare.vis.operator.OperatorList", size = Just 5248 }
    , { name = "flare.vis.operator.OperatorSequence", size = Just 4190 }
    , { name = "flare.vis.operator.OperatorSwitch", size = Just 2581 }
    , { name = "flare.vis.operator.SortOperator", size = Just 2023 }
    , { name = "flare.vis.Visualization", size = Just 16540 }
    ]
