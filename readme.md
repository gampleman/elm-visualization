# Elm-Visualization

This project is designed to give you all the tools needed to build data visualizations.
It is not a charting library in the sense that you have pre-bundled Excel-style
charts, but it should contain all the tools to make building such charts relatively
easy. The advantage is that you are free to design and build data visualizations
that uniquely suite your needs.

[![PadAngle](http://code.gampleman.eu/elm-visualization/PadAngle/preview.png) ![Plus](http://code.gampleman.eu/elm-visualization/Cross/preview.png) ![LineChart](http://code.gampleman.eu/elm-visualization/LineChart/preview.png) ![LineChart](http://code.gampleman.eu/elm-visualization/Curves/preview.png)](http://code.gampleman.eu/elm-visualization/)

## What's included?

### [Scales](http://package.elm-lang.org/packages/gampleman/elm-visualization/latest/Visualization-Scale)

Most of the time you have data that has properties that you want to display on the
screen, however these properties typically aren't in pixels. Scales solve this \
fundamental problem by giving you convenient ways to transform raw data into positions,
sizes, colors, labels and other ways to display data.

### [Axis](http://package.elm-lang.org/packages/gampleman/elm-visualization/latest/Visualization-Axis)

A component that allows you to visualize a Scale. Those little ticks that describe
the dimensions of a plot.

### [Paths](http://package.elm-lang.org/packages/gampleman/elm-visualization/latest/Visualization-Path)

A thin layer that gives a simple DSL for drawing paths.

~~~elm
myPath = begin
       |> moveTo 10 30
       |> lineTo 40 50
       |> arcTo 20 30 12 43 (pi / 2)
       |> close
~~~

### [Shapes](http://package.elm-lang.org/packages/gampleman/elm-visualization/latest/Visualization-Shape)

This module gives you ways to draw some fundamental shapes used in data visualization.

Want to create a line chart with smooth curves?

```elm
path [d (Shape.line Shape.monotoneInXCurve dataPoints), stroke "2", fill "none"] []
```

Done.

### [Utilities](http://package.elm-lang.org/packages/gampleman/elm-visualization/latest/Visualization-List)

A bag of list processing methods that encapsulate common algorithms.

## Acknowledgements

Currently, it is almost a straight port of parts of the [D3](https://github.com/d3/d3) library
by [Mike Bostock](https://bost.ocks.org/mike/). However since Elm provides a
great DOM abstraction already, selections are not part of this library.

## Contributing

This library is still under active development, so please submit feature requests
iff you are also willing to implement them. Bug reports are welcome.

Some things worth working on:

- [ ] Scales have a number of stubs in them for other scale types.
- [ ] Shape could do with more line generators.
