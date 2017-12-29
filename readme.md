<h1><img src="http://code.gampleman.eu/elm-visualization/misc/Logo@2x.png" alt="Elm-visualization" srcset="http://code.gampleman.eu/elm-visualization/misc/Logo@2x.png, http://code.gampleman.eu/elm-visualization/misc/Logo@2x.png 2x" style="max-width: 100%" /></h1>


[Docs](http://package.elm-lang.org/packages/gampleman/elm-visualization/latest/) | [Examples](http://code.gampleman.eu/elm-visualization/) | [GitHub](https://github.com/gampleman/elm-visualization) | [Changelog](https://github.com/gampleman/elm-visualization/releases) | `#visualization` on [Elm slack](https://elmlang.herokuapp.com)

This project is designed to give you all the tools needed to build data visualizations.
It is not a charting library in the sense that you have pre-bundled Excel-style
charts, but it should contain all the tools to make building such charts relatively
easy. The advantage is that you are free to design and build data visualizations
that uniquely suite your needs.

## Learn by example

<a href="http://code.gampleman.eu/elm-visualization/"><img src="http://code.gampleman.eu/elm-visualization/misc/examples@2x.png" alt="Examples" srcset="http://code.gampleman.eu/elm-visualization/misc/examples.png, http://code.gampleman.eu/elm-visualization/misc/examples@2x.png 2x" style="max-width: 100%" /></a>

## What's included?

### [Scales](http://package.elm-lang.org/packages/gampleman/elm-visualization/latest/Visualization-Scale)

Most of the time you have data that has properties that you want to display on the
screen, however these properties typically aren't in pixels. Scales solve this
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

This module gives you ways to draw some fundamental shapes used in data visualization, including lines (as in line or area charts),
as well as arcs (as in pie charts).

### [Force Layout](http://package.elm-lang.org/packages/gampleman/elm-visualization/latest/Visualization-Force)

Use a simulation of physical forces to do layout. Suitable for i.e. network graphs.

### [Histogram](http://package.elm-lang.org/packages/gampleman/elm-visualization/latest/Visualization-Histogram)

Compute histograms of data.

### [List Utilities](http://package.elm-lang.org/packages/gampleman/elm-visualization/latest/Visualization-List)

A bag of list processing methods that encapsulate common algorithms.

## Acknowledgements

Heavily inspired by parts of the [D3](https://github.com/d3/d3) library
by [Mike Bostock](https://bost.ocks.org/mike/). However since Elm provides a
great DOM abstraction already, selections are not part of this library.

## Contributing

This library is still under active development, so please submit feature requests
iff you are also willing to implement them. Bug reports are welcome.
