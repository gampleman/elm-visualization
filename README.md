# ![Elm-visualization](https://code.gampleman.eu/elm-visualization/misc/Logo-600.png)

[Tutorial](https://github.com/gampleman/elm-visualization/blob/master/docs/INTRO.md) | [Docs](https://package.elm-lang.org/packages/gampleman/elm-visualization/2.4.0/) | [Examples](https://elm-visualization.netlify.com/) | [GitHub](https://github.com/gampleman/elm-visualization) | [Changelog](https://github.com/gampleman/elm-visualization/releases) | `#visualization` on [Elm slack](https://elmlang.herokuapp.com)

This project is designed to give you all the tools needed to build data visualizations.
It is not a charting library in the sense that you have pre-bundled Excel-style
charts, but it should contain all the tools to make building such charts relatively
easy. The advantage is that you are free to design and build data visualizations
that uniquely suite your needs.

## Learn by [example](https://elm-visualization.netlify.com/)

[![Examples](https://code.gampleman.eu/elm-visualization/misc/examples-600.png)](https://elm-visualization.netlify.com/)

or [read the introduction](https://github.com/gampleman/elm-visualization/blob/master/docs/INTRO.md).

## Getting started

You will need to have [elm](https://elm-lang.org) installed. Then run:

```sh
elm init
elm install gampleman/elm-visualization
```

However, there are other packages that you will likely need to produce a visualization. Which depends somewhat on what you want to achieve, here are some common ones:

- [avh4/elm-color](https://package.elm-lang.org/packages/avh4/elm-color/latest) for the `Color` type
- [elm-community/typed-svg](https://package.elm-lang.org/packages/elm-community/typed-svg/latest) for rendering
- [folkertdev/one-true-path-experiment](https://package.elm-lang.org/packages/folkertdev/one-true-path-experiment/latest) for the `Path` type
- [gampleman/elm-rosetree](https://package.elm-lang.org/packages/gampleman/elm-rosetree/latest) for the `Tree` type

You can use [this Ellie](https://ellie-app.com/p6X5hXxcdRCa1) to run the examples, since it has all the dependencies already installed into it.

## What's included?

### [Scales](https://package.elm-lang.org/packages/gampleman/elm-visualization/2.4.0/Scale/)

Most of the time you have data that has properties that you want to display on the
screen, however these properties typically aren't in pixels. Scales solve this
fundamental problem by giving you convenient ways to transform raw data into positions,
sizes, colors, labels and other ways to display data.

### [Axis](https://package.elm-lang.org/packages/gampleman/elm-visualization/2.4.0/Axis/)

A component that allows you to visualize a Scale. Those little ticks that describe
the dimensions of a plot.

### [Shapes](https://package.elm-lang.org/packages/gampleman/elm-visualization/2.4.0/Shape/)

This module gives you ways to draw some fundamental shapes used in data visualization, including lines (as in line or area charts),
as well as arcs (as in pie charts).

### [Force Layout](https://package.elm-lang.org/packages/gampleman/elm-visualization/2.4.0/Force/)

Use a simulation of physical forces to do layout. Suitable for i.e. network graphs.

### [Hierarchy](https://package.elm-lang.org/packages/gampleman/elm-visualization/2.4.0/Hierarchy/)

Layout algorithms for dealing with trees.

### [Interpolation](https://package.elm-lang.org/packages/gampleman/elm-visualization/2.4.0/Interpolation/)

Smoothly transition between pairs of values. Useful for animation, or generating gradients of values.

### [Transition](https://package.elm-lang.org/packages/gampleman/elm-visualization/2.4.0/Transition/)

Build complex animations using Interpolation.

### [Histogram](https://package.elm-lang.org/packages/gampleman/elm-visualization/2.4.0/Histogram/)

Compute histograms of data.

### [Brush](https://package.elm-lang.org/packages/gampleman/elm-visualization/2.4.0/Brush/)

Interactively select subregions of a dataset.

### [Zoom](https://package.elm-lang.org/packages/gampleman/elm-visualization/2.4.0/Zoom/)

Build pan and zoom user interactions.

### [Statistics](https://package.elm-lang.org/packages/gampleman/elm-visualization/2.4.0/Statistics/)

Process data to extract useful insights for visualizations.

## Acknowledgements

Heavily inspired by parts of the [D3](https://github.com/d3/d3) library
by [Mike Bostock](https://bost.ocks.org/mike/). However since Elm provides a
great DOM abstraction already, selections are not part of this library.

## Contributing

This library is still under active development, so please submit feature requests
iff you are also willing to implement them. Bug reports are welcome.
