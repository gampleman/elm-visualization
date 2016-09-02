# Elm-Visualization

This project is designed to give you all the tools needed to build data visualizations.

Currently, it is almost a straight port of parts of the [D3](https://github.com/d3/d3) library
by [Mike Bostock](https://bost.ocks.org/mike/). However since Elm provides a
great DOM abstraction already, selections are not part of this library.

Furthermore, this is currently a work in progress. I am likely to refactor the
code to be more idiomatically Elmish and move away from its D3 heritage. I am releasing
it currently, since I believe that the algorithms already implemented are very
useful in many tasks and can even be used to build i.e. higher-level charting libraries.
