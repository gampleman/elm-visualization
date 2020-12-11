# elm-visualization - An Introduction

By Jakub Hampl (@gampleman)

Welcome to elm-visualization! In this introduction I'll briefly explain the concepts behind elm-visualization, some of the prerequisite knowledge. I'll also point you to some learning resources.

The best way to understand elm-visualization is as a standard library for data visualization. It aims to provide all the basic tools you'll need to build any data visualization you might imagine. It is not a framework in the sense that it doesn't provide any structure for you to base your code on, nor is it merely a configurable collection of pre-made charts. It is also decidedly not very high level in its approach. This leads to a distinct set of properties:

1. You control and understand your visualization code. This means that you can have confidence that as requirements (invariably) change, you will not hit a wall where suddenly something is "impossible" because the library doesn't support it out of the box.
2. Simultaneously you have much more granular control over the performance characteristics of your code.
3. You are forced to make more explicit decisions about the graphical style and user affordances. You won't have features just because they were the defaults in the library.
4. Making decisions usually leads to deeper insight into the properties of the data and can lead to more meaningful designs.
5. Making a simple visualization will lead to a relatively large amount of code. Making an unusual or complex visualization won't increase that code that much.
6. Huge flexibility. elm-visualization focuses on composable primitives, so there is little limit on what you can do.

Given these properties, you should consider whether elm-visualization is the right tool for the job. I think it excels at building novel charts, at building charts for "production" where you understand the data being visualized and for data journalism/storytelling. In other words:

- If you are trying to do **expoloratory** visualization (i.e. you don't know much about the data), I would recommend either using a tool (like Excel or Tablaux) or using a Grammar of Graphics style library like [gicentre/elm-vegalite](https://package.elm-lang.org/packages/gicentre/elm-vegalite/latest/).
- If you need some specific **well known chart type**, you might have an easier time just using a pre-made package. See the [Elm Package Catalog Visualization Page](https://korban.net/elm/catalog/packages/data/visualisation) to see what's available.

## Prerequsites

Elm-visualization uses a low-level approach. Unfortunately, that means there is quite a lot to learn.

- Most importantly, elm-visualization does very little actual rendering for you. You will have to write your own drawing code. The easiest way to do that in today's Elm is to use SVG. Unfortunately the SVG spec is very large and documentation tends to be somewhat scattered. I hope to write an intro to it soon. In the meantime there is the #svg channel in the Elm slack for questions. For now, I would also recommend the [elm-community/typed-svg](https://elm.dmy.fr/packages/elm-community/typed-svg/latest/) over the [elm/svg](https://elm.dmy.fr/packages/elm/svg/latest/) package.
- Building data visualization requires an understanding of data concepts. Learn about basic statistical concepts, and get some experience with data cleaning and data preparation tasks.
- Finally, you should have a reasonable understanding of front-end programming in Elm. Start with the [guide](https://guide.elm-lang.org) if you don't.

You don't need to be an expert in these topics, but we'll assume basic familiarity with these things in this guide as well as in other elm-visualization material such as the API docs and the examples.

## Learning by Example

One of the main learning resources we provide is the [examples gallery](https://elm-visualization.netlify.app/). I encourage you to browse these. You can bootstrap your project by copy-pasting the example that looks the most like the thing that you are trying to build, then tweak it to do what you want. You can learn a lot by breaking and improving these examples. One you get a little more exprienced, reading the example code can reveal new and interesting techniques that you can apply to your own code.

On a technical note, these examples depend on a [variety of community packages](https://github.com/gampleman/elm-visualization/blob/master/examples/elm.json), so if you see compiler errors (especially about missing modules) when copying these examples, you might need to install some of these dependencies. Alternatively, you can start with [this Ellie](https://ellie-app.com/8592jsvBL2ka1) that should have the dependencies already added.

## Data

One of the first tasks you'll need to do is acquire data into your Elm program. Most of the examples simply hardcode the data as program literals, and indeed this can be a simple way to prototype. However, for many usecases you will likely want to load data from external sources. First you need to understand how you want to model and represent the data in your program, then you will need to write appropriate decoders from the wire format this data comes in. JSON is probably the easiest to work with (and the default elm/json package works very well for this), but CSV is also common (ericgj/elm-csv-decode and lovasoa/elm-csv are packages that can help here). If you are not hosting the data on your own server, than [CORS](https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS) might be something you need to look into.

A related task to decoding is cleaning: you need to decide on what to do with missing data, null data, invalid data and data that is out of bounds.

Once you have the cleaned and decoded data, elm-visualization provides the [`Statistics`](https://elm.dmy.fr/packages/gampleman/elm-visualization/latest/Statistics) module to help you understand the data in question:

```elm
data : List Float
data =
    [ 2, 4, 1, 4, 9 ]


Statistics.extent data --> Just ( 1, 9 )
```

## Scales

The most fundamental tool in all of elm-visualization, that you will use in nearly any data graphic, is the Scale.
