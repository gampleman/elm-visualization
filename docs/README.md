### The examples website

Stuff in this folder is used to generate the Examples website. We use [elm-example-publisher](https://github.com/gampleman/elm-example-publisher) to build the website.

The Docs.elm file is the Elm program that actually generates the HTML. You can create a local version of the website by going to the root of the directory and running:

```sh
# Install dependencies
npm install

# Build website
npm run build-docs
```
