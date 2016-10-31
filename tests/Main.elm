port module Main exposing (..)

import Scale.Linear
import Scale.Quantize
import Axis
import Path
import Test
import Shape
import Test.Runner.Node exposing (run)
import Json.Encode exposing (Value)


main : Program Never
main =
    run emit <| Test.concat [ Scale.Linear.all, Scale.Quantize.all, Axis.all, Path.all, Shape.all ]


port emit : ( String, Value ) -> Cmd msg
