port module Main exposing (..)

import Scale.Linear
import Scale.Quantize
import Axis
import Test
import Test.Runner.Node exposing (run)
import Json.Encode exposing (Value)


main : Program Never
main =
    run emit <| Test.concat [ Scale.Linear.all, Scale.Quantize.all, Axis.all ]


port emit : ( String, Value ) -> Cmd msg
