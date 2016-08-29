module Visualization exposing (..)

import Html.App as App
import Html
import Visualization.Scale.Continuous as Scale
import Date

model = 0
update model msg =
  model
--1469219417323
scale = Scale.time (1420070400000, 1420070430000) (0, 100)

test = List.map (Scale.tickFormat scale 10) (Scale.ticks scale 10)

view model = Html.text <| toString <| test

main = App.beginnerProgram
  {
    model = model,
    update = update,
    view = view
  }
