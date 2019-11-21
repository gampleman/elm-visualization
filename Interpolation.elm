module Interpolation exposing (..)

type Interpolation a =
  Interpolation (a -> a ->Float ->  a)

bimap : (a -> b) -> (b -> a) -> Interpolation a -> Interpolation b
bimap fn parser (Interpolation interpolator) =
  Interpolation (\from to param ->  fn (interpolator (parser from) (parser to) param) )
