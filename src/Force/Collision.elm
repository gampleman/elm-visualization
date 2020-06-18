module Force.Collision exposing (wrapper)

import Dict exposing (Dict)
import Force.QuadTree as QuadTree exposing (QuadTree)


wrapper alpha iters collisionParams entities =
    entities
