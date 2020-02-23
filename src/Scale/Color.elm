module Scale.Color exposing
    ( category10, tableau10
    , viridisInterpolator, infernoInterpolator, magmaInterpolator, plasmaInterpolator
    , cielabInterpolator, hsluvInterpolator, rgbaToRgb255
    )

{-| We provide sequential and categorical color schemes designed to work with [ordinal](Scale#OrdinalScale) and [sequential](Scale#SequentialScale) scales. Color types come from [avh4/elm-color](https://package.elm-lang.org/packages/avh4/elm-color/latest/).


# Categorical

@docs category10, tableau10


# Sequential (Multi-hue)

@docs viridisInterpolator, infernoInterpolator, magmaInterpolator, plasmaInterpolator

-}

import Array exposing (Array)
import Color exposing (Color, black, rgb255, toRgba)
import HSLuv
import Scale


mkInterpolator : Array Color -> Float -> Color
mkInterpolator range =
    let
        n =
            Array.length range
    in
    \t ->
        Maybe.withDefault black <| Array.get (max 0 (min (n - 1) (floor (t * toFloat n)))) range


{-| ![Viridis](https://code.gampleman.eu/elm-visualization/misc/viridis.png)

Given a number t in the range [0,1], returns the corresponding
color from the “viridis” perceptually-uniform color scheme designed
by [van der Walt, Smith and Firing](https://bids.github.io/colormap/)
for matplotlib.

-}
viridisInterpolator : Float -> Color
viridisInterpolator =
    mkInterpolator <|
        Array.fromList
            [ rgb255 68 1 84, rgb255 68 2 86, rgb255 69 4 87, rgb255 69 5 89, rgb255 70 7 90, rgb255 70 8 92, rgb255 70 10 93, rgb255 70 11 94, rgb255 71 13 96, rgb255 71 14 97, rgb255 71 16 99, rgb255 71 17 100, rgb255 71 19 101, rgb255 72 20 103, rgb255 72 22 104, rgb255 72 23 105, rgb255 72 24 106, rgb255 72 26 108, rgb255 72 27 109, rgb255 72 28 110, rgb255 72 29 111, rgb255 72 31 112, rgb255 72 32 113, rgb255 72 33 115, rgb255 72 35 116, rgb255 72 36 117, rgb255 72 37 118, rgb255 72 38 119, rgb255 72 40 120, rgb255 72 41 121, rgb255 71 42 122, rgb255 71 44 122, rgb255 71 45 123, rgb255 71 46 124, rgb255 71 47 125, rgb255 70 48 126, rgb255 70 50 126, rgb255 70 51 127, rgb255 70 52 128, rgb255 69 53 129, rgb255 69 55 129, rgb255 69 56 130, rgb255 68 57 131, rgb255 68 58 131, rgb255 68 59 132, rgb255 67 61 132, rgb255 67 62 133, rgb255 66 63 133, rgb255 66 64 134, rgb255 66 65 134, rgb255 65 66 135, rgb255 65 68 135, rgb255 64 69 136, rgb255 64 70 136, rgb255 63 71 136, rgb255 63 72 137, rgb255 62 73 137, rgb255 62 74 137, rgb255 62 76 138, rgb255 61 77 138, rgb255 61 78 138, rgb255 60 79 138, rgb255 60 80 139, rgb255 59 81 139, rgb255 59 82 139, rgb255 58 83 139, rgb255 58 84 140, rgb255 57 85 140, rgb255 57 86 140, rgb255 56 88 140, rgb255 56 89 140, rgb255 55 90 140, rgb255 55 91 141, rgb255 54 92 141, rgb255 54 93 141, rgb255 53 94 141, rgb255 53 95 141, rgb255 52 96 141, rgb255 52 97 141, rgb255 51 98 141, rgb255 51 99 141, rgb255 50 100 142, rgb255 50 101 142, rgb255 49 102 142, rgb255 49 103 142, rgb255 49 104 142, rgb255 48 105 142, rgb255 48 106 142, rgb255 47 107 142, rgb255 47 108 142, rgb255 46 109 142, rgb255 46 110 142, rgb255 46 111 142, rgb255 45 112 142, rgb255 45 113 142, rgb255 44 113 142, rgb255 44 114 142, rgb255 44 115 142, rgb255 43 116 142, rgb255 43 117 142, rgb255 42 118 142, rgb255 42 119 142, rgb255 42 120 142, rgb255 41 121 142, rgb255 41 122 142, rgb255 41 123 142, rgb255 40 124 142, rgb255 40 125 142, rgb255 39 126 142, rgb255 39 127 142, rgb255 39 128 142, rgb255 38 129 142, rgb255 38 130 142, rgb255 38 130 142, rgb255 37 131 142, rgb255 37 132 142, rgb255 37 133 142, rgb255 36 134 142, rgb255 36 135 142, rgb255 35 136 142, rgb255 35 137 142, rgb255 35 138 141, rgb255 34 139 141, rgb255 34 140 141, rgb255 34 141 141, rgb255 33 142 141, rgb255 33 143 141, rgb255 33 144 141, rgb255 33 145 140, rgb255 32 146 140, rgb255 32 146 140, rgb255 32 147 140, rgb255 31 148 140, rgb255 31 149 139, rgb255 31 150 139, rgb255 31 151 139, rgb255 31 152 139, rgb255 31 153 138, rgb255 31 154 138, rgb255 30 155 138, rgb255 30 156 137, rgb255 30 157 137, rgb255 31 158 137, rgb255 31 159 136, rgb255 31 160 136, rgb255 31 161 136, rgb255 31 161 135, rgb255 31 162 135, rgb255 32 163 134, rgb255 32 164 134, rgb255 33 165 133, rgb255 33 166 133, rgb255 34 167 133, rgb255 34 168 132, rgb255 35 169 131, rgb255 36 170 131, rgb255 37 171 130, rgb255 37 172 130, rgb255 38 173 129, rgb255 39 173 129, rgb255 40 174 128, rgb255 41 175 127, rgb255 42 176 127, rgb255 44 177 126, rgb255 45 178 125, rgb255 46 179 124, rgb255 47 180 124, rgb255 49 181 123, rgb255 50 182 122, rgb255 52 182 121, rgb255 53 183 121, rgb255 55 184 120, rgb255 56 185 119, rgb255 58 186 118, rgb255 59 187 117, rgb255 61 188 116, rgb255 63 188 115, rgb255 64 189 114, rgb255 66 190 113, rgb255 68 191 112, rgb255 70 192 111, rgb255 72 193 110, rgb255 74 193 109, rgb255 76 194 108, rgb255 78 195 107, rgb255 80 196 106, rgb255 82 197 105, rgb255 84 197 104, rgb255 86 198 103, rgb255 88 199 101, rgb255 90 200 100, rgb255 92 200 99, rgb255 94 201 98, rgb255 96 202 96, rgb255 99 203 95, rgb255 101 203 94, rgb255 103 204 92, rgb255 105 205 91, rgb255 108 205 90, rgb255 110 206 88, rgb255 112 207 87, rgb255 115 208 86, rgb255 117 208 84, rgb255 119 209 83, rgb255 122 209 81, rgb255 124 210 80, rgb255 127 211 78, rgb255 129 211 77, rgb255 132 212 75, rgb255 134 213 73, rgb255 137 213 72, rgb255 139 214 70, rgb255 142 214 69, rgb255 144 215 67, rgb255 147 215 65, rgb255 149 216 64, rgb255 152 216 62, rgb255 155 217 60, rgb255 157 217 59, rgb255 160 218 57, rgb255 162 218 55, rgb255 165 219 54, rgb255 168 219 52, rgb255 170 220 50, rgb255 173 220 48, rgb255 176 221 47, rgb255 178 221 45, rgb255 181 222 43, rgb255 184 222 41, rgb255 186 222 40, rgb255 189 223 38, rgb255 192 223 37, rgb255 194 223 35, rgb255 197 224 33, rgb255 200 224 32, rgb255 202 225 31, rgb255 205 225 29, rgb255 208 225 28, rgb255 210 226 27, rgb255 213 226 26, rgb255 216 226 25, rgb255 218 227 25, rgb255 221 227 24, rgb255 223 227 24, rgb255 226 228 24, rgb255 229 228 25, rgb255 231 228 25, rgb255 234 229 26, rgb255 236 229 27, rgb255 239 229 28, rgb255 241 229 29, rgb255 244 230 30, rgb255 246 230 32, rgb255 248 230 33, rgb255 251 231 35, rgb255 253 231 37 ]


{-| ![magma](https://code.gampleman.eu/elm-visualization/misc/magma.png)

Given a number t in the range [0,1], returns the corresponding
color from the “magma” perceptually-uniform color scheme designed
by [van der Walt, Smith and Firing](https://bids.github.io/colormap/)
for matplotlib,.

-}
magmaInterpolator : Float -> Color
magmaInterpolator =
    mkInterpolator <|
        Array.fromList [ rgb255 0 0 4, rgb255 1 0 5, rgb255 1 1 6, rgb255 1 1 8, rgb255 2 1 9, rgb255 2 2 11, rgb255 2 2 13, rgb255 3 3 15, rgb255 3 3 18, rgb255 4 4 20, rgb255 5 4 22, rgb255 6 5 24, rgb255 6 5 26, rgb255 7 6 28, rgb255 8 7 30, rgb255 9 7 32, rgb255 10 8 34, rgb255 11 9 36, rgb255 12 9 38, rgb255 13 10 41, rgb255 14 11 43, rgb255 16 11 45, rgb255 17 12 47, rgb255 18 13 49, rgb255 19 13 52, rgb255 20 14 54, rgb255 21 14 56, rgb255 22 15 59, rgb255 24 15 61, rgb255 25 16 63, rgb255 26 16 66, rgb255 28 16 68, rgb255 29 17 71, rgb255 30 17 73, rgb255 32 17 75, rgb255 33 17 78, rgb255 34 17 80, rgb255 36 18 83, rgb255 37 18 85, rgb255 39 18 88, rgb255 41 17 90, rgb255 42 17 92, rgb255 44 17 95, rgb255 45 17 97, rgb255 47 17 99, rgb255 49 17 101, rgb255 51 16 103, rgb255 52 16 105, rgb255 54 16 107, rgb255 56 16 108, rgb255 57 15 110, rgb255 59 15 112, rgb255 61 15 113, rgb255 63 15 114, rgb255 64 15 116, rgb255 66 15 117, rgb255 68 15 118, rgb255 69 16 119, rgb255 71 16 120, rgb255 73 16 120, rgb255 74 16 121, rgb255 76 17 122, rgb255 78 17 123, rgb255 79 18 123, rgb255 81 18 124, rgb255 82 19 124, rgb255 84 19 125, rgb255 86 20 125, rgb255 87 21 126, rgb255 89 21 126, rgb255 90 22 126, rgb255 92 22 127, rgb255 93 23 127, rgb255 95 24 127, rgb255 96 24 128, rgb255 98 25 128, rgb255 100 26 128, rgb255 101 26 128, rgb255 103 27 128, rgb255 104 28 129, rgb255 106 28 129, rgb255 107 29 129, rgb255 109 29 129, rgb255 110 30 129, rgb255 112 31 129, rgb255 114 31 129, rgb255 115 32 129, rgb255 117 33 129, rgb255 118 33 129, rgb255 120 34 129, rgb255 121 34 130, rgb255 123 35 130, rgb255 124 35 130, rgb255 126 36 130, rgb255 128 37 130, rgb255 129 37 129, rgb255 131 38 129, rgb255 132 38 129, rgb255 134 39 129, rgb255 136 39 129, rgb255 137 40 129, rgb255 139 41 129, rgb255 140 41 129, rgb255 142 42 129, rgb255 144 42 129, rgb255 145 43 129, rgb255 147 43 128, rgb255 148 44 128, rgb255 150 44 128, rgb255 152 45 128, rgb255 153 45 128, rgb255 155 46 127, rgb255 156 46 127, rgb255 158 47 127, rgb255 160 47 127, rgb255 161 48 126, rgb255 163 48 126, rgb255 165 49 126, rgb255 166 49 125, rgb255 168 50 125, rgb255 170 51 125, rgb255 171 51 124, rgb255 173 52 124, rgb255 174 52 123, rgb255 176 53 123, rgb255 178 53 123, rgb255 179 54 122, rgb255 181 54 122, rgb255 183 55 121, rgb255 184 55 121, rgb255 186 56 120, rgb255 188 57 120, rgb255 189 57 119, rgb255 191 58 119, rgb255 192 58 118, rgb255 194 59 117, rgb255 196 60 117, rgb255 197 60 116, rgb255 199 61 115, rgb255 200 62 115, rgb255 202 62 114, rgb255 204 63 113, rgb255 205 64 113, rgb255 207 64 112, rgb255 208 65 111, rgb255 210 66 111, rgb255 211 67 110, rgb255 213 68 109, rgb255 214 69 108, rgb255 216 69 108, rgb255 217 70 107, rgb255 219 71 106, rgb255 220 72 105, rgb255 222 73 104, rgb255 223 74 104, rgb255 224 76 103, rgb255 226 77 102, rgb255 227 78 101, rgb255 228 79 100, rgb255 229 80 100, rgb255 231 82 99, rgb255 232 83 98, rgb255 233 84 98, rgb255 234 86 97, rgb255 235 87 96, rgb255 236 88 96, rgb255 237 90 95, rgb255 238 91 94, rgb255 239 93 94, rgb255 240 95 94, rgb255 241 96 93, rgb255 242 98 93, rgb255 242 100 92, rgb255 243 101 92, rgb255 244 103 92, rgb255 244 105 92, rgb255 245 107 92, rgb255 246 108 92, rgb255 246 110 92, rgb255 247 112 92, rgb255 247 114 92, rgb255 248 116 92, rgb255 248 118 92, rgb255 249 120 93, rgb255 249 121 93, rgb255 249 123 93, rgb255 250 125 94, rgb255 250 127 94, rgb255 250 129 95, rgb255 251 131 95, rgb255 251 133 96, rgb255 251 135 97, rgb255 252 137 97, rgb255 252 138 98, rgb255 252 140 99, rgb255 252 142 100, rgb255 252 144 101, rgb255 253 146 102, rgb255 253 148 103, rgb255 253 150 104, rgb255 253 152 105, rgb255 253 154 106, rgb255 253 155 107, rgb255 254 157 108, rgb255 254 159 109, rgb255 254 161 110, rgb255 254 163 111, rgb255 254 165 113, rgb255 254 167 114, rgb255 254 169 115, rgb255 254 170 116, rgb255 254 172 118, rgb255 254 174 119, rgb255 254 176 120, rgb255 254 178 122, rgb255 254 180 123, rgb255 254 182 124, rgb255 254 183 126, rgb255 254 185 127, rgb255 254 187 129, rgb255 254 189 130, rgb255 254 191 132, rgb255 254 193 133, rgb255 254 194 135, rgb255 254 196 136, rgb255 254 198 138, rgb255 254 200 140, rgb255 254 202 141, rgb255 254 204 143, rgb255 254 205 144, rgb255 254 207 146, rgb255 254 209 148, rgb255 254 211 149, rgb255 254 213 151, rgb255 254 215 153, rgb255 254 216 154, rgb255 253 218 156, rgb255 253 220 158, rgb255 253 222 160, rgb255 253 224 161, rgb255 253 226 163, rgb255 253 227 165, rgb255 253 229 167, rgb255 253 231 169, rgb255 253 233 170, rgb255 253 235 172, rgb255 252 236 174, rgb255 252 238 176, rgb255 252 240 178, rgb255 252 242 180, rgb255 252 244 182, rgb255 252 246 184, rgb255 252 247 185, rgb255 252 249 187, rgb255 252 251 189, rgb255 252 253 191 ]


{-| ![Inferno](https://code.gampleman.eu/elm-visualization/misc/inferno.png)

Given a number t in the range [0,1], returns the corresponding
color from the “inferno” perceptually-uniform color scheme designed
by [van der Walt, Smith and Firing](https://bids.github.io/colormap/)
for matplotlib.

-}
infernoInterpolator : Float -> Color
infernoInterpolator =
    mkInterpolator <|
        Array.fromList [ rgb255 0 0 4, rgb255 1 0 5, rgb255 1 1 6, rgb255 1 1 8, rgb255 2 1 10, rgb255 2 2 12, rgb255 2 2 14, rgb255 3 2 16, rgb255 4 3 18, rgb255 4 3 20, rgb255 5 4 23, rgb255 6 4 25, rgb255 7 5 27, rgb255 8 5 29, rgb255 9 6 31, rgb255 10 7 34, rgb255 11 7 36, rgb255 12 8 38, rgb255 13 8 41, rgb255 14 9 43, rgb255 16 9 45, rgb255 17 10 48, rgb255 18 10 50, rgb255 20 11 52, rgb255 21 11 55, rgb255 22 11 57, rgb255 24 12 60, rgb255 25 12 62, rgb255 27 12 65, rgb255 28 12 67, rgb255 30 12 69, rgb255 31 12 72, rgb255 33 12 74, rgb255 35 12 76, rgb255 36 12 79, rgb255 38 12 81, rgb255 40 11 83, rgb255 41 11 85, rgb255 43 11 87, rgb255 45 11 89, rgb255 47 10 91, rgb255 49 10 92, rgb255 50 10 94, rgb255 52 10 95, rgb255 54 9 97, rgb255 56 9 98, rgb255 57 9 99, rgb255 59 9 100, rgb255 61 9 101, rgb255 62 9 102, rgb255 64 10 103, rgb255 66 10 104, rgb255 68 10 104, rgb255 69 10 105, rgb255 71 11 106, rgb255 73 11 106, rgb255 74 12 107, rgb255 76 12 107, rgb255 77 13 108, rgb255 79 13 108, rgb255 81 14 108, rgb255 82 14 109, rgb255 84 15 109, rgb255 85 15 109, rgb255 87 16 110, rgb255 89 16 110, rgb255 90 17 110, rgb255 92 18 110, rgb255 93 18 110, rgb255 95 19 110, rgb255 97 19 110, rgb255 98 20 110, rgb255 100 21 110, rgb255 101 21 110, rgb255 103 22 110, rgb255 105 22 110, rgb255 106 23 110, rgb255 108 24 110, rgb255 109 24 110, rgb255 111 25 110, rgb255 113 25 110, rgb255 114 26 110, rgb255 116 26 110, rgb255 117 27 110, rgb255 119 28 109, rgb255 120 28 109, rgb255 122 29 109, rgb255 124 29 109, rgb255 125 30 109, rgb255 127 30 108, rgb255 128 31 108, rgb255 130 32 108, rgb255 132 32 107, rgb255 133 33 107, rgb255 135 33 107, rgb255 136 34 106, rgb255 138 34 106, rgb255 140 35 105, rgb255 141 35 105, rgb255 143 36 105, rgb255 144 37 104, rgb255 146 37 104, rgb255 147 38 103, rgb255 149 38 103, rgb255 151 39 102, rgb255 152 39 102, rgb255 154 40 101, rgb255 155 41 100, rgb255 157 41 100, rgb255 159 42 99, rgb255 160 42 99, rgb255 162 43 98, rgb255 163 44 97, rgb255 165 44 96, rgb255 166 45 96, rgb255 168 46 95, rgb255 169 46 94, rgb255 171 47 94, rgb255 173 48 93, rgb255 174 48 92, rgb255 176 49 91, rgb255 177 50 90, rgb255 179 50 90, rgb255 180 51 89, rgb255 182 52 88, rgb255 183 53 87, rgb255 185 53 86, rgb255 186 54 85, rgb255 188 55 84, rgb255 189 56 83, rgb255 191 57 82, rgb255 192 58 81, rgb255 193 58 80, rgb255 195 59 79, rgb255 196 60 78, rgb255 198 61 77, rgb255 199 62 76, rgb255 200 63 75, rgb255 202 64 74, rgb255 203 65 73, rgb255 204 66 72, rgb255 206 67 71, rgb255 207 68 70, rgb255 208 69 69, rgb255 210 70 68, rgb255 211 71 67, rgb255 212 72 66, rgb255 213 74 65, rgb255 215 75 63, rgb255 216 76 62, rgb255 217 77 61, rgb255 218 78 60, rgb255 219 80 59, rgb255 221 81 58, rgb255 222 82 56, rgb255 223 83 55, rgb255 224 85 54, rgb255 225 86 53, rgb255 226 87 52, rgb255 227 89 51, rgb255 228 90 49, rgb255 229 92 48, rgb255 230 93 47, rgb255 231 94 46, rgb255 232 96 45, rgb255 233 97 43, rgb255 234 99 42, rgb255 235 100 41, rgb255 235 102 40, rgb255 236 103 38, rgb255 237 105 37, rgb255 238 106 36, rgb255 239 108 35, rgb255 239 110 33, rgb255 240 111 32, rgb255 241 113 31, rgb255 241 115 29, rgb255 242 116 28, rgb255 243 118 27, rgb255 243 120 25, rgb255 244 121 24, rgb255 245 123 23, rgb255 245 125 21, rgb255 246 126 20, rgb255 246 128 19, rgb255 247 130 18, rgb255 247 132 16, rgb255 248 133 15, rgb255 248 135 14, rgb255 248 137 12, rgb255 249 139 11, rgb255 249 140 10, rgb255 249 142 9, rgb255 250 144 8, rgb255 250 146 7, rgb255 250 148 7, rgb255 251 150 6, rgb255 251 151 6, rgb255 251 153 6, rgb255 251 155 6, rgb255 251 157 7, rgb255 252 159 7, rgb255 252 161 8, rgb255 252 163 9, rgb255 252 165 10, rgb255 252 166 12, rgb255 252 168 13, rgb255 252 170 15, rgb255 252 172 17, rgb255 252 174 18, rgb255 252 176 20, rgb255 252 178 22, rgb255 252 180 24, rgb255 251 182 26, rgb255 251 184 29, rgb255 251 186 31, rgb255 251 188 33, rgb255 251 190 35, rgb255 250 192 38, rgb255 250 194 40, rgb255 250 196 42, rgb255 250 198 45, rgb255 249 199 47, rgb255 249 201 50, rgb255 249 203 53, rgb255 248 205 55, rgb255 248 207 58, rgb255 247 209 61, rgb255 247 211 64, rgb255 246 213 67, rgb255 246 215 70, rgb255 245 217 73, rgb255 245 219 76, rgb255 244 221 79, rgb255 244 223 83, rgb255 244 225 86, rgb255 243 227 90, rgb255 243 229 93, rgb255 242 230 97, rgb255 242 232 101, rgb255 242 234 105, rgb255 241 236 109, rgb255 241 237 113, rgb255 241 239 117, rgb255 241 241 121, rgb255 242 242 125, rgb255 242 244 130, rgb255 243 245 134, rgb255 243 246 138, rgb255 244 248 142, rgb255 245 249 146, rgb255 246 250 150, rgb255 248 251 154, rgb255 249 252 157, rgb255 250 253 161, rgb255 252 255 164 ]


{-| ![Plasma](https://code.gampleman.eu/elm-visualization/misc/plasma.png)

Given a number t in the range [0,1], returns the corresponding
color from the “plasma” perceptually-uniform color scheme designed
by [van der Walt, Smith and Firing](https://bids.github.io/colormap/)
for matplotlib.

-}
plasmaInterpolator : Float -> Color
plasmaInterpolator =
    mkInterpolator <|
        Array.fromList [ rgb255 13 8 135, rgb255 16 7 136, rgb255 19 7 137, rgb255 22 7 138, rgb255 25 6 140, rgb255 27 6 141, rgb255 29 6 142, rgb255 32 6 143, rgb255 34 6 144, rgb255 36 6 145, rgb255 38 5 145, rgb255 40 5 146, rgb255 42 5 147, rgb255 44 5 148, rgb255 46 5 149, rgb255 47 5 150, rgb255 49 5 151, rgb255 51 5 151, rgb255 53 4 152, rgb255 55 4 153, rgb255 56 4 154, rgb255 58 4 154, rgb255 60 4 155, rgb255 62 4 156, rgb255 63 4 156, rgb255 65 4 157, rgb255 67 3 158, rgb255 68 3 158, rgb255 70 3 159, rgb255 72 3 159, rgb255 73 3 160, rgb255 75 3 161, rgb255 76 2 161, rgb255 78 2 162, rgb255 80 2 162, rgb255 81 2 163, rgb255 83 2 163, rgb255 85 2 164, rgb255 86 1 164, rgb255 88 1 164, rgb255 89 1 165, rgb255 91 1 165, rgb255 92 1 166, rgb255 94 1 166, rgb255 96 1 166, rgb255 97 0 167, rgb255 99 0 167, rgb255 100 0 167, rgb255 102 0 167, rgb255 103 0 168, rgb255 105 0 168, rgb255 106 0 168, rgb255 108 0 168, rgb255 110 0 168, rgb255 111 0 168, rgb255 113 0 168, rgb255 114 1 168, rgb255 116 1 168, rgb255 117 1 168, rgb255 119 1 168, rgb255 120 1 168, rgb255 122 2 168, rgb255 123 2 168, rgb255 125 3 168, rgb255 126 3 168, rgb255 128 4 168, rgb255 129 4 167, rgb255 131 5 167, rgb255 132 5 167, rgb255 134 6 166, rgb255 135 7 166, rgb255 136 8 166, rgb255 138 9 165, rgb255 139 10 165, rgb255 141 11 165, rgb255 142 12 164, rgb255 143 13 164, rgb255 145 14 163, rgb255 146 15 163, rgb255 148 16 162, rgb255 149 17 161, rgb255 150 19 161, rgb255 152 20 160, rgb255 153 21 159, rgb255 154 22 159, rgb255 156 23 158, rgb255 157 24 157, rgb255 158 25 157, rgb255 160 26 156, rgb255 161 27 155, rgb255 162 29 154, rgb255 163 30 154, rgb255 165 31 153, rgb255 166 32 152, rgb255 167 33 151, rgb255 168 34 150, rgb255 170 35 149, rgb255 171 36 148, rgb255 172 38 148, rgb255 173 39 147, rgb255 174 40 146, rgb255 176 41 145, rgb255 177 42 144, rgb255 178 43 143, rgb255 179 44 142, rgb255 180 46 141, rgb255 181 47 140, rgb255 182 48 139, rgb255 183 49 138, rgb255 184 50 137, rgb255 186 51 136, rgb255 187 52 136, rgb255 188 53 135, rgb255 189 55 134, rgb255 190 56 133, rgb255 191 57 132, rgb255 192 58 131, rgb255 193 59 130, rgb255 194 60 129, rgb255 195 61 128, rgb255 196 62 127, rgb255 197 64 126, rgb255 198 65 125, rgb255 199 66 124, rgb255 200 67 123, rgb255 201 68 122, rgb255 202 69 122, rgb255 203 70 121, rgb255 204 71 120, rgb255 204 73 119, rgb255 205 74 118, rgb255 206 75 117, rgb255 207 76 116, rgb255 208 77 115, rgb255 209 78 114, rgb255 210 79 113, rgb255 211 81 113, rgb255 212 82 112, rgb255 213 83 111, rgb255 213 84 110, rgb255 214 85 109, rgb255 215 86 108, rgb255 216 87 107, rgb255 217 88 106, rgb255 218 90 106, rgb255 218 91 105, rgb255 219 92 104, rgb255 220 93 103, rgb255 221 94 102, rgb255 222 95 101, rgb255 222 97 100, rgb255 223 98 99, rgb255 224 99 99, rgb255 225 100 98, rgb255 226 101 97, rgb255 226 102 96, rgb255 227 104 95, rgb255 228 105 94, rgb255 229 106 93, rgb255 229 107 93, rgb255 230 108 92, rgb255 231 110 91, rgb255 231 111 90, rgb255 232 112 89, rgb255 233 113 88, rgb255 233 114 87, rgb255 234 116 87, rgb255 235 117 86, rgb255 235 118 85, rgb255 236 119 84, rgb255 237 121 83, rgb255 237 122 82, rgb255 238 123 81, rgb255 239 124 81, rgb255 239 126 80, rgb255 240 127 79, rgb255 240 128 78, rgb255 241 129 77, rgb255 241 131 76, rgb255 242 132 75, rgb255 243 133 75, rgb255 243 135 74, rgb255 244 136 73, rgb255 244 137 72, rgb255 245 139 71, rgb255 245 140 70, rgb255 246 141 69, rgb255 246 143 68, rgb255 247 144 68, rgb255 247 145 67, rgb255 247 147 66, rgb255 248 148 65, rgb255 248 149 64, rgb255 249 151 63, rgb255 249 152 62, rgb255 249 154 62, rgb255 250 155 61, rgb255 250 156 60, rgb255 250 158 59, rgb255 251 159 58, rgb255 251 161 57, rgb255 251 162 56, rgb255 252 163 56, rgb255 252 165 55, rgb255 252 166 54, rgb255 252 168 53, rgb255 252 169 52, rgb255 253 171 51, rgb255 253 172 51, rgb255 253 174 50, rgb255 253 175 49, rgb255 253 177 48, rgb255 253 178 47, rgb255 253 180 47, rgb255 253 181 46, rgb255 254 183 45, rgb255 254 184 44, rgb255 254 186 44, rgb255 254 187 43, rgb255 254 189 42, rgb255 254 190 42, rgb255 254 192 41, rgb255 253 194 41, rgb255 253 195 40, rgb255 253 197 39, rgb255 253 198 39, rgb255 253 200 39, rgb255 253 202 38, rgb255 253 203 38, rgb255 252 205 37, rgb255 252 206 37, rgb255 252 208 37, rgb255 252 210 37, rgb255 251 211 36, rgb255 251 213 36, rgb255 251 215 36, rgb255 250 216 36, rgb255 250 218 36, rgb255 249 220 36, rgb255 249 221 37, rgb255 248 223 37, rgb255 248 225 37, rgb255 247 226 37, rgb255 247 228 37, rgb255 246 230 38, rgb255 246 232 38, rgb255 245 233 38, rgb255 245 235 39, rgb255 244 237 39, rgb255 243 238 39, rgb255 243 240 39, rgb255 242 242 39, rgb255 241 244 38, rgb255 241 245 37, rgb255 240 247 36, rgb255 240 249 33 ]


{-| ![category10](https://code.gampleman.eu/elm-visualization/misc/category10.png)

A list of ten categorical colors

-}
category10 : List Color
category10 =
    [ rgb255 31 119 180, rgb255 255 127 14, rgb255 44 160 44, rgb255 214 39 40, rgb255 148 103 189, rgb255 140 86 75, rgb255 227 119 194, rgb255 127 127 127, rgb255 188 189 34, rgb255 23 190 207 ]


{-| ![category10](https://code.gampleman.eu/elm-visualization/misc/tableau10.png)

A list of ten categorical colors

-}
tableau10 : List Color
tableau10 =
    [ rgb255 78 121 167, rgb255 242 142 44, rgb255 225 87 89, rgb255 118 183 178, rgb255 89 161 79, rgb255 237 201 73, rgb255 175 122 161, rgb255 255 157 167, rgb255 156 117 95, rgb255 186 176 171 ]



-- CIELAB


linear : Float -> Float -> Float -> Float
linear t i1 i2 =
    i1 + (i2 - i1) * t


hsluvInterpolator : ( Color, Color ) -> Float -> Color
hsluvInterpolator ( start, end ) t =
    let
        start_ =
            HSLuv.color start
                |> HSLuv.toHsluv
                |> Debug.log "s"

        end_ =
            HSLuv.color end
                |> HSLuv.toHsluv
                |> Debug.log "e"

        i =
            linear t

        color h s l =
            HSLuv.hsluv { hue = h, saturation = s, lightness = l, alpha = 1 } |> HSLuv.toColor
    in
    color
        (i start_.hue end_.hue)
        (i start_.saturation end_.saturation)
        (i start_.lightness end_.lightness)


cielabInterpolator : ( Color, Color ) -> Float -> Color
cielabInterpolator ( start, end ) t =
    let
        s =
            colorToLab start
                |> Debug.log "s"

        e =
            colorToLab end
                |> Debug.log "e"

        i =
            linear t
    in
    labToColor
        { l = i s.l e.l
        , a = i s.a e.a
        , b = i s.b e.b
        }



-- COLORS EXTRA
-- adapted from: https://github.com/eskimoblood/elm-color-extra


type alias XYZ =
    { x : Float, y : Float, z : Float }


type alias Lab =
    { l : Float, a : Float, b : Float }


rgbaToRgb255 :
    { red : Float
    , green : Float
    , blue : Float
    , alpha : Float
    }
    ->
        { red : Int
        , green : Int
        , blue : Int
        }
rgbaToRgb255 { red, green, blue } =
    let
        s =
            Scale.linear ( 0, 255 ) ( 0, 1 )
    in
    { red = Scale.convert s red |> round
    , green = Scale.convert s green |> round
    , blue = Scale.convert s blue |> round
    }


{-| Convert color to CIELAB- color space
-}
colorToLab : Color -> { l : Float, a : Float, b : Float }
colorToLab =
    colorToXyz >> xyzToLab


colorToXyz : Color -> XYZ
colorToXyz cl =
    let
        c ch =
            let
                ch_ =
                    toFloat ch / 255

                ch__ =
                    if ch_ > 4.045e-2 then
                        ((ch_ + 5.5e-2) / 1.055) ^ 2.4

                    else
                        ch_ / 12.92
            in
            ch__ * 100

        { red, green, blue } =
            toRgba cl |> rgbaToRgb255

        r =
            c red

        g =
            c green

        b =
            c blue
    in
    { x = r * 0.4124 + g * 0.3576 + b * 0.1805
    , y = r * 0.2126 + g * 0.7152 + b * 7.22e-2
    , z = r * 1.93e-2 + g * 0.1192 + b * 0.9505
    }


xyzToLab : XYZ -> Lab
xyzToLab { x, y, z } =
    let
        c ch =
            if ch > 8.856e-3 then
                ch ^ (1 / 3)

            else
                (7.787 * ch) + (16 / 116)

        x_ =
            c (x / 95.047)

        y_ =
            c (y / 100)

        z_ =
            c (z / 108.883)
    in
    { l = (116 * y_) - 16
    , a = 500 * (x_ - y_)
    , b = 200 * (y_ - z_)
    }


{-| Convert a color in CIELAB- color space to Elm `Color`
-}
labToColor : { l : Float, a : Float, b : Float } -> Color
labToColor =
    labToXyz >> xyzToColor


labToXyz : Lab -> XYZ
labToXyz { l, a, b } =
    let
        c ch =
            let
                ch_ =
                    ch * ch * ch
            in
            if ch_ > 8.856e-3 then
                ch_

            else
                (ch - 16 / 116) / 7.787

        y =
            (l + 16) / 116
    in
    { y = c y * 100
    , x = c (y + a / 500) * 95.047
    , z = c (y - b / 200) * 108.883
    }


xyzToColor : XYZ -> Color
xyzToColor { x, y, z } =
    let
        x_ =
            x / 100

        y_ =
            y / 100

        z_ =
            z / 100

        r =
            x_ * 3.2404542 + y_ * -1.5371385 + z_ * -0.4986

        g =
            x_ * -0.969266 + y_ * 1.8760108 + z_ * 4.1556e-2

        b =
            x_ * 5.56434e-2 + y_ * -0.2040259 + z_ * 1.0572252

        c ch =
            let
                ch_ =
                    if ch > 3.1308e-3 then
                        1.055 * (ch ^ (1 / 2.4)) - 5.5e-2

                    else
                        12.92 * ch
            in
            round <| clamp 0 255 (ch_ * 255)
    in
    rgb255 (c r) (c g) (c b)
