module Scale.Color exposing
    ( category10, accent, paired, pastel1, pastel2, tableau10, colorblind, set1, set2
    , bluesInterpolator, greensInterpolator, greysInterpolator, orangesInterpolator, purplesInterpolator, redsInterpolator, brownsInterpolator, tealInterpolator, warmGreysInterpolator, lightOrangeInterpolator
    , viridisInterpolator, infernoInterpolator, magmaInterpolator, plasmaInterpolator, blueGreenInterpolator, bluePurpleInterpolator, greenBlueInterpolator, orangeRedInterpolator, purpleBlueInterpolator, purpleBlueGreenInterpolator, purpleRedInterpolator, redPurpleInterpolator, yellowGreenInterpolator, yellowOrangeBrownInterpolator, yellowOrangeRedInterpolator, tealBluesInterpolator, goldGreensInterpolator, goldOrangeInterpolator, goldRedInterpolator, lightGreyRedInterpolator, lightGreyTealInterpolator, lightMultiInterpolator
    , blueOrangeInterpolator, brownBlueGreenInterpolator, purpleGreenInterpolator, purpleOrangeInterpolator, redBlueInterpolator, redGreyInterpolator, yellowGreenBlueInterpolator, redYellowBlueInterpolator, redYellowGreenInterpolator, pinkYellowGreenInterpolator, spectralInterpolator, carbonDiverging1Interpolator, carbonDiverging2Interpolator
    , turboInterpolator, rainbowInterpolator, sinebowInterpolator
    , carbonAlert
    )

{-| We provide sequential and categorical color schemes designed to work with [ordinal](Scale#OrdinalScale) and [sequential](Scale#SequentialScale) scales. Color types come from [avh4/elm-color](https://package.elm-lang.org/packages/avh4/elm-color/latest/).


# Categorical

Categorical color schemes can be used to encode discrete data values, each representing a distinct category.

@docs category10, accent, paired, pastel1, pastel2, tableau10, colorblind, set1, set2


# Sequential Single-Hue

Given a number t in the range [0,1], returns the corresponding color from the color scheme

Sequential color schemes can be used to encode quantitative values. These color ramps are designed to encode increasing numeric values.

@docs bluesInterpolator, greensInterpolator, greysInterpolator, orangesInterpolator, purplesInterpolator, redsInterpolator, brownsInterpolator, tealInterpolator, warmGreysInterpolator, lightOrangeInterpolator


# Sequential Multi-Hue

Given a number t in the range [0,1], returns the corresponding color from the color scheme

Sequential color schemes can be used to encode quantitative values. These color ramps are designed to encode increasing numeric values, but use additional hues for more color discrimination, which may be useful for visualizations such as heatmaps. However, beware that using multiple hues may cause viewers to inaccurately see the data range as grouped into color-coded clusters.

@docs viridisInterpolator, infernoInterpolator, magmaInterpolator, plasmaInterpolator, blueGreenInterpolator, bluePurpleInterpolator, greenBlueInterpolator, orangeRedInterpolator, purpleBlueInterpolator, purpleBlueGreenInterpolator, purpleRedInterpolator, redPurpleInterpolator, yellowGreenInterpolator, yellowOrangeBrownInterpolator, yellowOrangeRedInterpolator, tealBluesInterpolator, goldGreensInterpolator, goldOrangeInterpolator, goldRedInterpolator, lightGreyRedInterpolator, lightGreyTealInterpolator, lightMultiInterpolator


# Diverging

Given a number t in the range [0,1], returns the corresponding color from the color scheme

Diverging color schemes can be used to encode quantitative values with a meaningful mid-point, such as zero or the average value. Color ramps with different hues diverge with increasing saturation to highlight the values below and above the mid-point.

@docs blueOrangeInterpolator, brownBlueGreenInterpolator, purpleGreenInterpolator, purpleOrangeInterpolator, redBlueInterpolator, redGreyInterpolator, yellowGreenBlueInterpolator, redYellowBlueInterpolator, redYellowGreenInterpolator, pinkYellowGreenInterpolator, spectralInterpolator, carbonDiverging1Interpolator, carbonDiverging2Interpolator


# Cyclic

Given a number t in the range [0,1], returns the corresponding color from the color scheme

Cyclical color schemes may be used to highlight periodic patterns in continuous data. However, these schemes are not well suited to accurately convey value differences.

@docs turboInterpolator, rainbowInterpolator, sinebowInterpolator


# Alert

Alert colors are used to reflect status. Typically, red represents danger or error; orange represents a serious warning; yellow represents a regular warning, and green represents normal or success.

@docs carbonAlert

-}

import Array exposing (Array)
import Color exposing (Color, black, rgb, rgb255)
import Hex
import Interpolation


{-| ![Viridis](https://code.gampleman.eu/elm-visualization/misc/viridis.png)

The “viridis” perceptually-uniform color scheme designed
by [van der Walt, Smith and Firing](https://bids.github.io/colormap/)
for matplotlib.

-}
viridisInterpolator : Float -> Color
viridisInterpolator =
    mkInterpolator <|
        Array.fromList
            [ rgb255 68 1 84, rgb255 68 2 86, rgb255 69 4 87, rgb255 69 5 89, rgb255 70 7 90, rgb255 70 8 92, rgb255 70 10 93, rgb255 70 11 94, rgb255 71 13 96, rgb255 71 14 97, rgb255 71 16 99, rgb255 71 17 100, rgb255 71 19 101, rgb255 72 20 103, rgb255 72 22 104, rgb255 72 23 105, rgb255 72 24 106, rgb255 72 26 108, rgb255 72 27 109, rgb255 72 28 110, rgb255 72 29 111, rgb255 72 31 112, rgb255 72 32 113, rgb255 72 33 115, rgb255 72 35 116, rgb255 72 36 117, rgb255 72 37 118, rgb255 72 38 119, rgb255 72 40 120, rgb255 72 41 121, rgb255 71 42 122, rgb255 71 44 122, rgb255 71 45 123, rgb255 71 46 124, rgb255 71 47 125, rgb255 70 48 126, rgb255 70 50 126, rgb255 70 51 127, rgb255 70 52 128, rgb255 69 53 129, rgb255 69 55 129, rgb255 69 56 130, rgb255 68 57 131, rgb255 68 58 131, rgb255 68 59 132, rgb255 67 61 132, rgb255 67 62 133, rgb255 66 63 133, rgb255 66 64 134, rgb255 66 65 134, rgb255 65 66 135, rgb255 65 68 135, rgb255 64 69 136, rgb255 64 70 136, rgb255 63 71 136, rgb255 63 72 137, rgb255 62 73 137, rgb255 62 74 137, rgb255 62 76 138, rgb255 61 77 138, rgb255 61 78 138, rgb255 60 79 138, rgb255 60 80 139, rgb255 59 81 139, rgb255 59 82 139, rgb255 58 83 139, rgb255 58 84 140, rgb255 57 85 140, rgb255 57 86 140, rgb255 56 88 140, rgb255 56 89 140, rgb255 55 90 140, rgb255 55 91 141, rgb255 54 92 141, rgb255 54 93 141, rgb255 53 94 141, rgb255 53 95 141, rgb255 52 96 141, rgb255 52 97 141, rgb255 51 98 141, rgb255 51 99 141, rgb255 50 100 142, rgb255 50 101 142, rgb255 49 102 142, rgb255 49 103 142, rgb255 49 104 142, rgb255 48 105 142, rgb255 48 106 142, rgb255 47 107 142, rgb255 47 108 142, rgb255 46 109 142, rgb255 46 110 142, rgb255 46 111 142, rgb255 45 112 142, rgb255 45 113 142, rgb255 44 113 142, rgb255 44 114 142, rgb255 44 115 142, rgb255 43 116 142, rgb255 43 117 142, rgb255 42 118 142, rgb255 42 119 142, rgb255 42 120 142, rgb255 41 121 142, rgb255 41 122 142, rgb255 41 123 142, rgb255 40 124 142, rgb255 40 125 142, rgb255 39 126 142, rgb255 39 127 142, rgb255 39 128 142, rgb255 38 129 142, rgb255 38 130 142, rgb255 38 130 142, rgb255 37 131 142, rgb255 37 132 142, rgb255 37 133 142, rgb255 36 134 142, rgb255 36 135 142, rgb255 35 136 142, rgb255 35 137 142, rgb255 35 138 141, rgb255 34 139 141, rgb255 34 140 141, rgb255 34 141 141, rgb255 33 142 141, rgb255 33 143 141, rgb255 33 144 141, rgb255 33 145 140, rgb255 32 146 140, rgb255 32 146 140, rgb255 32 147 140, rgb255 31 148 140, rgb255 31 149 139, rgb255 31 150 139, rgb255 31 151 139, rgb255 31 152 139, rgb255 31 153 138, rgb255 31 154 138, rgb255 30 155 138, rgb255 30 156 137, rgb255 30 157 137, rgb255 31 158 137, rgb255 31 159 136, rgb255 31 160 136, rgb255 31 161 136, rgb255 31 161 135, rgb255 31 162 135, rgb255 32 163 134, rgb255 32 164 134, rgb255 33 165 133, rgb255 33 166 133, rgb255 34 167 133, rgb255 34 168 132, rgb255 35 169 131, rgb255 36 170 131, rgb255 37 171 130, rgb255 37 172 130, rgb255 38 173 129, rgb255 39 173 129, rgb255 40 174 128, rgb255 41 175 127, rgb255 42 176 127, rgb255 44 177 126, rgb255 45 178 125, rgb255 46 179 124, rgb255 47 180 124, rgb255 49 181 123, rgb255 50 182 122, rgb255 52 182 121, rgb255 53 183 121, rgb255 55 184 120, rgb255 56 185 119, rgb255 58 186 118, rgb255 59 187 117, rgb255 61 188 116, rgb255 63 188 115, rgb255 64 189 114, rgb255 66 190 113, rgb255 68 191 112, rgb255 70 192 111, rgb255 72 193 110, rgb255 74 193 109, rgb255 76 194 108, rgb255 78 195 107, rgb255 80 196 106, rgb255 82 197 105, rgb255 84 197 104, rgb255 86 198 103, rgb255 88 199 101, rgb255 90 200 100, rgb255 92 200 99, rgb255 94 201 98, rgb255 96 202 96, rgb255 99 203 95, rgb255 101 203 94, rgb255 103 204 92, rgb255 105 205 91, rgb255 108 205 90, rgb255 110 206 88, rgb255 112 207 87, rgb255 115 208 86, rgb255 117 208 84, rgb255 119 209 83, rgb255 122 209 81, rgb255 124 210 80, rgb255 127 211 78, rgb255 129 211 77, rgb255 132 212 75, rgb255 134 213 73, rgb255 137 213 72, rgb255 139 214 70, rgb255 142 214 69, rgb255 144 215 67, rgb255 147 215 65, rgb255 149 216 64, rgb255 152 216 62, rgb255 155 217 60, rgb255 157 217 59, rgb255 160 218 57, rgb255 162 218 55, rgb255 165 219 54, rgb255 168 219 52, rgb255 170 220 50, rgb255 173 220 48, rgb255 176 221 47, rgb255 178 221 45, rgb255 181 222 43, rgb255 184 222 41, rgb255 186 222 40, rgb255 189 223 38, rgb255 192 223 37, rgb255 194 223 35, rgb255 197 224 33, rgb255 200 224 32, rgb255 202 225 31, rgb255 205 225 29, rgb255 208 225 28, rgb255 210 226 27, rgb255 213 226 26, rgb255 216 226 25, rgb255 218 227 25, rgb255 221 227 24, rgb255 223 227 24, rgb255 226 228 24, rgb255 229 228 25, rgb255 231 228 25, rgb255 234 229 26, rgb255 236 229 27, rgb255 239 229 28, rgb255 241 229 29, rgb255 244 230 30, rgb255 246 230 32, rgb255 248 230 33, rgb255 251 231 35, rgb255 253 231 37 ]


{-| ![magma](https://code.gampleman.eu/elm-visualization/misc/magma.png)

The “magma” perceptually-uniform color scheme designed
by [van der Walt, Smith and Firing](https://bids.github.io/colormap/)
for matplotlib,.

-}
magmaInterpolator : Float -> Color
magmaInterpolator =
    mkInterpolator <|
        Array.fromList [ rgb255 0 0 4, rgb255 1 0 5, rgb255 1 1 6, rgb255 1 1 8, rgb255 2 1 9, rgb255 2 2 11, rgb255 2 2 13, rgb255 3 3 15, rgb255 3 3 18, rgb255 4 4 20, rgb255 5 4 22, rgb255 6 5 24, rgb255 6 5 26, rgb255 7 6 28, rgb255 8 7 30, rgb255 9 7 32, rgb255 10 8 34, rgb255 11 9 36, rgb255 12 9 38, rgb255 13 10 41, rgb255 14 11 43, rgb255 16 11 45, rgb255 17 12 47, rgb255 18 13 49, rgb255 19 13 52, rgb255 20 14 54, rgb255 21 14 56, rgb255 22 15 59, rgb255 24 15 61, rgb255 25 16 63, rgb255 26 16 66, rgb255 28 16 68, rgb255 29 17 71, rgb255 30 17 73, rgb255 32 17 75, rgb255 33 17 78, rgb255 34 17 80, rgb255 36 18 83, rgb255 37 18 85, rgb255 39 18 88, rgb255 41 17 90, rgb255 42 17 92, rgb255 44 17 95, rgb255 45 17 97, rgb255 47 17 99, rgb255 49 17 101, rgb255 51 16 103, rgb255 52 16 105, rgb255 54 16 107, rgb255 56 16 108, rgb255 57 15 110, rgb255 59 15 112, rgb255 61 15 113, rgb255 63 15 114, rgb255 64 15 116, rgb255 66 15 117, rgb255 68 15 118, rgb255 69 16 119, rgb255 71 16 120, rgb255 73 16 120, rgb255 74 16 121, rgb255 76 17 122, rgb255 78 17 123, rgb255 79 18 123, rgb255 81 18 124, rgb255 82 19 124, rgb255 84 19 125, rgb255 86 20 125, rgb255 87 21 126, rgb255 89 21 126, rgb255 90 22 126, rgb255 92 22 127, rgb255 93 23 127, rgb255 95 24 127, rgb255 96 24 128, rgb255 98 25 128, rgb255 100 26 128, rgb255 101 26 128, rgb255 103 27 128, rgb255 104 28 129, rgb255 106 28 129, rgb255 107 29 129, rgb255 109 29 129, rgb255 110 30 129, rgb255 112 31 129, rgb255 114 31 129, rgb255 115 32 129, rgb255 117 33 129, rgb255 118 33 129, rgb255 120 34 129, rgb255 121 34 130, rgb255 123 35 130, rgb255 124 35 130, rgb255 126 36 130, rgb255 128 37 130, rgb255 129 37 129, rgb255 131 38 129, rgb255 132 38 129, rgb255 134 39 129, rgb255 136 39 129, rgb255 137 40 129, rgb255 139 41 129, rgb255 140 41 129, rgb255 142 42 129, rgb255 144 42 129, rgb255 145 43 129, rgb255 147 43 128, rgb255 148 44 128, rgb255 150 44 128, rgb255 152 45 128, rgb255 153 45 128, rgb255 155 46 127, rgb255 156 46 127, rgb255 158 47 127, rgb255 160 47 127, rgb255 161 48 126, rgb255 163 48 126, rgb255 165 49 126, rgb255 166 49 125, rgb255 168 50 125, rgb255 170 51 125, rgb255 171 51 124, rgb255 173 52 124, rgb255 174 52 123, rgb255 176 53 123, rgb255 178 53 123, rgb255 179 54 122, rgb255 181 54 122, rgb255 183 55 121, rgb255 184 55 121, rgb255 186 56 120, rgb255 188 57 120, rgb255 189 57 119, rgb255 191 58 119, rgb255 192 58 118, rgb255 194 59 117, rgb255 196 60 117, rgb255 197 60 116, rgb255 199 61 115, rgb255 200 62 115, rgb255 202 62 114, rgb255 204 63 113, rgb255 205 64 113, rgb255 207 64 112, rgb255 208 65 111, rgb255 210 66 111, rgb255 211 67 110, rgb255 213 68 109, rgb255 214 69 108, rgb255 216 69 108, rgb255 217 70 107, rgb255 219 71 106, rgb255 220 72 105, rgb255 222 73 104, rgb255 223 74 104, rgb255 224 76 103, rgb255 226 77 102, rgb255 227 78 101, rgb255 228 79 100, rgb255 229 80 100, rgb255 231 82 99, rgb255 232 83 98, rgb255 233 84 98, rgb255 234 86 97, rgb255 235 87 96, rgb255 236 88 96, rgb255 237 90 95, rgb255 238 91 94, rgb255 239 93 94, rgb255 240 95 94, rgb255 241 96 93, rgb255 242 98 93, rgb255 242 100 92, rgb255 243 101 92, rgb255 244 103 92, rgb255 244 105 92, rgb255 245 107 92, rgb255 246 108 92, rgb255 246 110 92, rgb255 247 112 92, rgb255 247 114 92, rgb255 248 116 92, rgb255 248 118 92, rgb255 249 120 93, rgb255 249 121 93, rgb255 249 123 93, rgb255 250 125 94, rgb255 250 127 94, rgb255 250 129 95, rgb255 251 131 95, rgb255 251 133 96, rgb255 251 135 97, rgb255 252 137 97, rgb255 252 138 98, rgb255 252 140 99, rgb255 252 142 100, rgb255 252 144 101, rgb255 253 146 102, rgb255 253 148 103, rgb255 253 150 104, rgb255 253 152 105, rgb255 253 154 106, rgb255 253 155 107, rgb255 254 157 108, rgb255 254 159 109, rgb255 254 161 110, rgb255 254 163 111, rgb255 254 165 113, rgb255 254 167 114, rgb255 254 169 115, rgb255 254 170 116, rgb255 254 172 118, rgb255 254 174 119, rgb255 254 176 120, rgb255 254 178 122, rgb255 254 180 123, rgb255 254 182 124, rgb255 254 183 126, rgb255 254 185 127, rgb255 254 187 129, rgb255 254 189 130, rgb255 254 191 132, rgb255 254 193 133, rgb255 254 194 135, rgb255 254 196 136, rgb255 254 198 138, rgb255 254 200 140, rgb255 254 202 141, rgb255 254 204 143, rgb255 254 205 144, rgb255 254 207 146, rgb255 254 209 148, rgb255 254 211 149, rgb255 254 213 151, rgb255 254 215 153, rgb255 254 216 154, rgb255 253 218 156, rgb255 253 220 158, rgb255 253 222 160, rgb255 253 224 161, rgb255 253 226 163, rgb255 253 227 165, rgb255 253 229 167, rgb255 253 231 169, rgb255 253 233 170, rgb255 253 235 172, rgb255 252 236 174, rgb255 252 238 176, rgb255 252 240 178, rgb255 252 242 180, rgb255 252 244 182, rgb255 252 246 184, rgb255 252 247 185, rgb255 252 249 187, rgb255 252 251 189, rgb255 252 253 191 ]


{-| ![Inferno](https://code.gampleman.eu/elm-visualization/misc/inferno.png)

The “inferno” perceptually-uniform color scheme designed
by [van der Walt, Smith and Firing](https://bids.github.io/colormap/)
for matplotlib.

-}
infernoInterpolator : Float -> Color
infernoInterpolator =
    mkInterpolator <|
        Array.fromList [ rgb255 0 0 4, rgb255 1 0 5, rgb255 1 1 6, rgb255 1 1 8, rgb255 2 1 10, rgb255 2 2 12, rgb255 2 2 14, rgb255 3 2 16, rgb255 4 3 18, rgb255 4 3 20, rgb255 5 4 23, rgb255 6 4 25, rgb255 7 5 27, rgb255 8 5 29, rgb255 9 6 31, rgb255 10 7 34, rgb255 11 7 36, rgb255 12 8 38, rgb255 13 8 41, rgb255 14 9 43, rgb255 16 9 45, rgb255 17 10 48, rgb255 18 10 50, rgb255 20 11 52, rgb255 21 11 55, rgb255 22 11 57, rgb255 24 12 60, rgb255 25 12 62, rgb255 27 12 65, rgb255 28 12 67, rgb255 30 12 69, rgb255 31 12 72, rgb255 33 12 74, rgb255 35 12 76, rgb255 36 12 79, rgb255 38 12 81, rgb255 40 11 83, rgb255 41 11 85, rgb255 43 11 87, rgb255 45 11 89, rgb255 47 10 91, rgb255 49 10 92, rgb255 50 10 94, rgb255 52 10 95, rgb255 54 9 97, rgb255 56 9 98, rgb255 57 9 99, rgb255 59 9 100, rgb255 61 9 101, rgb255 62 9 102, rgb255 64 10 103, rgb255 66 10 104, rgb255 68 10 104, rgb255 69 10 105, rgb255 71 11 106, rgb255 73 11 106, rgb255 74 12 107, rgb255 76 12 107, rgb255 77 13 108, rgb255 79 13 108, rgb255 81 14 108, rgb255 82 14 109, rgb255 84 15 109, rgb255 85 15 109, rgb255 87 16 110, rgb255 89 16 110, rgb255 90 17 110, rgb255 92 18 110, rgb255 93 18 110, rgb255 95 19 110, rgb255 97 19 110, rgb255 98 20 110, rgb255 100 21 110, rgb255 101 21 110, rgb255 103 22 110, rgb255 105 22 110, rgb255 106 23 110, rgb255 108 24 110, rgb255 109 24 110, rgb255 111 25 110, rgb255 113 25 110, rgb255 114 26 110, rgb255 116 26 110, rgb255 117 27 110, rgb255 119 28 109, rgb255 120 28 109, rgb255 122 29 109, rgb255 124 29 109, rgb255 125 30 109, rgb255 127 30 108, rgb255 128 31 108, rgb255 130 32 108, rgb255 132 32 107, rgb255 133 33 107, rgb255 135 33 107, rgb255 136 34 106, rgb255 138 34 106, rgb255 140 35 105, rgb255 141 35 105, rgb255 143 36 105, rgb255 144 37 104, rgb255 146 37 104, rgb255 147 38 103, rgb255 149 38 103, rgb255 151 39 102, rgb255 152 39 102, rgb255 154 40 101, rgb255 155 41 100, rgb255 157 41 100, rgb255 159 42 99, rgb255 160 42 99, rgb255 162 43 98, rgb255 163 44 97, rgb255 165 44 96, rgb255 166 45 96, rgb255 168 46 95, rgb255 169 46 94, rgb255 171 47 94, rgb255 173 48 93, rgb255 174 48 92, rgb255 176 49 91, rgb255 177 50 90, rgb255 179 50 90, rgb255 180 51 89, rgb255 182 52 88, rgb255 183 53 87, rgb255 185 53 86, rgb255 186 54 85, rgb255 188 55 84, rgb255 189 56 83, rgb255 191 57 82, rgb255 192 58 81, rgb255 193 58 80, rgb255 195 59 79, rgb255 196 60 78, rgb255 198 61 77, rgb255 199 62 76, rgb255 200 63 75, rgb255 202 64 74, rgb255 203 65 73, rgb255 204 66 72, rgb255 206 67 71, rgb255 207 68 70, rgb255 208 69 69, rgb255 210 70 68, rgb255 211 71 67, rgb255 212 72 66, rgb255 213 74 65, rgb255 215 75 63, rgb255 216 76 62, rgb255 217 77 61, rgb255 218 78 60, rgb255 219 80 59, rgb255 221 81 58, rgb255 222 82 56, rgb255 223 83 55, rgb255 224 85 54, rgb255 225 86 53, rgb255 226 87 52, rgb255 227 89 51, rgb255 228 90 49, rgb255 229 92 48, rgb255 230 93 47, rgb255 231 94 46, rgb255 232 96 45, rgb255 233 97 43, rgb255 234 99 42, rgb255 235 100 41, rgb255 235 102 40, rgb255 236 103 38, rgb255 237 105 37, rgb255 238 106 36, rgb255 239 108 35, rgb255 239 110 33, rgb255 240 111 32, rgb255 241 113 31, rgb255 241 115 29, rgb255 242 116 28, rgb255 243 118 27, rgb255 243 120 25, rgb255 244 121 24, rgb255 245 123 23, rgb255 245 125 21, rgb255 246 126 20, rgb255 246 128 19, rgb255 247 130 18, rgb255 247 132 16, rgb255 248 133 15, rgb255 248 135 14, rgb255 248 137 12, rgb255 249 139 11, rgb255 249 140 10, rgb255 249 142 9, rgb255 250 144 8, rgb255 250 146 7, rgb255 250 148 7, rgb255 251 150 6, rgb255 251 151 6, rgb255 251 153 6, rgb255 251 155 6, rgb255 251 157 7, rgb255 252 159 7, rgb255 252 161 8, rgb255 252 163 9, rgb255 252 165 10, rgb255 252 166 12, rgb255 252 168 13, rgb255 252 170 15, rgb255 252 172 17, rgb255 252 174 18, rgb255 252 176 20, rgb255 252 178 22, rgb255 252 180 24, rgb255 251 182 26, rgb255 251 184 29, rgb255 251 186 31, rgb255 251 188 33, rgb255 251 190 35, rgb255 250 192 38, rgb255 250 194 40, rgb255 250 196 42, rgb255 250 198 45, rgb255 249 199 47, rgb255 249 201 50, rgb255 249 203 53, rgb255 248 205 55, rgb255 248 207 58, rgb255 247 209 61, rgb255 247 211 64, rgb255 246 213 67, rgb255 246 215 70, rgb255 245 217 73, rgb255 245 219 76, rgb255 244 221 79, rgb255 244 223 83, rgb255 244 225 86, rgb255 243 227 90, rgb255 243 229 93, rgb255 242 230 97, rgb255 242 232 101, rgb255 242 234 105, rgb255 241 236 109, rgb255 241 237 113, rgb255 241 239 117, rgb255 241 241 121, rgb255 242 242 125, rgb255 242 244 130, rgb255 243 245 134, rgb255 243 246 138, rgb255 244 248 142, rgb255 245 249 146, rgb255 246 250 150, rgb255 248 251 154, rgb255 249 252 157, rgb255 250 253 161, rgb255 252 255 164 ]


{-| ![Plasma](https://code.gampleman.eu/elm-visualization/misc/plasma.png)

The “plasma” perceptually-uniform color scheme designed
by [van der Walt, Smith and Firing](https://bids.github.io/colormap/)
for matplotlib.

-}
plasmaInterpolator : Float -> Color
plasmaInterpolator =
    mkInterpolator <|
        Array.fromList [ rgb255 13 8 135, rgb255 16 7 136, rgb255 19 7 137, rgb255 22 7 138, rgb255 25 6 140, rgb255 27 6 141, rgb255 29 6 142, rgb255 32 6 143, rgb255 34 6 144, rgb255 36 6 145, rgb255 38 5 145, rgb255 40 5 146, rgb255 42 5 147, rgb255 44 5 148, rgb255 46 5 149, rgb255 47 5 150, rgb255 49 5 151, rgb255 51 5 151, rgb255 53 4 152, rgb255 55 4 153, rgb255 56 4 154, rgb255 58 4 154, rgb255 60 4 155, rgb255 62 4 156, rgb255 63 4 156, rgb255 65 4 157, rgb255 67 3 158, rgb255 68 3 158, rgb255 70 3 159, rgb255 72 3 159, rgb255 73 3 160, rgb255 75 3 161, rgb255 76 2 161, rgb255 78 2 162, rgb255 80 2 162, rgb255 81 2 163, rgb255 83 2 163, rgb255 85 2 164, rgb255 86 1 164, rgb255 88 1 164, rgb255 89 1 165, rgb255 91 1 165, rgb255 92 1 166, rgb255 94 1 166, rgb255 96 1 166, rgb255 97 0 167, rgb255 99 0 167, rgb255 100 0 167, rgb255 102 0 167, rgb255 103 0 168, rgb255 105 0 168, rgb255 106 0 168, rgb255 108 0 168, rgb255 110 0 168, rgb255 111 0 168, rgb255 113 0 168, rgb255 114 1 168, rgb255 116 1 168, rgb255 117 1 168, rgb255 119 1 168, rgb255 120 1 168, rgb255 122 2 168, rgb255 123 2 168, rgb255 125 3 168, rgb255 126 3 168, rgb255 128 4 168, rgb255 129 4 167, rgb255 131 5 167, rgb255 132 5 167, rgb255 134 6 166, rgb255 135 7 166, rgb255 136 8 166, rgb255 138 9 165, rgb255 139 10 165, rgb255 141 11 165, rgb255 142 12 164, rgb255 143 13 164, rgb255 145 14 163, rgb255 146 15 163, rgb255 148 16 162, rgb255 149 17 161, rgb255 150 19 161, rgb255 152 20 160, rgb255 153 21 159, rgb255 154 22 159, rgb255 156 23 158, rgb255 157 24 157, rgb255 158 25 157, rgb255 160 26 156, rgb255 161 27 155, rgb255 162 29 154, rgb255 163 30 154, rgb255 165 31 153, rgb255 166 32 152, rgb255 167 33 151, rgb255 168 34 150, rgb255 170 35 149, rgb255 171 36 148, rgb255 172 38 148, rgb255 173 39 147, rgb255 174 40 146, rgb255 176 41 145, rgb255 177 42 144, rgb255 178 43 143, rgb255 179 44 142, rgb255 180 46 141, rgb255 181 47 140, rgb255 182 48 139, rgb255 183 49 138, rgb255 184 50 137, rgb255 186 51 136, rgb255 187 52 136, rgb255 188 53 135, rgb255 189 55 134, rgb255 190 56 133, rgb255 191 57 132, rgb255 192 58 131, rgb255 193 59 130, rgb255 194 60 129, rgb255 195 61 128, rgb255 196 62 127, rgb255 197 64 126, rgb255 198 65 125, rgb255 199 66 124, rgb255 200 67 123, rgb255 201 68 122, rgb255 202 69 122, rgb255 203 70 121, rgb255 204 71 120, rgb255 204 73 119, rgb255 205 74 118, rgb255 206 75 117, rgb255 207 76 116, rgb255 208 77 115, rgb255 209 78 114, rgb255 210 79 113, rgb255 211 81 113, rgb255 212 82 112, rgb255 213 83 111, rgb255 213 84 110, rgb255 214 85 109, rgb255 215 86 108, rgb255 216 87 107, rgb255 217 88 106, rgb255 218 90 106, rgb255 218 91 105, rgb255 219 92 104, rgb255 220 93 103, rgb255 221 94 102, rgb255 222 95 101, rgb255 222 97 100, rgb255 223 98 99, rgb255 224 99 99, rgb255 225 100 98, rgb255 226 101 97, rgb255 226 102 96, rgb255 227 104 95, rgb255 228 105 94, rgb255 229 106 93, rgb255 229 107 93, rgb255 230 108 92, rgb255 231 110 91, rgb255 231 111 90, rgb255 232 112 89, rgb255 233 113 88, rgb255 233 114 87, rgb255 234 116 87, rgb255 235 117 86, rgb255 235 118 85, rgb255 236 119 84, rgb255 237 121 83, rgb255 237 122 82, rgb255 238 123 81, rgb255 239 124 81, rgb255 239 126 80, rgb255 240 127 79, rgb255 240 128 78, rgb255 241 129 77, rgb255 241 131 76, rgb255 242 132 75, rgb255 243 133 75, rgb255 243 135 74, rgb255 244 136 73, rgb255 244 137 72, rgb255 245 139 71, rgb255 245 140 70, rgb255 246 141 69, rgb255 246 143 68, rgb255 247 144 68, rgb255 247 145 67, rgb255 247 147 66, rgb255 248 148 65, rgb255 248 149 64, rgb255 249 151 63, rgb255 249 152 62, rgb255 249 154 62, rgb255 250 155 61, rgb255 250 156 60, rgb255 250 158 59, rgb255 251 159 58, rgb255 251 161 57, rgb255 251 162 56, rgb255 252 163 56, rgb255 252 165 55, rgb255 252 166 54, rgb255 252 168 53, rgb255 252 169 52, rgb255 253 171 51, rgb255 253 172 51, rgb255 253 174 50, rgb255 253 175 49, rgb255 253 177 48, rgb255 253 178 47, rgb255 253 180 47, rgb255 253 181 46, rgb255 254 183 45, rgb255 254 184 44, rgb255 254 186 44, rgb255 254 187 43, rgb255 254 189 42, rgb255 254 190 42, rgb255 254 192 41, rgb255 253 194 41, rgb255 253 195 40, rgb255 253 197 39, rgb255 253 198 39, rgb255 253 200 39, rgb255 253 202 38, rgb255 253 203 38, rgb255 252 205 37, rgb255 252 206 37, rgb255 252 208 37, rgb255 252 210 37, rgb255 251 211 36, rgb255 251 213 36, rgb255 251 215 36, rgb255 250 216 36, rgb255 250 218 36, rgb255 249 220 36, rgb255 249 221 37, rgb255 248 223 37, rgb255 248 225 37, rgb255 247 226 37, rgb255 247 228 37, rgb255 246 230 38, rgb255 246 232 38, rgb255 245 233 38, rgb255 245 235 39, rgb255 244 237 39, rgb255 243 238 39, rgb255 243 240 39, rgb255 242 242 39, rgb255 241 244 38, rgb255 241 245 37, rgb255 240 247 36, rgb255 240 249 33 ]


{-| ![turbo](https://code.gampleman.eu/elm-visualization/misc/turbo.png)

The “turbo” color scheme by [Anton Mikhailov](https://ai.googleblog.com/2019/08/turbo-improved-rainbow-colormap-for.html).

-}
turboInterpolator : Float -> Color
turboInterpolator =
    mkInterpolator <|
        Array.fromList
            [ rgb 0.18995 0.07176 0.23217
            , rgb 0.19483 0.08339 0.26149
            , rgb 0.19956 0.09498 0.29024
            , rgb 0.20415 0.10652 0.31844
            , rgb 0.2086 0.11802 0.34607
            , rgb 0.21291 0.12947 0.37314
            , rgb 0.21708 0.14087 0.39964
            , rgb 0.22111 0.15223 0.42558
            , rgb 0.225 0.16354 0.45096
            , rgb 0.22875 0.17481 0.47578
            , rgb 0.23236 0.18603 0.50004
            , rgb 0.23582 0.1972 0.52373
            , rgb 0.23915 0.20833 0.54686
            , rgb 0.24234 0.21941 0.56942
            , rgb 0.24539 0.23044 0.59142
            , rgb 0.2483 0.24143 0.61286
            , rgb 0.25107 0.25237 0.63374
            , rgb 0.25369 0.26327 0.65406
            , rgb 0.25618 0.27412 0.67381
            , rgb 0.25853 0.28492 0.693
            , rgb 0.26074 0.29568 0.71162
            , rgb 0.2628 0.30639 0.72968
            , rgb 0.26473 0.31706 0.74718
            , rgb 0.26652 0.32768 0.76412
            , rgb 0.26816 0.33825 0.7805
            , rgb 0.26967 0.34878 0.79631
            , rgb 0.27103 0.35926 0.81156
            , rgb 0.27226 0.3697 0.82624
            , rgb 0.27334 0.38008 0.84037
            , rgb 0.27429 0.39043 0.85393
            , rgb 0.27509 0.40072 0.86692
            , rgb 0.27576 0.41097 0.87936
            , rgb 0.27628 0.42118 0.89123
            , rgb 0.27667 0.43134 0.90254
            , rgb 0.27691 0.44145 0.91328
            , rgb 0.27701 0.45152 0.92347
            , rgb 0.27698 0.46153 0.93309
            , rgb 0.2768 0.47151 0.94214
            , rgb 0.27648 0.48144 0.95064
            , rgb 0.27603 0.49132 0.95857
            , rgb 0.27543 0.50115 0.96594
            , rgb 0.27469 0.51094 0.97275
            , rgb 0.27381 0.52069 0.97899
            , rgb 0.27273 0.5304 0.98461
            , rgb 0.27106 0.54015 0.9893
            , rgb 0.26878 0.54995 0.99303
            , rgb 0.26592 0.55979 0.99583
            , rgb 0.26252 0.56967 0.99773
            , rgb 0.25862 0.57958 0.99876
            , rgb 0.25425 0.5895 0.99896
            , rgb 0.24946 0.59943 0.99835
            , rgb 0.24427 0.60937 0.99697
            , rgb 0.23874 0.61931 0.99485
            , rgb 0.23288 0.62923 0.99202
            , rgb 0.22676 0.63913 0.98851
            , rgb 0.22039 0.64901 0.98436
            , rgb 0.21382 0.65886 0.97959
            , rgb 0.20708 0.66866 0.97423
            , rgb 0.20021 0.67842 0.96833
            , rgb 0.19326 0.68812 0.9619
            , rgb 0.18625 0.69775 0.95498
            , rgb 0.17923 0.70732 0.94761
            , rgb 0.17223 0.7168 0.93981
            , rgb 0.16529 0.7262 0.93161
            , rgb 0.15844 0.73551 0.92305
            , rgb 0.15173 0.74472 0.91416
            , rgb 0.14519 0.75381 0.90496
            , rgb 0.13886 0.76279 0.8955
            , rgb 0.13278 0.77165 0.8858
            , rgb 0.12698 0.78037 0.8759
            , rgb 0.12151 0.78896 0.86581
            , rgb 0.11639 0.7974 0.85559
            , rgb 0.11167 0.80569 0.84525
            , rgb 0.10738 0.81381 0.83484
            , rgb 0.10357 0.82177 0.82437
            , rgb 0.10026 0.82955 0.81389
            , rgb 0.0975 0.83714 0.80342
            , rgb 0.09532 0.84455 0.79299
            , rgb 0.09377 0.85175 0.78264
            , rgb 0.09287 0.85875 0.7724
            , rgb 0.09267 0.86554 0.7623
            , rgb 0.0932 0.87211 0.75237
            , rgb 0.09451 0.87844 0.74265
            , rgb 0.09662 0.88454 0.73316
            , rgb 0.09958 0.8904 0.72393
            , rgb 0.10342 0.896 0.715
            , rgb 0.10815 0.90142 0.70599
            , rgb 0.11374 0.90673 0.69651
            , rgb 0.12014 0.91193 0.6866
            , rgb 0.12733 0.91701 0.67627
            , rgb 0.13526 0.92197 0.66556
            , rgb 0.14391 0.9268 0.65448
            , rgb 0.15323 0.93151 0.64308
            , rgb 0.16319 0.93609 0.63137
            , rgb 0.17377 0.94053 0.61938
            , rgb 0.18491 0.94484 0.60713
            , rgb 0.19659 0.94901 0.59466
            , rgb 0.20877 0.95304 0.58199
            , rgb 0.22142 0.95692 0.56914
            , rgb 0.23449 0.96065 0.55614
            , rgb 0.24797 0.96423 0.54303
            , rgb 0.2618 0.96765 0.52981
            , rgb 0.27597 0.97092 0.51653
            , rgb 0.29042 0.97403 0.50321
            , rgb 0.30513 0.97697 0.48987
            , rgb 0.32006 0.97974 0.47654
            , rgb 0.33517 0.98234 0.46325
            , rgb 0.35043 0.98477 0.45002
            , rgb 0.36581 0.98702 0.43688
            , rgb 0.38127 0.98909 0.42386
            , rgb 0.39678 0.99098 0.41098
            , rgb 0.41229 0.99268 0.39826
            , rgb 0.42778 0.99419 0.38575
            , rgb 0.44321 0.99551 0.37345
            , rgb 0.45854 0.99663 0.3614
            , rgb 0.47375 0.99755 0.34963
            , rgb 0.48879 0.99828 0.33816
            , rgb 0.50362 0.99879 0.32701
            , rgb 0.51822 0.9991 0.31622
            , rgb 0.53255 0.99919 0.30581
            , rgb 0.54658 0.99907 0.29581
            , rgb 0.56026 0.99873 0.28623
            , rgb 0.57357 0.99817 0.27712
            , rgb 0.58646 0.99739 0.26849
            , rgb 0.59891 0.99638 0.26038
            , rgb 0.61088 0.99514 0.2528
            , rgb 0.62233 0.99366 0.24579
            , rgb 0.63323 0.99195 0.23937
            , rgb 0.64362 0.98999 0.23356
            , rgb 0.65394 0.98775 0.22835
            , rgb 0.66428 0.98524 0.2237
            , rgb 0.67462 0.98246 0.2196
            , rgb 0.68494 0.97941 0.21602
            , rgb 0.69525 0.9761 0.21294
            , rgb 0.70553 0.97255 0.21032
            , rgb 0.71577 0.96875 0.20815
            , rgb 0.72596 0.9647 0.2064
            , rgb 0.7361 0.96043 0.20504
            , rgb 0.74617 0.95593 0.20406
            , rgb 0.75617 0.95121 0.20343
            , rgb 0.76608 0.94627 0.20311
            , rgb 0.77591 0.94113 0.2031
            , rgb 0.78563 0.93579 0.20336
            , rgb 0.79524 0.93025 0.20386
            , rgb 0.80473 0.92452 0.20459
            , rgb 0.8141 0.91861 0.20552
            , rgb 0.82333 0.91253 0.20663
            , rgb 0.83241 0.90627 0.20788
            , rgb 0.84133 0.89986 0.20926
            , rgb 0.8501 0.89328 0.21074
            , rgb 0.85868 0.88655 0.2123
            , rgb 0.86709 0.87968 0.21391
            , rgb 0.8753 0.87267 0.21555
            , rgb 0.88331 0.86553 0.21719
            , rgb 0.89112 0.85826 0.2188
            , rgb 0.8987 0.85087 0.22038
            , rgb 0.90605 0.84337 0.22188
            , rgb 0.91317 0.83576 0.22328
            , rgb 0.92004 0.82806 0.22456
            , rgb 0.92666 0.82025 0.2257
            , rgb 0.93301 0.81236 0.22667
            , rgb 0.93909 0.80439 0.22744
            , rgb 0.94489 0.79634 0.228
            , rgb 0.95039 0.78823 0.22831
            , rgb 0.9556 0.78005 0.22836
            , rgb 0.96049 0.77181 0.22811
            , rgb 0.96507 0.76352 0.22754
            , rgb 0.96931 0.75519 0.22663
            , rgb 0.97323 0.74682 0.22536
            , rgb 0.97679 0.73842 0.22369
            , rgb 0.98 0.73 0.22161
            , rgb 0.98289 0.7214 0.21918
            , rgb 0.98549 0.7125 0.2165
            , rgb 0.98781 0.7033 0.21358
            , rgb 0.98986 0.69382 0.21043
            , rgb 0.99163 0.68408 0.20706
            , rgb 0.99314 0.67408 0.20348
            , rgb 0.99438 0.66386 0.19971
            , rgb 0.99535 0.65341 0.19577
            , rgb 0.99607 0.64277 0.19165
            , rgb 0.99654 0.63193 0.18738
            , rgb 0.99675 0.62093 0.18297
            , rgb 0.99672 0.60977 0.17842
            , rgb 0.99644 0.59846 0.17376
            , rgb 0.99593 0.58703 0.16899
            , rgb 0.99517 0.57549 0.16412
            , rgb 0.99419 0.56386 0.15918
            , rgb 0.99297 0.55214 0.15417
            , rgb 0.99153 0.54036 0.1491
            , rgb 0.98987 0.52854 0.14398
            , rgb 0.98799 0.51667 0.13883
            , rgb 0.9859 0.50479 0.13367
            , rgb 0.9836 0.49291 0.12849
            , rgb 0.98108 0.48104 0.12332
            , rgb 0.97837 0.4692 0.11817
            , rgb 0.97545 0.4574 0.11305
            , rgb 0.97234 0.44565 0.10797
            , rgb 0.96904 0.43399 0.10294
            , rgb 0.96555 0.42241 0.09798
            , rgb 0.96187 0.41093 0.0931
            , rgb 0.95801 0.39958 0.08831
            , rgb 0.95398 0.38836 0.08362
            , rgb 0.94977 0.37729 0.07905
            , rgb 0.94538 0.36638 0.07461
            , rgb 0.94084 0.35566 0.07031
            , rgb 0.93612 0.34513 0.06616
            , rgb 0.93125 0.33482 0.06218
            , rgb 0.92623 0.32473 0.05837
            , rgb 0.92105 0.31489 0.05475
            , rgb 0.91572 0.3053 0.05134
            , rgb 0.91024 0.29599 0.04814
            , rgb 0.90463 0.28696 0.04516
            , rgb 0.89888 0.27824 0.04243
            , rgb 0.89298 0.26981 0.03993
            , rgb 0.88691 0.26152 0.03753
            , rgb 0.88066 0.25334 0.03521
            , rgb 0.87422 0.24526 0.03297
            , rgb 0.8676 0.2373 0.03082
            , rgb 0.86079 0.22945 0.02875
            , rgb 0.8538 0.2217 0.02677
            , rgb 0.84662 0.21407 0.02487
            , rgb 0.83926 0.20654 0.02305
            , rgb 0.83172 0.19912 0.02131
            , rgb 0.82399 0.19182 0.01966
            , rgb 0.81608 0.18462 0.01809
            , rgb 0.80799 0.17753 0.0166
            , rgb 0.79971 0.17055 0.0152
            , rgb 0.79125 0.16368 0.01387
            , rgb 0.7826 0.15693 0.01264
            , rgb 0.77377 0.15028 0.01148
            , rgb 0.76476 0.14374 0.01041
            , rgb 0.75556 0.13731 0.00942
            , rgb 0.74617 0.13098 0.00851
            , rgb 0.73661 0.12477 0.00769
            , rgb 0.72686 0.11867 0.00695
            , rgb 0.71692 0.11268 0.00629
            , rgb 0.7068 0.1068 0.00571
            , rgb 0.6965 0.10102 0.00522
            , rgb 0.68602 0.09536 0.00481
            , rgb 0.67535 0.0898 0.00449
            , rgb 0.66449 0.08436 0.00424
            , rgb 0.65345 0.07902 0.00408
            , rgb 0.64223 0.0738 0.00401
            , rgb 0.63082 0.06868 0.00401
            , rgb 0.61923 0.06367 0.0041
            , rgb 0.60746 0.05878 0.00427
            , rgb 0.5955 0.05399 0.00453
            , rgb 0.58336 0.04931 0.00486
            , rgb 0.57103 0.04474 0.00529
            , rgb 0.55852 0.04028 0.00579
            , rgb 0.54583 0.03593 0.00638
            , rgb 0.53295 0.03169 0.00705
            , rgb 0.51989 0.02756 0.0078
            , rgb 0.50664 0.02354 0.00863
            , rgb 0.49321 0.01963 0.00955
            , rgb 0.4796 0.01583 0.01055
            ]


{-| ![category10](https://code.gampleman.eu/elm-visualization/misc/category10.png)

A list of ten categorical colors

-}
category10 : List Color
category10 =
    [ rgb255 31 119 180, rgb255 255 127 14, rgb255 44 160 44, rgb255 214 39 40, rgb255 148 103 189, rgb255 140 86 75, rgb255 227 119 194, rgb255 127 127 127, rgb255 188 189 34, rgb255 23 190 207 ]


{-| ![accent](https://code.gampleman.eu/elm-visualization/misc/accent.png)

A list of eight categorical colors

-}
accent : List Color
accent =
    "7fc97fbeaed4fdc086ffff99386cb0f0027fbf5b17666666"
        |> toHexColorStrings
        |> List.map hexToColor


{-| ![category10](https://code.gampleman.eu/elm-visualization/misc/tableau10.png)

A list of ten categorical colors

-}
tableau10 : List Color
tableau10 =
    [ rgb255 78 121 167, rgb255 242 142 44, rgb255 225 87 89, rgb255 118 183 178, rgb255 89 161 79, rgb255 237 201 73, rgb255 175 122 161, rgb255 255 157 167, rgb255 156 117 95, rgb255 186 176 171 ]


{-| ![pastel1](https://code.gampleman.eu/elm-visualization/misc/pastel1.png)

A list of nine categorical pastel colors

-}
pastel1 : List Color
pastel1 =
    "fbb4aeb3cde3ccebc5decbe4fed9a6ffffcce5d8bdfddaecf2f2f2"
        |> toHexColorStrings
        |> List.map hexToColor


{-| ![pastel2](https://code.gampleman.eu/elm-visualization/misc/pastel2.png)

A list of eight categorical pastel colors

-}
pastel2 : List Color
pastel2 =
    "b3e2cdfdcdaccbd5e8f4cae4e6f5c9fff2aef1e2cccccccc"
        |> toHexColorStrings
        |> List.map hexToColor


{-| ![paired](https://code.gampleman.eu/elm-visualization/misc/paired.png)

A list of twelve categorical paired colors

-}
paired : List Color
paired =
    "a6cee31f78b4b2df8a33a02cfb9a99e31a1cfdbf6fff7f00cab2d66a3d9affff99b15928"
        |> toHexColorStrings
        |> List.map hexToColor


{-| ![colorblind](https://code.gampleman.eu/elm-visualization/misc/colorblind.png)

A list of eight colorblind friendly categorical colors

-}
colorblind : List Color
colorblind =
    "0x0072b2e69f00f0e442009e7356b4e9d55e00cc79a7000000"
        |> toHexColorStrings
        |> List.map hexToColor


{-| ![set1](https://code.gampleman.eu/elm-visualization/misc/set1.png)

A list of nine categorical colors

-}
set1 : List Color
set1 =
    "e41a1c377eb84daf4a984ea3ff7f00ffff33a65628f781bf999999"
        |> toHexColorStrings
        |> List.map hexToColor


{-| ![set2](https://code.gampleman.eu/elm-visualization/misc/set2.png)

A list of eight categorical colors

-}
set2 : List Color
set2 =
    "66c2a5fc8d628da0cbe78ac3a6d854ffd92fe5c494b3b3b3"
        |> toHexColorStrings
        |> List.map hexToColor



-- ALERT PALETTE


{-| ![carbonAlert](https://code.gampleman.eu/elm-visualization/misc/carbonAlert.png)

A list of alert colors from the [Carbon Design System](https://www.carbondesignsystem.com/data-visualization)

-}
carbonAlert : List Color
carbonAlert =
    "da1e28ff832bf1c21b24a148"
        |> toHexColorStrings
        |> List.map hexToColor



-- CYCLIC


{-| ![rainbow](https://code.gampleman.eu/elm-visualization/misc/rainbow.png)
-}
rainbowInterpolator : Float -> Color
rainbowInterpolator =
    mkPiecewiseInterpolator "6e40aa883eb1a43db3bf3cafd83fa4ee4395fe4b83ff576eff6659ff7847ff8c38f3a130e2b72fcfcc36bee044aff05b8ff4576ff65b52f6673af27828ea8d1ddfa319d0b81cbecb23abd82f96e03d82e14c6edb5a5dd0664dbf6e40aa"


{-| ![sinebow](https://code.gampleman.eu/elm-visualization/misc/sinebow.png)
-}
sinebowInterpolator : Float -> Color
sinebowInterpolator =
    mkPiecewiseInterpolator "ff4040fc582af47218e78d0bd5a703bfbf00a7d5038de70b72f41858fc2a40ff402afc5818f4720be78d03d5a700bfbf03a7d50b8de71872f42a58fc4040ff582afc7218f48d0be7a703d5bf00bfd503a7e70b8df41872fc2a58ff4040"



-- CONTINOUS


{-| ![blues](https://code.gampleman.eu/elm-visualization/misc/blues.png)
-}
bluesInterpolator : Float -> Color
bluesInterpolator =
    mkPiecewiseInterpolator "cfe1f2bed8eca8cee58fc1de74b2d75ba3cf4592c63181bd206fb2125ca40a4a90"


{-| ![greens](https://code.gampleman.eu/elm-visualization/misc/greens.png)
-}
greensInterpolator : Float -> Color
greensInterpolator =
    mkPiecewiseInterpolator "d3eecdc0e6baabdda594d3917bc77d60ba6c46ab5e329a512089430e7735036429"


{-| ![greys](https://code.gampleman.eu/elm-visualization/misc/greys.png)
-}
greysInterpolator : Float -> Color
greysInterpolator =
    mkPiecewiseInterpolator "e2e2e2d4d4d4c4c4c4b1b1b19d9d9d8888887575756262624d4d4d3535351e1e1e"


{-| ![oranges](https://code.gampleman.eu/elm-visualization/misc/oranges.png)
-}
orangesInterpolator : Float -> Color
orangesInterpolator =
    mkPiecewiseInterpolator "fdd8b3fdc998fdb87bfda55efc9244f87f2cf06b18e4580bd14904b93d029f3303"


{-| ![purples](https://code.gampleman.eu/elm-visualization/misc/purples.png)
-}
purplesInterpolator : Float -> Color
purplesInterpolator =
    mkPiecewiseInterpolator "e2e1efd4d4e8c4c5e0b4b3d6a3a0cc928ec3827cb97566ae684ea25c3696501f8c"


{-| ![reds](https://code.gampleman.eu/elm-visualization/misc/reds.png)
-}
redsInterpolator : Float -> Color
redsInterpolator =
    mkPiecewiseInterpolator "fdc9b4fcb49afc9e80fc8767fa7051f6573fec3f2fdc2a25c81b1db21218970b13"


{-| ![blue-greens](https://code.gampleman.eu/elm-visualization/misc/blue-greens.png)
-}
blueGreenInterpolator : Float -> Color
blueGreenInterpolator =
    mkPiecewiseInterpolator "d5efedc1e8e0a7ddd18bd2be70c6a958ba9144ad77319c5d2089460e7736036429"


{-| ![blue-purples](https://code.gampleman.eu/elm-visualization/misc/blue-purples.png)
-}
bluePurpleInterpolator : Float -> Color
bluePurpleInterpolator =
    mkPiecewiseInterpolator "ccddecbad0e4a8c2dd9ab0d4919cc98d85be8b6db28a55a6873c99822287730f71"


{-| ![green-blues](https://code.gampleman.eu/elm-visualization/misc/green-blues.png)
-}
greenBlueInterpolator : Float -> Color
greenBlueInterpolator =
    mkPiecewiseInterpolator "d3eecec5e8c3b1e1bb9bd8bb82cec269c2ca51b2cd3c9fc7288abd1675b10b60a1"


{-| ![orange-reds](https://code.gampleman.eu/elm-visualization/misc/orange-reds.png)
-}
orangeRedInterpolator : Float -> Color
orangeRedInterpolator =
    mkPiecewiseInterpolator "fddcaffdcf9bfdc18afdad77fb9562f67d53ee6545e24932d32d1ebf130da70403"


{-| ![purples-blues](https://code.gampleman.eu/elm-visualization/misc/purples-blues.png)
-}
purpleBlueInterpolator : Float -> Color
purpleBlueInterpolator =
    mkPiecewiseInterpolator "dbdaebc8cee4b1c3de97b7d87bacd15b9fc93a90c01e7fb70b70ab056199045281"


{-| ![purple-blue-greens](https://code.gampleman.eu/elm-visualization/misc/purple-blue-greens.png)
-}
purpleBlueGreenInterpolator : Float -> Color
purpleBlueGreenInterpolator =
    mkPiecewiseInterpolator "dbd8eac8cee4b0c3de93b7d872acd1549fc83892bb1c88a3097f8702736b016353"


{-| ![purple-reds](https://code.gampleman.eu/elm-visualization/misc/purple-reds.png)
-}
purpleRedInterpolator : Float -> Color
purpleRedInterpolator =
    mkPiecewiseInterpolator "dcc9e2d3b3d7ce9eccd186c0da6bb2e14da0e23189d91e6fc61159ab07498f023a"


{-| ![red-purples](https://code.gampleman.eu/elm-visualization/misc/red-purples.png)
-}
redPurpleInterpolator : Float -> Color
redPurpleInterpolator =
    mkPiecewiseInterpolator "fccfccfcbec0faa9b8f98faff571a5ec539ddb3695c41b8aa908808d0179700174"


{-| ![yellow-greens](https://code.gampleman.eu/elm-visualization/misc/yellow-greens.png)
-}
yellowGreenInterpolator : Float -> Color
yellowGreenInterpolator =
    mkPiecewiseInterpolator "e4f4acd1eca0b9e2949ed68880c97c62bb6e47aa5e3297502083440e723b036034"


{-| ![yellow-orange-browns](https://code.gampleman.eu/elm-visualization/misc/yellow-orange-browns.png)
-}
yellowOrangeBrownInterpolator : Float -> Color
yellowOrangeBrownInterpolator =
    mkPiecewiseInterpolator "feeaa1fedd84fecc63feb746fca031f68921eb7215db5e0bc54c05ab3d038f3204"


{-| ![yellow-orange-reds](https://code.gampleman.eu/elm-visualization/misc/yellow-orange-reds.png)
-}
yellowOrangeRedInterpolator : Float -> Color
yellowOrangeRedInterpolator =
    mkPiecewiseInterpolator "fee087fed16ffebd59fea849fd903efc7335f9522bee3423de1b20ca0b22af0225"


{-| ![browns](https://code.gampleman.eu/elm-visualization/misc/browns.png)
-}
brownsInterpolator : Float -> Color
brownsInterpolator =
    mkPiecewiseInterpolator "eedbbdecca96e9b97ae4a865dc9856d18954c7784cc0673fb85536ad44339f3632"


{-| ![teal-blues](https://code.gampleman.eu/elm-visualization/misc/teal-blues.png)
-}
tealBluesInterpolator : Float -> Color
tealBluesInterpolator =
    mkPiecewiseInterpolator "bce4d89dd3d181c3cb65b3c245a2b9368fae347da0306a932c5985"


{-| ![teals](https://code.gampleman.eu/elm-visualization/misc/teals.png)
-}
tealInterpolator : Float -> Color
tealInterpolator =
    mkPiecewiseInterpolator "bbdfdfa2d4d58ac9c975bcbb61b0af4da5a43799982b8b8c1e7f7f127273006667"


{-| ![warm-greys](https://code.gampleman.eu/elm-visualization/misc/warm-greys.png)
-}
warmGreysInterpolator : Float -> Color
warmGreysInterpolator =
    mkPiecewiseInterpolator "dcd4d0cec5c1c0b8b4b3aaa7a59c9998908c8b827f7e7673726866665c5a59504e"


{-| ![gold-greens](https://code.gampleman.eu/elm-visualization/misc/gold-greens.png)
-}
goldGreensInterpolator : Float -> Color
goldGreensInterpolator =
    mkPiecewiseInterpolator "f4d166d5ca60b6c35c98bb597cb25760a6564b9c533f8f4f33834a257740146c36"


{-| ![gold-oranges](https://code.gampleman.eu/elm-visualization/misc/gold-oranges.png)
-}
goldOrangeInterpolator : Float -> Color
goldOrangeInterpolator =
    mkPiecewiseInterpolator "f4d166f8be5cf8aa4cf5983bf3852aef701be2621fd65322c54923b142239e3a26"


{-| ![gold-reds](https://code.gampleman.eu/elm-visualization/misc/gold-reds.png)
-}
goldRedInterpolator : Float -> Color
goldRedInterpolator =
    mkPiecewiseInterpolator "f4d166f6be59f9aa51fc964ef6834bee734ae56249db5247cf4244c43141b71d3e"


{-| ![light-grey-reds](https://code.gampleman.eu/elm-visualization/misc/light-grey-reds.png)
-}
lightGreyRedInterpolator : Float -> Color
lightGreyRedInterpolator =
    mkPiecewiseInterpolator "efe9e6e1dad7d5cbc8c8bdb9bbaea9cd967ddc7b43e15f19df4011dc000b"


{-| ![light-grey-teals](https://code.gampleman.eu/elm-visualization/misc/light-grey-teals.png)
-}
lightGreyTealInterpolator : Float -> Color
lightGreyTealInterpolator =
    mkPiecewiseInterpolator "e4eaead6dcddc8ced2b7c2c7a6b4bc64b0bf22a6c32295c11f85be1876bc"


{-| ![light-multi](https://code.gampleman.eu/elm-visualization/misc/light-multi.png)
-}
lightMultiInterpolator : Float -> Color
lightMultiInterpolator =
    mkPiecewiseInterpolator "e0f1f2c4e9d0b0de9fd0e181f6e072f6c053f3993ef77440ef4a3c"


{-| ![light-oranges](https://code.gampleman.eu/elm-visualization/misc/light-oranges.png)
-}
lightOrangeInterpolator : Float -> Color
lightOrangeInterpolator =
    mkPiecewiseInterpolator "f2e7daf7d5baf9c499fab184fa9c73f68967ef7860e8645bde515bd43d5b"



-- DIVERGING


{-| ![carbon-palette1](https://code.gampleman.eu/elm-visualization/misc/carbon-palette1.png)

The “Carbon palette1” diverging color scheme, from the [Carbon Design System](https://www.carbondesignsystem.com/data-visualization/color-palettes)

The red-cyan palette has a natural association with temperature. Use this palette for data representing hot-vs-cold.

-}
carbonDiverging1Interpolator : Float -> Color
carbonDiverging1Interpolator =
    mkPiecewiseInterpolator "750e13a2191fda1e28fa4d56ff8389ffb3b8ffd7d9fff1f1e5f6ffbae6ff82cfff33b1ff1192e80072c300539a003a6d"


{-| ![carbon-palette2](https://code.gampleman.eu/elm-visualization/misc/carbon-palette2.png)

The “Carbon palette2” diverging color scheme, from the [Carbon Design System](https://www.carbondesignsystem.com/data-visualization/color-palettes)

The purple-teal palette is good for data with no temperature associations, such as performance, sales, and rates of change.

-}
carbonDiverging2Interpolator : Float -> Color
carbonDiverging2Interpolator =
    mkPiecewiseInterpolator "491d8b6929c48a3ffca56effbe95ffd4bbffe8dafff6f2ffd9fbfb9ef0f03ddbd908bdba009d9a007d79005d5d004144"


{-| ![blue-oranges](https://code.gampleman.eu/elm-visualization/misc/blue-oranges.png)
-}
blueOrangeInterpolator : Float -> Color
blueOrangeInterpolator =
    mkPiecewiseInterpolator "134b852f78b35da2cb9dcae1d2e5eff2f0ebfce0bafbbf74e8932fc5690d994a07"


{-| ![brown-blue-greens](https://code.gampleman.eu/elm-visualization/misc/brown-blue-greens.png)
-}
brownBlueGreenInterpolator : Float -> Color
brownBlueGreenInterpolator =
    mkPiecewiseInterpolator "704108a0651ac79548e3c78af3e6c6eef1eac9e9e48ed1c74da79e187a72025147"


{-| ![purple-greens](https://code.gampleman.eu/elm-visualization/misc/purple-greens.png)
-}
purpleGreenInterpolator : Float -> Color
purpleGreenInterpolator =
    mkPiecewiseInterpolator "5b1667834792a67fb6c9aed3e6d6e8eff0efd9efd5aedda971bb75368e490e5e29"


{-| ![purple-oranges](https://code.gampleman.eu/elm-visualization/misc/purple-oranges.png)
-}
purpleOrangeInterpolator : Float -> Color
purpleOrangeInterpolator =
    mkPiecewiseInterpolator "4114696647968f83b7b9b4d6dadbebf3eeeafce0bafbbf74e8932fc5690d994a07"


{-| ![red-blues](https://code.gampleman.eu/elm-visualization/misc/red-blues.png)
-}
redBlueInterpolator : Float -> Color
redBlueInterpolator =
    mkPiecewiseInterpolator "8c0d25bf363adf745ef4ae91fbdbc9f2efeed2e5ef9dcae15da2cb2f78b3134b85"


{-| ![red-greys](https://code.gampleman.eu/elm-visualization/misc/red-greys.png)
-}
redGreyInterpolator : Float -> Color
redGreyInterpolator =
    mkPiecewiseInterpolator "8c0d25bf363adf745ef4ae91fcdccbfaf4f1e2e2e2c0c0c0969696646464343434"


{-| ![yellow-green-blues](https://code.gampleman.eu/elm-visualization/misc/yellow-green-blues.png)
-}
yellowGreenBlueInterpolator : Float -> Color
yellowGreenBlueInterpolator =
    mkPiecewiseInterpolator "eff9bddbf1b4bde5b594d5b969c5be45b4c22c9ec02182b82163aa23479c1c3185"


{-| ![red-yellow-blues](https://code.gampleman.eu/elm-visualization/misc/red-yellow-blues.png)
-}
redYellowBlueInterpolator : Float -> Color
redYellowBlueInterpolator =
    mkPiecewiseInterpolator "a50026d4322cf16e43fcac64fedd90faf8c1dcf1ecabd6e875abd04a74b4313695"


{-| ![red-yellow-greens](https://code.gampleman.eu/elm-visualization/misc/red-yellow-greens.png)
-}
redYellowGreenInterpolator : Float -> Color
redYellowGreenInterpolator =
    mkPiecewiseInterpolator "a50026d4322cf16e43fcac63fedd8df9f7aed7ee8ea4d86e64bc6122964f006837"


{-| ![pink-yellow-greens](https://code.gampleman.eu/elm-visualization/misc/pink-yellow-greens.png)
-}
pinkYellowGreenInterpolator : Float -> Color
pinkYellowGreenInterpolator =
    mkPiecewiseInterpolator "8e0152c0267edd72adf0b3d6faddedf5f3efe1f2cab6de8780bb474f9125276419"


{-| ![spectral](https://code.gampleman.eu/elm-visualization/misc/spectral.png)
-}
spectralInterpolator : Float -> Color
spectralInterpolator =
    mkPiecewiseInterpolator "9e0142d13c4bf0704afcac63fedd8dfbf8b0e0f3a1a9dda269bda94288b55e4fa2"



-- HELPERS


mkInterpolator : Array Color -> Float -> Color
mkInterpolator range =
    let
        n =
            Array.length range
    in
    \t ->
        Maybe.withDefault black <| Array.get (max 0 (min (n - 1) (floor (t * toFloat n)))) range


mkPiecewiseInterpolator : String -> (Float -> Color)
mkPiecewiseInterpolator values =
    let
        hexColors =
            toHexColorStrings values

        head =
            hexColors
                |> List.head
                |> Maybe.withDefault "#fff"
                |> hexToColor

        tail =
            hexColors
                |> List.tail
                |> Maybe.withDefault []
                |> List.map hexToColor
    in
    Interpolation.piecewise Interpolation.rgb head tail


toHexColorStrings : String -> List String
toHexColorStrings palette =
    let
        n =
            (String.length palette |> toFloat) / 6 |> round

        f =
            \i ->
                "#" ++ String.slice (i * 6) ((i + 1) * 6) palette
    in
    Array.initialize n f
        |> Array.toList


{-| Hexadecimal color string to Color
-}
hexToColor : String -> Color
hexToColor hex =
    hex
        |> String.dropLeft 1
        |> (\s ->
                let
                    r =
                        String.slice 0 2 s
                            |> Hex.fromString
                            |> Result.withDefault 0

                    g =
                        String.slice 2 4 s
                            |> Hex.fromString
                            |> Result.withDefault 0

                    b =
                        String.slice 4 6 s
                            |> Hex.fromString
                            |> Result.withDefault 0
                in
                rgb255 r g b
           )
