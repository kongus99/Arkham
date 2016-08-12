module Graphics.Investigators exposing (start, end, connections)

import BoardData exposing(..)
import Graphics.Common exposing (..)
import Svg exposing (Svg, path, circle)
import Svg.Attributes exposing (d, fill, cx, cy, r)
import String

connections = []

end = []

start : (Place, List Color) -> List (Svg a)
start (p, colors)  =
    let
        m = middle p
    in
        drawPies (Point (m.x + 30) m.y) 20 colors

drawPies : Point -> Int -> List Color -> List (Svg a)
drawPies middle radius colors =
    if List.length colors == 1 then
        [circle [cx <| toString middle.x, cy <| toString middle.y, r <| toString radius, fill <| Maybe.withDefault "green" <| List.head colors][]]
    else List.indexedMap (calculatePie middle radius (List.length colors)) colors

calculatePie middle radius total index color =
    let
        angle1 = (toFloat index) * 2 * pi / (toFloat total)
        x1 = ceiling (toFloat middle.x + toFloat radius * sin angle1)
        y1 = ceiling (toFloat middle.y + toFloat radius * cos angle1)
        angle2 = (toFloat (index + 1)) * 2 * pi / (toFloat total)
        x2 = ceiling (toFloat middle.x + toFloat radius * sin angle2)
        y2 = ceiling (toFloat middle.y + toFloat radius * cos angle2)
        toDraw = String.concat
            [" M ", toString x1, " ", toString y1,
             " A ", toString radius, " ", toString radius, ", 0, 0, 0, ", toString x2, " ", toString y2,
             " L ", toString middle.x, " ", toString middle.y,
             " Z"]
    in
        path [d toDraw, fill color][]