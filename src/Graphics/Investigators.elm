module Graphics.Investigators exposing (start, end, connections, minimalData, characterCard)

import BoardData exposing(..)
import Selection exposing (Selection)
import Graphics.Common exposing (..)
import Svg exposing (Svg, path, circle, line, rect, text', text, Attribute, image, ellipse)
import Svg.Attributes exposing (d, fill, cx, cy, r, strokeWidth, stroke, strokeDasharray, fillOpacity, x1, x2, y1, y2, strokeLinecap, x , y, width, height, textAnchor, fontFamily, fontSize, xlinkHref, rx, ry)
import String
import Skills

withMargin m r =
    Rectangle (r.x + m) (r.y + m) (r.width - m * 2) (r.height - m * 2)

minimalData: (Investigator -> Attribute a) -> Int -> Selection (Investigator, Color, Int) -> List (Svg a)
minimalData msgGenerator index selection =
    let
        (investigator, color, movesLeft) = Selection.unpack selection
        dashArray = if Selection.isSelected selection then "2 5" else ""
        outline = Rectangle (index * smallInvestigatorDim.width % sideDim.width) ((index // 4) * smallInvestigatorDim.height) smallInvestigatorDim.width smallInvestigatorDim.height |> withMargin 3
        middle = rectangleMiddle outline
        investigatorInfo = String.concat [investigator.name, " ", toString movesLeft]
    in
        text' [textAnchor "middle", x <| toString <| middle.x, y <| toString <| middle.y, fontFamily "Verdana", fontSize "15"][text investigatorInfo]
        :: rect [x <| toString <| outline.x, y <| toString <| outline.y, width <| toString outline.width, height <| toString outline.height, stroke color, strokeWidth "3", strokeDasharray dashArray, fillOpacity "0.0", msgGenerator investigator][]
        ::[]

characterCard: ((Skills.SkillSet, Int) -> List (Attribute a)) -> Maybe (Investigator, Skills.SkillAdjustments) -> List (Svg a)
characterCard msgGenerator investigatorData =
    let
        xCoord = (sideDim.width - investigatorCardDim.width) // 2
        yCoord = smallInvestigatorDim.height * 2
    in
         Maybe.map (drawInvestigatorCard msgGenerator xCoord yCoord) investigatorData |> Maybe.withDefault []

drawInvestigatorCard msgGenerator xCoord yCoord (inv, adjustments) =
    let
        currentEllipses = List.map (\(ss, v) -> (sliderEllipse (ss, v), []))  <| Skills.getCurrentAdjustments adjustments
        possibleEllipses = List.map (\(ss, v) -> (sliderEllipse (ss, v), msgGenerator (ss, v))) <| Skills.getPossibleAdjustments adjustments
        svgEllipse s (e, msg) =
            ellipse (List.append [xCoord + e.x |> toString |> cx, yCoord + e.y |> toString |> cy, e.xRadius |> toString |> rx, e.yRadius |> toString |> ry, fillOpacity "0.0", strokeWidth "3", stroke s]  msg)[]
        svgEllipses = List.concat [List.map (svgEllipse "black") currentEllipses, List.map (svgEllipse "none") possibleEllipses]
    in
        List.append [image [xlinkHref inv.card, x <| toString xCoord, y <| toString yCoord, width <| toString investigatorCardDim.width, height <| toString investigatorCardDim.height][]] svgEllipses

connections : ((Place, Place), List Color) -> List (Svg a)
connections ((start, end), colors) =
    List.indexedMap (calculateLine (middle start) (middle end) (List.length colors)) colors

calculateLine p1 p2 total index color =
    let
        height = 3 * total // 2 - (index * 3)
        start = Point p1.x (p1.y + height)
        end = Point p2.x (p2.y + height)
    in
        line [x1 <| toString start.x, y1 <| toString start.y, x2 <| toString end.x, y2 <| toString end.y, stroke color, strokeWidth "3", strokeLinecap "round"] []

end : (Place, List Color) -> List (Svg a)
end (p, colors) =
    List.indexedMap (calculateCircle (middle p) 24) colors

calculateCircle middle radius index color =
    circle [cx <| toString (middle.x + 30), cy <| toString middle.y, r <| toString (radius - 3 * index), strokeWidth "3", stroke color, fillOpacity "0.0"][]

start : (Place, List Color) -> List (Svg a)
start (p, colors)  =
    let
        m = middle p
    in
        drawPies (Point (m.x + 30) m.y) 24 colors

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