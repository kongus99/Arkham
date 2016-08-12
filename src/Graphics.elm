module Graphics exposing (..)

import BoardData exposing(..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import List
import List.Extra as Lists
import MonsterBowl exposing (Monster)
import String
import Graphics.Investigators as Investigators
import Graphics.Common exposing (..)

localeCircle : (Location -> List(Attribute a)) -> Location -> Svg a
localeCircle generator l =
    let
        c = locationCircle l
        commonAttributes = [cx <| toString c.cx, cy <| toString c.cy, r <| toString c.radius, strokeWidth "1", strokeOpacity "0.0", fillOpacity "0.0"]
        generatedAttributes = generator l
    in
        circle (List.append commonAttributes generatedAttributes) []

streetRectangle : (Neighborhood -> List(Attribute a)) -> Neighborhood -> Svg a
streetRectangle generator n =
    let
        r = neighborhoodRectangle n
        commonAttributes = [x <| toString r.x, y <| toString r.y, width <| toString r.width, height <| toString r.height, strokeOpacity "0.0", fillOpacity "0.0"]
        generatedAttributes = generator n
    in
        rect (List.append commonAttributes generatedAttributes) []

monsterSquare : (Place, List Monster) -> List (Svg a)
monsterSquare (place, monsters) =
    let
        mid = middle place
        rectX = mid.x - 60
        rectY = mid.y - 20
        side = 40
        textX= rectX + 33
        textY = rectY + 38
    in
        rect [x <| toString <| rectX, y <| toString <| rectY, width <| toString side, height <| toString side, fill "red"][]
      ::text' [textAnchor "middle", x <| toString <| textX, y <| toString <| textY][text (toString <| List.length monsters)]
      ::[]


boardDim = Dimension 1606 2384
checkDim = Dimension 150 225

investigatorDim = Dimension 350 493

leftOffsets number maxWidth tileWidth maxInRow =
    let
        remainder = number % maxInRow
        leftMargin (i, isFull) =
            let lm = if isFull then (maxWidth % tileWidth) // 2 else (maxWidth - remainder * tileWidth) // 2
            in lm + (i  * tileWidth)
        indexes = List.scanl (\x -> \y -> (x + y) % maxInRow) 0 (List.repeat number 1)
        isFullRow = List.append (List.repeat (number - remainder) True) (List.repeat remainder False)
    in
       List.map leftMargin <| (Lists.zip indexes isFullRow)

topOffsets number maxHeight tileHeight maxInRow =
    let
        numOfRows = ceiling (toFloat number / toFloat maxInRow)
        topMargin i = (maxHeight - numOfRows * tileHeight) // 2 + (i  * tileHeight)
        indexes = List.scanl (+) 0 (List.repeat (numOfRows - 1) 1)
        rowTopMargins = List.map topMargin indexes
    in
        List.concat <| List.map (List.repeat maxInRow) rowTopMargins

createRectangle width height leftMargin topMargin (x,y) =
    let
        rx = x + leftMargin
        ry = y + topMargin
        middle =  {x = rx + width // 2, y = ry + height // 2}
    in
        {x = rx, y = ry, width = width , height = height, middle = middle}

calculateCheckerPositions number =
    let
        checkDimWithMargins = Dimension (checkDim.width * 4 // 3) (checkDim.height * 4 // 3)
        maxColumns = boardDim.width // checkDimWithMargins.width
        topLeftPoints = Lists.zip (leftOffsets number boardDim.width checkDimWithMargins.width maxColumns) (topOffsets number boardDim.height checkDimWithMargins.height maxColumns)
        leftMargin = (checkDimWithMargins.width - checkDim.width) // 2
        topMargin = (checkDimWithMargins.height - checkDim.height) // 2
    in
        List.map (createRectangle checkDim.width checkDim.height leftMargin topMargin) topLeftPoints

drawDiceCheck : (UnresolvedCheck -> Attribute a) -> UnresolvedCheck -> Svg a
drawDiceCheck generator check = icon check "sneak.png" generator

drawResolvedDiceCheck : (ResolvedCheck -> Attribute a) -> ResolvedCheck -> Svg a
drawResolvedDiceCheck generator check = if check.wasSuccess then icon check "ok.jpg" generator else icon check "notOk.png" generator

drawSelectedCheck : (LocationCheck b a -> Attribute c) -> List (b -> List (Svg c)) -> LocationCheck b a -> List (Svg c)
drawSelectedCheck msgGenerator textGenerators check =
    let
        rectangles = calculateCheckerPositions <| List.length check.throws
        throwRectangles = Lists.zip check.throws rectangles
        maxTextRows = 2 + List.length textGenerators
        generateText i gen = List.map (\(t, r) -> info (i + 3) maxTextRows (gen t) r) throwRectangles
        texts = List.concat <| List.indexedMap generateText textGenerators
    in
        List.concat [
            [drawBoardOverlay check msgGenerator]
            , List.map (checkRectangle check "1.0" msgGenerator) rectangles
            , List.map (info 1 maxTextRows [text <| testName check.checkType]) rectangles
            , List.map (info 2 maxTextRows [text <| String.append "Location: " (toString check.location)]) rectangles
            , texts
            ]

drawBoardOverlay check generator =
    checkRectangle check "0.2" generator {x = 0, y = 0 , width = boardDim.width, height = boardDim.height}

checkRectangle check op generator r =
        rect [x <| toString r.x, y <| toString r.y, width <| toString r.width, height <| toString r.height, fill "white", stroke "black", opacity op, generator check][]

info rowNumber maxRows content rectangle =
    let
        middlePoint = rectangle.middle
        posX = middlePoint.x
        posY = middlePoint.y - checkDim.height // 2 + rowNumber * checkDim.height // ( maxRows + 1)
        length = 5 * checkDim.width // 6
    in
        text' [x <| toString posX, y <| toString posY, textLength <| toString length, lengthAdjust "spacingAndGlyphs", fontFamily "Verdana", textAnchor "middle"] content

icon check link generator =
     let
        iconSize = 16
        middlePoint = middle check.location
        posX = middlePoint.x - iconSize // 2
        posY = middlePoint.y - iconSize // 2 - 25
     in
        image [xlinkHref link, x <| toString posX, y <| toString posY, height <| toString iconSize, width <| toString iconSize, generator check] []

testName checkType =
    case checkType of
        Evade -> "Monster evasion"