module Graphics exposing (..)

import BoardData exposing(..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import List
import List.Extra as Lists
import MonsterBowl exposing (Monster)
import String
import PieChart
import GraphicTypes exposing (..)

neighborhoodRectangle: Neighborhood -> Rectangle
neighborhoodRectangle n =
    let
        rectangleMiddle r =
            {x = r.x + (r.width // 2), y = r.y + (r.height // 2)}
        st =
           case n of
                Northside ->            {x = 296, y = 396,  width = 180, height = 80}
                Downtown ->             {x = 591, y = 393,  width = 192, height = 82}
                Easttown ->             {x = 731, y = 621,  width = 208, height = 82}
                Merchant_District ->    {x = 363, y = 808,  width = 264, height = 80}
                Rivertown ->            {x = 720, y = 808,  width = 182, height = 78}
                Miskatonic_University ->{x = 379, y = 1177, width = 236, height = 84}
                French_Hill ->          {x = 721, y = 1219, width = 195, height = 84}
                Uptown ->               {x = 469, y = 1669, width = 152, height = 84}
                Southside ->            {x = 753, y = 1673, width = 180, height = 82}
    in
        {x = st.x, y = st.y, width =  st.width, height = st.height, middle = rectangleMiddle st}


locationCircle: Location -> Circle
locationCircle location =
    let
        circleMiddle c = {x = c.cx, y = c.cy}
        loc =
            case location of
                 Train_Station ->          {cx = 264,  cy = 112,  radius = 64}
                 Independence_Square ->    {cx = 924,  cy = 112,  radius = 64}
                 Bank_of_Arkham ->         {cx = 487,  cy = 112,  radius = 64}
                 Arkham_Asylum ->          {cx = 706,  cy = 112,  radius = 64}
                 Newspaper ->              {cx = 126,  cy = 286,  radius = 64}
                 Hibb's_Roadhouse ->       {cx = 927,  cy = 356,  radius = 64}
                 Velma's_Diner ->          {cx = 1148, cy = 351,  radius = 64}
                 Curiositie_Shoppe ->      {cx = 125 , cy = 473,  radius = 64}
                 Unvisited_Isle ->         {cx = 124 , cy = 759,  radius = 64}
                 Police_Station ->         {cx = 1148, cy = 551,  radius = 64}
                 Graveyard ->              {cx = 1148, cy = 845,  radius = 64}
                 River_Docks ->            {cx = 122,  cy = 938,  radius = 64}
                 The_Unnamable ->          {cx = 342,  cy = 1025, radius = 64}
                 General_Store ->          {cx = 898,  cy = 1017, radius = 64}
                 Black_Cave ->             {cx = 1150, cy = 1030, radius = 64}
                 Science_Building ->       {cx = 122,  cy = 1195, radius = 64}
                 The_Witch_House ->        {cx = 1150, cy = 1312, radius = 64}
                 Silver_Twilight_Lodge ->  {cx = 936,  cy = 1383, radius = 64}
                 Library ->                {cx = 447,  cy = 1382, radius = 64}
                 Administration_Building ->{cx = 243,  cy = 1450, radius = 64}
                 St_Mary's_Hospital ->     {cx = 122,  cy = 1701, radius = 64}
                 Ma's_Boarding_House ->    {cx = 1149, cy = 1640, radius = 64}
                 South_Church ->           {cx = 1058, cy = 1901, radius = 64}
                 Historical_Society ->     {cx = 816,  cy = 1976, radius = 64}
                 Woods ->                  {cx = 562,  cy = 1976, radius = 64}
                 Ye_Olde_Magick_Shoppe ->  {cx = 282,  cy = 1947, radius = 64}
                 _ ->                      {cx = 0,    cy = 0,    radius = 0}
    in
        {cx = loc.cx, cy = loc.cy, radius = loc.radius, middle = circleMiddle loc}

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

positionCircle : Place -> Investigator -> (Investigator -> Attribute a) -> Bool -> List (Svg a)
positionCircle p i msgGenerator isFilled =
    let
        m = middle p
        circleX = toString <| m.x + 30
        circleY = toString m.y
    in
        circle [cx <| circleX, cy <| circleY, r "20", strokeWidth "3", fill "green", stroke "green", fillOpacity (if isFilled then "1.0" else "0.0")][]
      ::text' [textAnchor "middle", x <| circleX, y <| circleY, fill "red", msgGenerator i][text "1"]
      ::[]

investigatorStartPositions : Place -> List Color -> List (Svg a)
investigatorStartPositions p colors  =
    let
        m = middle p
    in
        PieChart.chart (Point (m.x + 30) m.y) 20 colors

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
middle place =
    case place of
        Street s -> (neighborhoodRectangle s).middle
        Locale l -> (locationCircle l).middle

movement: Color -> (Place, Place) -> Svg a
movement color (start, end) =
    let
        p1 = middle start
        p2 = middle end
    in
        line [x1 <| toString p1.x, y1 <| toString p1.y, x2 <| toString p2.x, y2 <| toString p2.y, stroke color, strokeWidth "3", strokeLinecap "round"] []

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