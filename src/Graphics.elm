module Graphics exposing (..)

import BoardData exposing(..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import List exposing (..)
import MonsterBowl exposing (Monster)
import String

type alias Point = {x : Int, y : Int}
type alias LineConnective shape = { shape | middle : Point }
type alias Circle = LineConnective {cx : Int, cy : Int, radius : Int}
type alias Rectangle = LineConnective {x : Int, y : Int, width : Int, height : Int}
type alias Color = String

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
        circle (append commonAttributes generatedAttributes) []

streetRectangle : (Neighborhood -> List(Attribute a)) -> Neighborhood -> Svg a
streetRectangle generator n =
    let
        r = neighborhoodRectangle n
        commonAttributes = [x <| toString r.x, y <| toString r.y, width <| toString r.width, height <| toString r.height, strokeOpacity "0.0", fillOpacity "0.0"]
        generatedAttributes = generator n
    in
        rect (append commonAttributes generatedAttributes) []

positionCircle : Place Neighborhood Location -> Investigator -> Bool -> List (Svg a)
positionCircle p i isFilled =
    let
        m = middle p
        circleX = toString <| m.x + 30
        circleY = toString m.y
    in
        circle [cx <| circleX, cy <| circleY, r "25", strokeWidth "3", fill "green", stroke "green", fillOpacity (if isFilled then "1.0" else "0.0")][]
      ::text' [textAnchor "middle", x <| circleX, y <| circleY, fill "red"][text (abbreviation i)]
      ::[]
monsterSquare : (Place Neighborhood Location, Monster) -> List (Svg a)
monsterSquare (place, monster) =
    let
        mid = middle place
        rectX = mid.x - 55
        rectY = mid.y - 25
        side = 50
        textX= rectX + 43
        textY = rectY + 48
    in
        rect [x <| toString <| rectX, y <| toString <| rectY, width <| toString side, height <| toString side, fill "red"][]
      ::text' [textAnchor "middle", x <| toString <| textX, y <| toString <| textY][text (toString monster.awareness)]
      ::[]
middle place =
    case place of
        Street s -> (neighborhoodRectangle s).middle
        Locale l -> (locationCircle l).middle

movement: Color -> (Place Neighborhood Location, Place Neighborhood Location) -> Svg a
movement color (start, end) =
    let
        p1 = middle start
        p2 = middle end
    in
        line [x1 <| toString p1.x, y1 <| toString p1.y, x2 <| toString p2.x, y2 <| toString p2.y, stroke color, strokeWidth "3", strokeLinecap "round"] []

diceBoxWidth = 300
diceBoxHeight = 200

drawDiceCheck : (DiceCheck -> Attribute a) -> Int -> DiceCheck -> List (Svg a)
drawDiceCheck generator index check =
    let
        rectangleX = diceBoxWidth * index
        rectangleY = 0
    in
        if check.isDetailed then
            [ rect [x <| toString rectangleX, y <| toString rectangleY, width <| toString diceBoxWidth, height <| toString diceBoxHeight, fill "white", stroke "black"][]
            , info index 1 <| [text <| testName check.checkType]
            , info index 2 <| [text <| String.append "Location: " (toString check.location)]
            , info index 3 <| [text <| String.append "Dices available: " (toString check.dicesAmount)]
            , info index 4 <| [text <| String.append "Successes required: " (toString check.requiredSuccesses)]
            , rect [x <| toString rectangleX, y <| toString rectangleY, width <| toString diceBoxWidth, height <| toString diceBoxHeight, opacity "0.0", generator check][]]
        else
            [icon index "sneak.png" <| generator check]

drawResolvedDiceCheck : (ResolvedDiceCheck -> Attribute a) -> Int -> ResolvedDiceCheck -> List (Svg a)
drawResolvedDiceCheck generator index check =
    let
        rectangleX = diceBoxWidth * index
        rectangleY = 0
    in
        case (check.wasSuccess, check.isDetailed) of
            (_, True) -> [ rect [x <| toString rectangleX, y <| toString rectangleY, width <| toString diceBoxWidth, height <| toString diceBoxHeight, fill "white", stroke "black"][]
                         , info index 1 <| [text <| testName check.checkType]
                         , info index 3 <| (List.map singleDice check.dices)
                         , rect [x <| toString rectangleX, y <| toString rectangleY, width <| toString diceBoxWidth, height <| toString diceBoxHeight, opacity "0.0", generator check][]]
            (True, _) -> [icon index "ok.jpg" (generator check)]
            (False, _) -> [icon index "notOk.png" (generator check)]


info indexX indexY content =
    let
        posX = diceBoxWidth // 2  + diceBoxWidth * indexX
        posY = indexY * diceBoxHeight // 5
        length = 5 * diceBoxWidth / 6
    in
        text' [x <| toString posX, y <| toString posY, textLength <| toString length, lengthAdjust "spacingAndGlyphs", fontFamily "Verdana", textAnchor "middle"] content

icon index link whenClicked =
     let
        iconSize = 16
        posX = (diceBoxWidth - iconSize) // 2 + diceBoxWidth * index
        posY = (diceBoxHeight - iconSize) // 2
     in
        image [xlinkHref link, x <| toString posX, y <| toString posY, height <| toString iconSize, width <| toString iconSize, whenClicked] []

singleDice : (Int, WasSuccess) -> Svg a
singleDice (faceValue, wasSuccess) = tspan  [fill (diceStyle wasSuccess), fontWeight "bold"] [ text (toString faceValue) ]

diceStyle: WasSuccess -> String
diceStyle wasSuccess =
    if wasSuccess then
        "green"
    else
        "red"

testName checkType =
    case checkType of
        Evade -> "Monster evasion"