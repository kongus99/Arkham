module Movement exposing (..)

import List exposing (..)
import List.Extra exposing (break, zip, elemIndex, minimumBy)
import Html exposing (Html, span, button)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App
import Json.Decode as Json exposing ((:=))
import Maybe exposing (withDefault)
import BoardData exposing (..)
import Paths exposing (..)
-- MODEL
type alias Color = String
type alias Model = { start : (Place Neighborhood Location), path : List (Place Neighborhood Location), movementPoints : Int, isValid : Bool }
initialModel = { start = Locale Train_Station, path = [], movementPoints = 3, isValid = True }

path : Place Neighborhood Location -> Place Neighborhood Location -> List Neighborhood -> List (Place Neighborhood Location)
path p1 p2 excluded =
    if p1 == p2 then [] else
        let
           getNeighborhood p = case p of
                                Street n -> n
                                Locale l -> parent l
           start = getNeighborhood p1
           end = getNeighborhood p2
           path =  if not (p1 == p2) then map Street (pathBFS (createPathData end adjacent excluded) start) else []
        in
          case (p1, p2) of
            (Street n1, Street n2) -> withDefault [] (tail path)
            (Street n, Locale l) -> append (withDefault [] (tail path)) [p2]
            (Locale l, Street n) -> path
            (Locale l1, Locale l2) -> append path [p2]

-- Update

type Msg = Show Point | Move (Place Neighborhood Location) | Submit

update msg model =
    case msg of
        Submit ->
            if model.isValid then
                ({model | path = [], start = withDefault model.start <| head <| reverse model.path}, Cmd.none)
            else
                (model, Cmd.none)
        Show p ->
            let
                x = Debug.log "clicked" p
            in
                (model, Cmd.none)
        Move place ->
            let
                toNeighborhood p =
                    case p of
                        Street s -> Just s
                        _ -> Nothing
                (firstPartOfPath, _) = break (\x -> x == place) model.path
                startOfPath = withDefault model.start (head <| reverse firstPartOfPath)
                secondPartOfPath = path startOfPath place (filterMap toNeighborhood firstPartOfPath)
                mergedPath = merge firstPartOfPath secondPartOfPath
                newPath = path model.start place []
                minPath = if length newPath < length mergedPath then newPath else mergedPath
            in
                ({model | path = minPath, isValid = length newPath <= model.movementPoints}, Cmd.none)

merge path1 path2 =
    let
        maxLength = length path1 + length path2
        indexInPath1 elem = withDefault maxLength (elemIndex elem path1)
        indexesInBothPaths index elem = (indexInPath1 elem, index)
        indexPairs = indexedMap indexesInBothPaths path2
        sum (a, b) = a + (length path2 - b)
        (minPath1Index, minPath2Index) = withDefault (maxLength, maxLength) (minimumBy sum indexPairs)
    in
        if (sum (minPath1Index, minPath2Index)) < maxLength then append (take minPath1Index path1) (drop minPath2Index path2) else append path1 path2

-- View

view model = svg [ width "1606", height "2384" ] (concat
                                                [ [boardImage]
                                                , (map locationCircle allLocation)
                                                , (map streetRectangle allNeighborhood)
                                                , (movementLines model)
                                                , [positionCircle model.start True]
                                                , [positionCircle (withDefault model.start <| head <| reverse model.path) False]])

boardImage =
  image [xlinkHref "board.jpg", x "0", y "0", width "1606", height "2384", on "click" (Json.map Show offsetPosition)][]

type alias Point = {x : Int, y : Int}
type alias LineConnective shape = { shape | connector : Point }
--Location circles
type alias Circle = LineConnective {cx : Int, cy : Int, radius : Int}

locationCircle : Location -> Svg Msg
locationCircle l =
    circle (circleParams (locationDimensions l) l) []

locationDimensions location =
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
        {cx = loc.cx, cy = loc.cy, radius = loc.radius, connector = circleMiddle loc}

circleParams c l =
    [cx <| toString c.cx, cy <| toString c.cy, r <| toString c.radius, strokeWidth "1", strokeOpacity "0.0", fillOpacity "0.0", onDoubleClick Submit, onClick <| Move (Locale l)]
-- Position circle
positionCircle : Place Neighborhood Location -> Bool -> Svg a
positionCircle p isFilled =
    let
        connector = connectorPoint p
    in
        circle [cx <| toString connector.x, cy <| toString connector.y, r "12", strokeWidth "3", stroke "black", strokeOpacity "1.0", fillOpacity (if isFilled then "1.0" else "0.0")][]

-- Street rectangles
type alias Rectangle = LineConnective {x : Int, y : Int, width : Int, height : Int}

streetRectangle s =
    rect (rectangleParams (streetDimensions s) s) []

streetDimensions street =
    let
        rectangleMiddle r =
            {x = floor <| r.x + (r.width / 2), y = floor <| r.y + (r.height / 2)}
        st =
           case street of
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
        {x = st.x, y = st.y,  width = st.width, height = st.height, connector = rectangleMiddle st}

rectangleParams r s =
    [ x <| toString r.x, y <| toString r.y, width <| toString r.width, height <| toString r.height, strokeOpacity "0.0", fillOpacity "0.0", onDoubleClick Submit, onClick <| Move (Street s)]

-- Movement lines
movementLines : Model -> List (Svg c)
movementLines model=
    let
        color = if model.isValid then "green" else "red"
        lines = zip (model.start :: model.path) model.path
    in
        map (movement color) lines

connectorPoint place =
    case place of
        Street s -> (streetDimensions s).connector
        Locale l -> (locationDimensions l).connector

movement: Color -> (Place Neighborhood Location, Place Neighborhood Location) -> Svg a
movement color (start, end) = drawLine (connectorPoint start) (connectorPoint end) color

drawLine p1 p2 color=
    line [x1 <| toString p1.x, y1 <| toString p1.y, x2 <| toString p2.x, y2 <| toString p2.y, stroke color, strokeWidth "5", strokeLinecap "round"] []

-- mouse position
offsetPosition : Json.Decoder Point
offsetPosition =
    Json.object2 Point ("offsetX" := Json.int) ("offsetY" := Json.int)

main =
   App.program { init = ( initialModel, Cmd.none ), view = view, update = update, subscriptions = \_ -> Sub.none }
