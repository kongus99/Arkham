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
import Graphics exposing (..)


-- MODEL
type alias Color = String
type alias Model = { start : (Place Neighborhood Location), path : List (Place Neighborhood Location), movementPoints : Int, isValid : Bool, obstructions : List (Place Neighborhood Location) }
initialModel = { start = Locale Train_Station, path = [], movementPoints = 3, isValid = True, obstructions = [] }

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

type Msg = Show Point | Move (Place Neighborhood Location) | AddObstruction (Place Neighborhood Location)| Submit

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
        AddObstruction place -> (model, Cmd.none)

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
view : Model -> Html Msg
view model = svg [ width "1606", height "2384" ] (concat
                                                [ [boardImage]
                                                , (map (localeCircle localeMsg) allLocation)
                                                , (map streetRectangle allNeighborhood)
                                                , (movementLines model)
                                                , [positionCircle model.start True]
                                                , [positionCircle (withDefault model.start <| head <| reverse model.path) False]])

boardImage =
  image [xlinkHref "board.jpg", x "0", y "0", width "1606", height "2384", on "click" (Json.map Show offsetPosition)][]

--Location circles
localeMsg : Location -> List(Attribute Msg)
localeMsg l =
    [onDoubleClick Submit, onClick <| Move (Locale l)]

-- Position circles
positionCircle : Place Neighborhood Location -> Bool -> Svg a
positionCircle p isFilled =
    let
        connector = connectorPoint p
    in
        circle [cx <| toString connector.x, cy <| toString connector.y, r "12", strokeWidth "3", stroke "black", strokeOpacity "1.0", fillOpacity (if isFilled then "1.0" else "0.0")][]

-- Street rectangles

streetRectangle s =
    rect (rectangleParams (neighborhoodRectangle s) s) []

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
        Street s -> (neighborhoodRectangle s).connector
        Locale l -> (locationCircle l).connector

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
