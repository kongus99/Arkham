module Movement exposing (..)

import List exposing (..)
import List.Extra exposing (break, zip, elemIndex, minimumBy, remove)
import Html exposing (Html, span, button)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App
import Json.Decode as Json exposing ((:=), bool, andThen)
import Maybe exposing (withDefault)
import BoardData exposing (..)
import Paths exposing (..)
import Graphics exposing (..)
import AllDict exposing (AllDict)
import MonsterBowl exposing (Monster)


-- MODEL
type alias Model = { start : (Place Neighborhood Location), path : List (Place Neighborhood Location), investigator : Investigator, isValid : Bool, obstructions : AllDict (Place Neighborhood Location) Monster String}
initialModel = { start = Locale Train_Station, path = [], investigator = firstInvestigator, isValid = True, obstructions = AllDict.empty placeOrder }

path : Place Neighborhood Location -> Place Neighborhood Location -> List Neighborhood -> List (Place Neighborhood Location)
path p1 p2 excluded =
    if p1 == p2 then [] else
        let
           getNeighborhood p = case p of
                                Street n -> n
                                Locale l -> parent l
           start = getNeighborhood p1
           end = getNeighborhood p2
           path = map Street (pathBFS (createPathData end adjacent excluded) start)
        in
          case (p1, p2) of
            (Street n1, Street n2) -> withDefault [] (tail path)
            (Street n, Locale l) -> append (withDefault [] (tail path)) [p2]
            (Locale l, Street n) -> path
            (Locale l1, Locale l2) -> if path /= [] then append path [p2] else []

-- Update

type Msg = Show Point | Move (Place Neighborhood Location) | AddObstruction (Place Neighborhood Location) (Maybe Monster)| Submit

update msg model =
    case msg of
        Submit ->
            if model.isValid then
                {model | path = [], start = withDefault model.start <| head <| reverse model.path}
            else
                model
        Show p ->
            let
                x = Debug.log "clicked" p
            in
                model
        Move place ->
            let
                toNeighborhood p =
                    case p of
                        Street s -> Just s
                        _ -> Nothing
                currentEnd = withDefault model.start (head <| reverse model.path)
                newPath = if model.start == place then []
                          else if member place model.path then append (fst <| break (\x -> x == place) model.path) [place]
                          else if isAdjacent currentEnd place then
                            append model.path [place]
                          else
                            path model.start place <| filterMap toNeighborhood (AllDict.keys model.obstructions)
            in
                {model | path = newPath, isValid = length newPath <= model.investigator.movementPoints}
        AddObstruction place maybeMonster->
                if AllDict.member place model.obstructions then
                    {model | obstructions = AllDict.remove place model.obstructions}
                else
                    case maybeMonster of
                        Nothing -> model
                        Just mm -> {model | obstructions = AllDict.insert place mm model.obstructions}
-- View
view : Model -> Html Msg
view model = svg [ width "1606", height "2384" ] (concat
                                                [ [boardImage]
                                                , (positionCircle model.start model.investigator True)
                                                , (positionCircle (withDefault model.start <| head <| reverse model.path) model.investigator False)
                                                , (concatMap obstructionSquare (AllDict.toList model.obstructions))
                                                , (movementLines model)
                                                , (map (localeCircle localeMsg) allLocation)
                                                , (map (streetRectangle streetMsg) allNeighborhood)
                                                ])

boardImage =
  image [xlinkHref "board.jpg", x "0", y "0", width "1606", height "2384", on "click" (Json.map Show offsetPosition)][]

--Msg generators

onCtrlClick : Place Neighborhood Location -> Html.Attribute Msg
onCtrlClick p =  on "click" (("ctrlKey" := bool) `andThen` msgForCtrlClick p)

msgForCtrlClick place ctrl =
    if ctrl then Json.succeed <| AddObstruction place Nothing else Json.succeed <| Move place

localeMsg : Location -> List(Attribute Msg)
localeMsg l =
    [onDoubleClick Submit, onCtrlClick <| Locale l]

streetMsg : Neighborhood -> List(Attribute Msg)
streetMsg n =
    [onDoubleClick Submit, onCtrlClick <| Street n]

-- Movement lines
movementLines : Model -> List (Svg c)
movementLines model=
    let
        color = if model.isValid then "green" else "red"
        lines = zip (model.start :: model.path) model.path
    in
        map (movement color) lines

-- mouse position
offsetPosition : Json.Decoder Point
offsetPosition =
    Json.object2 Point ("offsetX" := Json.int) ("offsetY" := Json.int)

