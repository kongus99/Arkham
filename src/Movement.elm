module Movement exposing (moveTo, pathEnd, isValidPath, Model, initialModel, firstEvadeCheck, evadeCheck)

import BoardData exposing (..)
import Paths
import AllDict exposing (AllDict)
import List.Extra exposing (break)
import DiceChecker exposing (..)
import MonsterBowl exposing (Monster)

type alias Model = { start : Place Neighborhood Location, path : List (Place Neighborhood Location), evadeTests : DiceChecker.Model}
initialModel = { start = Locale Train_Station, path = [], evadeTests = DiceChecker.initialChecks }

path : Place Neighborhood Location -> Place Neighborhood Location -> List Neighborhood -> List (Place Neighborhood Location)
path p1 p2 excluded =
    if p1 == p2 then [] else
        let
           getNeighborhood p = case p of
                                Street n -> n
                                Locale l -> parent l
           start = getNeighborhood p1
           end = getNeighborhood p2
           path = List.map Street (Paths.pathBFS (Paths.createPathData end adjacent excluded) start)
        in
          case (p1, p2) of
            (Street n1, Street n2) -> Maybe.withDefault [] (List.tail path)
            (Street n, Locale l) ->   if path /= [] then List.append (Maybe.withDefault [] (List.tail path)) [p2] else []
            (Locale l, Street n) -> if List.member start excluded then [] else path
            (Locale l1, Locale l2) -> if path /= [] then List.append path [p2] else []

moveTo: Place Neighborhood Location -> AllDict (Place Neighborhood Location) Monster String -> Investigator -> Model -> Model
moveTo place monsters investigator model =
    let
        currentEnd = pathEnd model
        newPath = if model.start == place then []
                  else if List.member place model.path then List.append (fst <| break (\x -> x == place) model.path) [place]
                  else if isAdjacent currentEnd place then
                    List.append model.path [place]
                  else
                    path model.start place <| List.filterMap toNeighborhood (AllDict.keys monsters)
        newEvadeTests = prepareEvadeTests model.start newPath monsters investigator model.evadeTests
    in
        {model | path = newPath, evadeTests = newEvadeTests}

prepareEvadeTests : Place Neighborhood Location -> List (Place Neighborhood Location) -> AllDict (Place Neighborhood Location) Monster String -> Investigator -> DiceChecker.Model ->  DiceChecker.Model
prepareEvadeTests start path monsters investigator model =
    let
        monsterList = AllDict.toList monsters
        monstersOnPath = if List.isEmpty path then [] else List.filter (\(p, m) -> List.member p (start :: path)) monsterList
    in
        {model |  currentChecks = List.reverse (List.map (\(p, m) -> DiceChecker.prepareCheck p Evade (investigator.sneak - m.awareness) 1 5) monstersOnPath)}

endMove : Place Neighborhood Location -> Model -> Model
endMove place model =
    {model | path = [], start = place, evadeTests = DiceChecker.clearPendingChecks model.evadeTests}

evadeCheck : ResolvedDiceCheck -> (DiceCheck -> List Int -> a) -> Model -> ( Model, Cmd a )
evadeCheck resolved wrapper model =
    let
        newModel = {model | evadeTests = addResolvedCheck resolved model.evadeTests }
    in
        if resolved.wasSuccess then
            finalizeMovement wrapper newModel
        else
            (endMove resolved.location newModel, Cmd.none)

firstEvadeCheck : (DiceCheck -> List Int -> a) -> Model -> (Model, Cmd a)
firstEvadeCheck wrapper model =
    let
        newChecks = DiceChecker.clearPreviousChecks model.evadeTests
    in
        finalizeMovement wrapper {model | evadeTests = newChecks}


finalizeMovement : (DiceCheck -> List Int -> a) -> Model -> (Model, Cmd a)
finalizeMovement wrapper model=
    let
        allChecks = model.evadeTests
    in
        case allChecks.currentChecks of
            [] -> (endMove (pathEnd model) model, Cmd.none)
            t :: ts ->
                let
                    newModel = {model | evadeTests = {allChecks | currentChecks = ts}}
                    check = DiceChecker.runCheck t (wrapper t)
                in
                    (newModel, check)

toNeighborhood p =
    case p of
        Street s -> Just s
        _ -> Nothing

pathEnd model =
     Maybe.withDefault model.start <| List.head <| List.reverse model.path

isValidPath investigator model =
    List.length model.path <= investigator.movementPoints

