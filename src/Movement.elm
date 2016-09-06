module Movement exposing (moveTo, pathEnd, Model, initialModel, finalizeMovement, evadeCheck, update, movesLeft)

import BoardData exposing (..)
import Paths
import AllDict exposing (AllDict)
import List.Extra exposing (break)
import DiceChecker exposing (..)
import MonsterBowl exposing (Monster)

type alias Model = { start : Place, path : List Place, evadeTests : DiceChecker.Model}
initialModel = Model (Locale Train_Station) [] DiceChecker.initialChecks

update : DiceChecker.Msg -> Model -> Model
update msg model = {model | evadeTests = DiceChecker.update msg model.evadeTests}

path : Place -> Place -> List Neighborhood -> List Place
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

moveTo: Place -> AllDict Place (List Monster) String -> Investigator -> Model -> Model
moveTo place monsters investigator model =
    let
        currentEnd = pathEnd model
        newPath = if model.start == place then []
                  else if List.member place model.path then List.append (fst <| break (\x -> x == place) model.path) [place]
                  else if isAdjacent currentEnd place then
                    List.append model.path [place]
                  else
                    path model.start place <| List.filterMap toNeighborhood (AllDict.keys monsters)
        newEvadeTests = prepareEvadeTests (model.start :: newPath) monsters investigator model.evadeTests
    in
        if movesLeft investigator newPath >= 0 then
            {model | path = newPath, evadeTests = newEvadeTests}
        else model

prepareEvadeTests : List Place -> AllDict Place (List Monster) String -> Investigator -> DiceChecker.Model ->  DiceChecker.Model
prepareEvadeTests path monsters investigator model =
    let
        generateCheck place ms = DiceChecker.prepareCheck place Evade (List.map (\m -> Throw (investigator.sneak - m.awareness) 1) ms) 5
    in
        DiceChecker.generateNewChecks (List.filterMap (\p -> Maybe.map (generateCheck p) (AllDict.get p monsters)) path) model

endMove : Place -> Model -> Model
endMove place model =
    {model | path = [], start = place, evadeTests = DiceChecker.clearPendingChecks model.evadeTests}

evadeCheck : ResolvedCheck -> Model -> ( Model, Cmd ResolvedCheck )
evadeCheck resolved model =
    let
        newModel = {model | evadeTests = addResolvedCheck resolved model.evadeTests }
    in
        if resolved.wasSuccess then
            finalizeMovement newModel
        else
            (endMove resolved.location newModel, Cmd.none)

finalizeMovement : Model -> (Model, Cmd ResolvedCheck)
finalizeMovement model =
    let
        (tests, cmd) = runCheck model.evadeTests
        newModel = {model | evadeTests = tests}
    in
        if DiceChecker.hasPendingChecks model.evadeTests then
            (endMove (pathEnd model) newModel, Cmd.none)
        else
            (newModel, cmd)

toNeighborhood p =
    case p of
        Street s -> Just s
        _ -> Nothing

pathEnd model =
     Maybe.withDefault model.start <| List.head <| List.reverse model.path

movesLeft investigator path =
    investigator.speed - List.length path
