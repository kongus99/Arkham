module Investigators exposing (move, Model, initialModel, showCheckDetails, finalizeMovement, resolveCheck, investigatorBoardView, checkersView)

import BoardData exposing (..)
import Movement
import DiceChecker
import Array exposing (Array)
import Graphics
import Graphics.Common exposing (Color)
import Graphics.Investigators as Positions
import Svg.Attributes exposing (class)
import List.Extra exposing (zip)
import Html.App as App
import List.Extra as Lists
import AllDict

investigatorColors = ["red", "green", "blue", "pink", "violet", "yellow", "black", "orange"]

type alias InvestigatorState = { movement : Movement.Model,  color : Color, investigator : Investigator}

type alias Model = { investigatorList : List InvestigatorState, selected : Maybe InvestigatorState }

initState (c, i) = InvestigatorState Movement.initialModel c i

initialState = List.map initState <| Lists.zip investigatorColors allInvestigators

initialModel = Model (Maybe.withDefault [] <| List.tail initialState) (List.head initialState)

---------------------------------------------
resolveCheck : UnresolvedCheck -> List Int -> Model -> (Model, Cmd (UnresolvedCheck, List Int))
resolveCheck check results model =
    let
        performResolveCheck check results state =
            case check.checkType of
                Evade ->
                        Movement.evadeCheck (DiceChecker.resolveCheck check results) state.movement
    in
        updateMovementWithCmd (performResolveCheck check results) model

finalizeMovement : Place -> Model -> (Model, Cmd (UnresolvedCheck, List Int))
finalizeMovement place model =
    let
        performFinalizeMovement place state=
            if Movement.pathEnd state.movement == place && Movement.isValidPath state.investigator state.movement then
                Movement.finalizeMovement state.movement
            else
                (state.movement, Cmd.none)
    in
        updateMovementWithCmd (performFinalizeMovement place) model

move place monsters model =
    updateMovement (\s -> Movement.moveTo place monsters s.investigator s.movement) model

showCheckDetails msg model =
    updateMovement (\s -> Movement.update msg s.movement) model

updateMovement stateUpdater model =
    {model | selected = Maybe.map (\s -> {s | movement = stateUpdater s}) model.selected}

updateMovementWithCmd stateUpdater model =
    let
        pair = Maybe.map stateUpdater model.selected
    in
        case pair of
            Nothing -> (model, Cmd.none)
            Just (movement, cmd) -> (updateMovement (\s -> movement) model, cmd)

---------------------------------------------
allStates model =
    List.append (Maybe.withDefault [] <| Maybe.map (\i -> [i]) model.selected) model.investigatorList

investigatorSideView model =
    let
        investigators = List.map (\s -> s.investigator) <| allStates model
    in
        List.concat <| (List.indexedMap Positions.minimalData investigators)

investigatorBoardView model =
    let
        states = allStates model
        startPair state =
            (state.movement.start, state.color)
        endPair state =
            (Movement.pathEnd state.movement, state.color)
        linePairs state =
            List.map (\p -> (p, state.color)) <| zip (state.movement.start :: state.movement.path) state.movement.path
        startPositions = groupList <| List.map startPair states
        endPositions = groupList <| List.map endPair states
        linePositions = groupList <| List.concat <| List.map linePairs states
    in
        List.concat <| List.concat [(List.map Positions.start startPositions), (List.map Positions.end endPositions), (List.map Positions.connections linePositions)]

groupList : List ( a, b ) -> List ( a, List b )
groupList pairsToGroup =
    let
        sorted = List.sortBy (\(p, _) -> toString p) <| List.map (\(k, v) -> (k, [v])) pairsToGroup
        grouped = Lists.groupWhile (\f->\s-> fst f == fst s) sorted
        mergePairs pair1 pair2 =
            (fst pair1, List.append (snd pair1) (snd pair2))
    in
        List.filterMap (Lists.foldl1 mergePairs) grouped

checkersView msgGenerator model =
    Maybe.withDefault [] <| Maybe.map (checkersViewDraw msgGenerator) model.selected

checkersViewDraw msgGenerator state =
    List.map (App.map msgGenerator) (DiceChecker.view state.movement.evadeTests)