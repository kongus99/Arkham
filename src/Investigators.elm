module Investigators exposing (move, select, Model, initialModel, showCheckDetails, finalizeMovement, resolveCheck, investigatorBoardView, checkersView, investigatorSideView)

import BoardData exposing (..)
import Selection exposing (Selection)
import Movement
import Svg exposing (Svg, Attribute)
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

type alias Model = { investigatorList : List (Selection InvestigatorState) }

initState (c, i) = InvestigatorState Movement.initialModel c i

initialState = List.map initState <| Lists.zip investigatorColors allInvestigators

initialModel = Model (List.map Selection.NotSelected initialState)

---------------------------------------------
resolveCheck : ResolvedCheck -> Model -> (Model, Cmd ResolvedCheck)
resolveCheck check model =
    let
        performResolveCheck check state =
            case check.checkType of
                Evade ->
                        Movement.evadeCheck check state.movement
    in
        updateMovementWithCmd (performResolveCheck check) model

finalizeMovement : Place -> Model -> (Model, Cmd ResolvedCheck)
finalizeMovement place model =
    let
        performFinalizeMovement place state=
            if Movement.pathEnd state.movement == place then
                Movement.finalizeMovement state.movement
            else
                (state.movement, Cmd.none)
    in
        updateMovementWithCmd (performFinalizeMovement place) model

move place monsters model =
    updateMovement (\s -> Movement.moveTo place monsters s.investigator s.movement) model

select investigator model =
    {model | investigatorList = Selection.selectNew (\s -> s.investigator == investigator) model.investigatorList }

showCheckDetails msg model =
    updateMovement (\s -> Movement.update msg s.movement) model

updateMovement stateUpdater model =
    {model | investigatorList = Selection.map (\s -> {s | movement = stateUpdater s}) (Just identity) model.investigatorList}

updateMovementWithCmd stateUpdater model =
    let
        selected = Maybe.map Selection.unpack <| Lists.find Selection.isSelected model.investigatorList
        pair = Maybe.map stateUpdater selected
    in
        case pair of
            Nothing -> (model, Cmd.none)
            Just (movement, cmd) -> (updateMovement (\s -> movement) model, cmd)

---------------------------------------------
investigatorSideView : (Investigator -> Attribute a) -> Model -> List (Svg a)
investigatorSideView msgGenerator model =
    let
        selectedInvestigator = Selection.findSelected model.investigatorList |> Maybe.map (\i -> i.investigator)
        investigators = Selection.map (\s -> (s.investigator, Movement.movesLeft s.investigator s.movement.path)) Nothing model.investigatorList
    in
        List.append (List.concat <| (List.indexedMap (Positions.minimalData msgGenerator) investigators)) <| Positions.characterCard selectedInvestigator

investigatorBoardView :Model -> List (Svg a)
investigatorBoardView model =
    let
        states = List.map Selection.unpack model.investigatorList
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

checkersView : (DiceChecker.Msg -> a) -> Model -> List (Svg a)
checkersView msgGenerator model =
    Maybe.withDefault [] <| Maybe.map (\s -> checkersViewDraw msgGenerator <| Selection.unpack s) <| Lists.find Selection.isSelected model.investigatorList

checkersViewDraw msgGenerator state =
    List.map (App.map msgGenerator) (DiceChecker.view state.movement.evadeTests)