module Investigators exposing (move, Model, initialModel, showCheckDetails, finalizeMovement, resolveCheck, investigatorView, checkersView)

import BoardData exposing (..)
import Movement
import DiceChecker
import Array exposing (Array)
import Graphics
import Svg.Attributes exposing (class)
import List.Extra exposing (zip)
import Html.App as App

type alias InvestigatorState = { investigator : Investigator, movement : Movement.Model }

type alias Model = { investigatorList : List InvestigatorState, selected : Maybe InvestigatorState }

initialState = InvestigatorState defaultInvestigator Movement.initialModel

initialModel = Model [] <| Just initialState

---------------------------------------------
resolveCheck : UnresolvedCheck -> List Int -> (UnresolvedCheck -> List Int -> c) -> Model -> (Model, Cmd c)
resolveCheck check results msgGenerator model =
    let
        performResolveCheck check results msgGenerator state =
            case check.checkType of
                Evade ->
                    let
                        resolved = DiceChecker.resolveCheck check results
                    in
                        Movement.evadeCheck resolved msgGenerator state.movement
    in
        updateMovementWithCmd (performResolveCheck check results msgGenerator) model

finalizeMovement place msgGenerator model =
    let
        performFinalizeMovement place msgGenerator state=
            if Movement.pathEnd state.movement == place && Movement.isValidPath state.investigator state.movement then
                Movement.finalizeMovement msgGenerator state.movement
            else
                (state.movement, Cmd.none)
    in
        updateMovementWithCmd (performFinalizeMovement place msgGenerator) model

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

investigatorView model =
    Maybe.withDefault [] <| Maybe.map positionDraw model.selected

positionDraw state =
    List.concat
        [ Graphics.positionCircle state.movement.start state.investigator (\i -> class "ccc") True
        , Graphics.positionCircle (Movement.pathEnd state.movement) state.investigator (\i -> class "ccc") False
        , movementLinesDraw state]

movementLinesDraw state =
    let
        color = if Movement.isValidPath state.investigator state.movement then "green" else "red"
        lines = zip (state.movement.start :: state.movement.path) state.movement.path
    in
        List.map (Graphics.movement color) lines

checkersView msgGenerator model =
    Maybe.withDefault [] <| Maybe.map (checkersViewDraw msgGenerator) model.selected

checkersViewDraw msgGenerator state =
    List.map (App.map msgGenerator) (DiceChecker.view state.movement.evadeTests)