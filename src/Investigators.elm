module Investigators exposing (move, select, Model, initialModel, showCheckDetails, prepareChecks, resolveChecks, investigatorBoardView, checkersView, investigatorSideView, adjustSkills, approveSkillAdjustments)

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
import Skills

investigatorColors = ["red", "green", "blue", "pink", "violet", "yellow", "black", "orange"]

type alias InvestigatorData = { movement : Movement.Model,  color : Color, adjustments : Skills.SkillAdjustments, investigator : Investigator}

type alias Model = { investigatorList : List (Selection InvestigatorData) }

initData (c, i) = InvestigatorData (Movement.initialModel i.start) c Skills.initialAdjustments i

initialData = List.map initData <| Lists.zip investigatorColors allInvestigators

initialModel = Model (List.map Selection.NotSelected initialData)

---------------------------------------------
resolveChecks : (Investigator, List ResolvedCheck) -> Model -> Model
resolveChecks (investigator, checks) model =
   updateInvestigatorMovement investigator (\s -> Movement.resolveEvades checks s.movement) model

prepareChecks : Model -> Cmd (Investigator, List ResolvedCheck)
prepareChecks model =
    let
        cmdGenerator selectedData =
            let
                data = Selection.unpack selectedData
            in
                Cmd.map (\c -> (data.investigator, c)) <| Movement.prepareEvades data.movement

    in
        Cmd.batch <| List.map cmdGenerator model.investigatorList

move place monsters model =
    updateSelectedMovement (\s -> Movement.moveTo place monsters (s.investigator.skills, s.adjustments) s.movement) model

select investigator model =
    {model | investigatorList = Selection.selectNew (\s -> s.investigator == investigator) model.investigatorList }

adjustSkills skillData model =
     {model | investigatorList = Selection.map (\s -> {s | adjustments = Skills.adjustSkill skillData (s.investigator.skills.focus, s.adjustments)}) (Just identity) model.investigatorList }

approveSkillAdjustments model =
    {model | investigatorList = Selection.update (\s -> True) (\s -> {s | adjustments = Skills.approveSkills s.adjustments}) model.investigatorList }

showCheckDetails msg model =
    updateSelectedMovement (\s -> Movement.update msg s.movement) model

updateSelectedMovement : (InvestigatorData -> Movement.Model) -> Model -> Model
updateSelectedMovement dataUpdater model =
    {model | investigatorList = Selection.map (\s -> {s | movement = dataUpdater s}) (Just identity) model.investigatorList}

updateInvestigatorMovement : Investigator -> (InvestigatorData -> Movement.Model) -> Model -> Model
updateInvestigatorMovement investigator dataUpdater model =
    {model | investigatorList = Selection.update (\s -> s.investigator == investigator) (\s -> {s | movement = dataUpdater s}) model.investigatorList}

---------------------------------------------
investigatorSideView : ((Skills.SkillSet, Int) -> List (Attribute a)) -> (Investigator -> Attribute a) -> Model -> List (Svg a)
investigatorSideView skillSetSelection invSelection model =
    let
        selectedInvestigator = Selection.findSelected model.investigatorList |> Maybe.map (\i -> (i.investigator, i.adjustments))
        investigators = Selection.map (\s -> (s.investigator, s.color, Movement.movesLeft (s.investigator.skills, s.adjustments) s.movement.path)) Nothing model.investigatorList
    in
        List.append (List.concat <| (List.indexedMap (Positions.minimalData invSelection) investigators)) <| Positions.characterCard skillSetSelection selectedInvestigator

investigatorBoardView :Model -> List (Svg a)
investigatorBoardView model =
    let
        data = List.map Selection.unpack model.investigatorList
        startPair data =
            (data.movement.start, data.color)
        endPair data =
            (Movement.pathEnd data.movement, data.color)
        linePairs data =
            List.map (\p -> (p, data.color)) <| zip (data.movement.start :: data.movement.path) data.movement.path
        startPositions = groupList <| List.map startPair data
        endPositions = groupList <| List.map endPair data
        linePositions = groupList <| List.concat <| List.map linePairs data
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

checkersViewDraw msgGenerator data =
    List.map (App.map msgGenerator) (DiceChecker.view data.color data.movement.evadeTests)