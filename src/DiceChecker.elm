module DiceChecker exposing (prepareCheck, runCheck, view, Model, initialChecks, resolveOldChecks, Msg, update, generateNewChecks, hasNoPendingChecks)

import BoardData exposing (..)
import Graphics.Common exposing (Color)
import String
import Random
import List.Extra as Lists
import Svg exposing (Svg,svg, text', text, tspan)
import Svg.Attributes exposing (height, width, fill, fontWeight)
import Svg.Events exposing (onClick)
import Graphics

type alias IsUnresolved = Bool

type alias Model = { currentChecks : List UnresolvedCheck, selected : Maybe (IsUnresolved, Int), previousChecks : List ResolvedCheck}

initialChecks = { currentChecks = [], selected = Nothing, previousChecks = []}

resolveOldChecks resolved model = {model | currentChecks = [], previousChecks = List.reverse resolved}

generateNewChecks checks model = {model | currentChecks = checks, previousChecks = []}

hasNoPendingChecks model = List.isEmpty model.currentChecks

prepareCheck location checkType throws successThreshold =
    { location = location
    , checkType = checkType
    , throws = throws
    , successThreshold = successThreshold
    }

runCheck : Model -> Cmd (List ResolvedCheck)
runCheck model = generateAllChecks model.currentChecks

generateAllChecks : List UnresolvedCheck -> Cmd (List ResolvedCheck)
generateAllChecks checks =
    let
        resolvedGenerator = mergeChecks <| List.map generateCheck checks
    in
        case resolvedGenerator of
        Nothing -> Cmd.none
        Just g -> Random.generate identity g

mergeChecks : List (Random.Generator ResolvedCheck) -> Maybe (Random.Generator (List ResolvedCheck))
mergeChecks generators =
    let
        randomLists = List.map (Random.map (\g -> [g])) generators
        merger genList1 genList2 = Random.map2 List.append genList1 genList2
    in
        Lists.foldl1 merger randomLists

generateCheck : UnresolvedCheck -> Random.Generator ResolvedCheck
generateCheck check =
    let
        total = List.sum (List.map (\t -> t.dices) check.throws)
    in
        Random.map (resolveCheck check) <| Random.list total (Random.int 1 6)

resolveCheck : UnresolvedCheck -> List Int -> ResolvedCheck
resolveCheck check results =
    let
        splitResults = splitList results (List.map (\t -> t.dices) check.throws)
        throwResults = List.map (resolveSingle check.successThreshold) (Lists.zip check.throws splitResults)
        wasSuccess = List.isEmpty <| List.filter (\t -> not t.wasSuccess) throwResults
    in
        {location = check.location,
         checkType = check.checkType,
         throws = throwResults,
         wasSuccess = wasSuccess}

splitList list amounts =
    case amounts of
        [] -> []
        x :: xs -> (List.take x list) :: splitList (List.drop x list) xs

resolveSingle successThreshold (throw, results) =
    let
        rollSuccessful res = res >= successThreshold
        dices = List.map (\r -> (r, rollSuccessful r)) results
        wasSuccess = (List.length (List.filter rollSuccessful results)) >= throw.numOfSuccesses
    in
        ThrowResult dices wasSuccess

type Msg = UnresolvedDetails UnresolvedCheck | ResolvedDetails ResolvedCheck | HideDetails

update : Msg -> Model -> Model
update msg model =
    case msg of
        UnresolvedDetails c -> {model | selected = findCheck c True model.currentChecks}
        ResolvedDetails c -> {model | selected = findCheck c False model.previousChecks}
        HideDetails -> {model | selected = Nothing }

findCheck : LocationCheck b a -> IsUnresolved -> List (LocationCheck b a) -> Maybe (IsUnresolved, Int)
findCheck check isUnresolved checks =
     Maybe.map (\i -> (isUnresolved, i))  <| Lists.elemIndex check checks

getSelectedUnresolved model =
    case model.selected of
        Just (True, i) -> Lists.getAt i model.currentChecks
        _ -> Nothing

getSelectedResolved model =
    case model.selected of
        Just (False, i) -> Lists.getAt i model.previousChecks
        _ -> Nothing

dicesAvailable throw = [text <| String.append "Dices available: " (toString throw.dices)]

successesRequired throw = [text <| String.append "Successes required: " (toString throw.numOfSuccesses)]

throwResults throw = (List.map singleDice throw.dices)

singleDice (faceValue, wasSuccess) =
    let
        diceStyle wasSuccess = if wasSuccess then "green" else "red"
    in
        tspan  [fill (diceStyle wasSuccess), fontWeight "bold"] [ text (toString faceValue) ]

view : Color -> Model -> List (Svg Msg)
view color model =
    let
        checksToPerform = List.map (Graphics.drawDiceCheck (\check -> onClick <| UnresolvedDetails check)) model.currentChecks
        checksPerformed = List.map (Graphics.drawResolvedDiceCheck (\check -> onClick <| ResolvedDetails check)) model.previousChecks
        selectedDiceCheck = Maybe.withDefault [] <| Maybe.map (Graphics.drawSelectedCheck color (\c -> onClick <| HideDetails) [dicesAvailable, successesRequired]) (getSelectedUnresolved model)
        selectedResolvedDiceCheck = Maybe.withDefault [] <| Maybe.map (Graphics.drawSelectedCheck color (\c -> onClick <| HideDetails) [throwResults]) (getSelectedResolved model)
    in
        List.concat [checksToPerform, checksPerformed, selectedDiceCheck, selectedResolvedDiceCheck]


