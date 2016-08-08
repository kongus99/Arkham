module DiceChecker exposing (prepareCheck, runCheck, resolveCheck, view, Model, initialChecks, addResolvedCheck, Msg, update, clearPendingChecks, generateNewChecks, hasPendingChecks)

import BoardData exposing (..)
import String
import Random
import List.Extra as Lists
import Svg exposing (Svg,svg)
import Svg.Attributes exposing (height, width)
import Svg.Events exposing (onClick)
import Graphics

type alias IsUnresolved = Bool

type alias Model = { currentChecks : List UnresolvedCheck, selected : Maybe (IsUnresolved, Int), previousChecks : List ResolvedCheck}

initialChecks = { currentChecks = [], selected = Nothing, previousChecks = []}

addResolvedCheck resolved model = {model | previousChecks = List.reverse (resolved :: (List.reverse model.previousChecks))}

clearPendingChecks model = {model | currentChecks = []}

generateNewChecks checks model = {model | currentChecks = checks, previousChecks = []}

hasPendingChecks model = List.isEmpty model.currentChecks

prepareCheck location checkType throws successThreshold =
    { location = location
    , checkType = checkType
    , throws = throws
    , successThreshold = successThreshold
    }

runCheck : (UnresolvedCheck -> List Int -> a) -> Model -> (Model, Cmd a)
runCheck wrapper model =
    case model.currentChecks of
        [] -> (model, Cmd.none)
        c :: cs -> ({model | currentChecks = cs}, generateCheck c (wrapper c))

generateCheck : UnresolvedCheck -> (List Int -> a) -> Cmd a
generateCheck check wrapper =
    let
        total = List.sum (List.map (\t -> t.dices) check.throws)
    in
        Random.generate wrapper <| Random.list total (Random.int 1 6)

resolveCheck : UnresolvedCheck -> List Int -> ResolvedCheck
resolveCheck check results =
    let
        splitResults = splitList results (List.map (\t -> t.dices) check.throws)
        throwResults = List.map (resolveSingle check.successThreshold) (Lists.zip check.throws splitResults)
        wasSuccess = Lists.find (\t -> t.wasSuccess) throwResults /= Nothing
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

findCheck : LocationCheck a -> IsUnresolved -> List (LocationCheck a) -> Maybe (IsUnresolved, Int)
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

view : Model -> List (Svg Msg)
view model =
    let
        checksToPerform = List.map (Graphics.drawDiceCheck (\check -> onClick <| UnresolvedDetails check)) model.currentChecks
        checksPerformed = List.map (Graphics.drawResolvedDiceCheck (\check -> onClick <| ResolvedDetails check)) model.previousChecks
        selectedDiceCheck = Maybe.withDefault [] <| Maybe.map (Graphics.drawSelectedDiceCheck (\c -> onClick <| HideDetails)) (getSelectedUnresolved model)
        selectedResolvedDiceCheck = Maybe.withDefault [] <| Maybe.map (Graphics.drawSelectedResolvedDiceCheck (\c -> onClick <| HideDetails)) (getSelectedResolved model)
    in
        List.concat [checksToPerform, checksPerformed, selectedDiceCheck, selectedResolvedDiceCheck]


