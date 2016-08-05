module DiceChecker exposing (prepareCheck, runCheck, resolveCheck, view, Model, initialChecks, addResolvedCheck, Msg, update, clearPendingChecks, generateNewChecks, hasPendingChecks)

import BoardData exposing (..)
import String
import Random
import List.Extra as Lists
import Svg exposing (Svg,svg)
import Svg.Attributes exposing (height, width)
import Svg.Events exposing (onClick)
import Graphics

type alias Model = { currentChecks : List UnresolvedCheck, previousChecks : List ResolvedCheck}

initialChecks = { currentChecks = [], previousChecks = []}

addResolvedCheck resolved model = {model | previousChecks = List.reverse (resolved :: (List.reverse model.previousChecks))}

clearPendingChecks model = {model | currentChecks = []}

generateNewChecks checks model = {model | currentChecks = checks, previousChecks = []}

hasPendingChecks model = List.isEmpty model.currentChecks

prepareCheck location checkType dicesAmount requiredSuccesses successThreshold =
    { location = location
    , checkType = checkType
    , isDetailed = False
    , throws = [Throw dicesAmount requiredSuccesses]
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
         wasSuccess = wasSuccess,
         isDetailed = check.isDetailed}

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

type Msg = UnresolvedDetailsToggle UnresolvedCheck | ResolvedDetailsToggle ResolvedCheck

update : Msg -> Model -> Model
update msg model =
    case msg of
        UnresolvedDetailsToggle c -> {model | currentChecks = List.map (toggleDetails c) model.currentChecks}
        ResolvedDetailsToggle c -> {model | previousChecks = List.map (toggleDetails c) model.previousChecks}

toggleDetails : LocationCheck a -> LocationCheck a -> LocationCheck a
toggleDetails expectedCheck check =
    if expectedCheck == check then
        {check | isDetailed = not check.isDetailed}
    else
        check

view : Model -> List (Svg Msg)
view model =
    let
        checksToPerform = List.indexedMap (Graphics.drawDiceCheck (\check -> onClick <| UnresolvedDetailsToggle check)) model.currentChecks
        checksPerformed = List.indexedMap (Graphics.drawResolvedDiceCheck (\check -> onClick <| ResolvedDetailsToggle check)) model.previousChecks
    in
        List.concat (List.append checksToPerform checksPerformed)


