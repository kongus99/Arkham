module DiceChecker exposing (prepareCheck, runCheck, resolveCheck, view, Model, initialChecks, addResolvedCheck, Msg, update, clearPendingChecks, generateNewChecks, hasPendingChecks)

import BoardData exposing (..)
import String
import Random
import Svg exposing (Svg,svg)
import Svg.Attributes exposing (height, width)
import Svg.Events exposing (onClick)
import Graphics

type alias Model = { currentChecks : List DiceCheck, previousChecks : List ResolvedDiceCheck}

initialChecks = { currentChecks = [], previousChecks = []}

addResolvedCheck resolved model = {model | previousChecks = List.reverse (resolved :: (List.reverse model.previousChecks))}

clearPendingChecks model = {model | currentChecks = []}

generateNewChecks checks model = {model | currentChecks = checks, previousChecks = []}

hasPendingChecks model = List.isEmpty model.currentChecks

prepareCheck location checkType dicesAmount requiredSuccesses successThreshold =
    {location = location,
     checkType = checkType,
     dicesAmount = dicesAmount,
     requiredSuccesses = requiredSuccesses,
     successThreshold = successThreshold,
     isDetailed = False}

runCheck : (DiceCheck -> List Int -> a) -> Model -> (Model, Cmd a)
runCheck wrapper model =
    case model.currentChecks of
        [] -> ({model | currentChecks = []}, Cmd.none)
        t :: ts -> ({model | currentChecks = ts}, generateCheck t (wrapper t))

generateCheck : DiceCheck -> (List Int -> a) -> Cmd a
generateCheck check wrapper =
    Random.generate wrapper <| Random.list check.dicesAmount (Random.int 1 6)

resolveCheck : DiceCheck -> List Int -> ResolvedDiceCheck
resolveCheck check results =
    let
        rollSuccessful res = res >= check.successThreshold
        dices = List.map (\r -> (r, rollSuccessful r)) results
        wasSuccess = (List.length (List.filter rollSuccessful results)) >= check.requiredSuccesses
    in
        {location = check.location,
         checkType = check.checkType,
         dicesAmount = check.dicesAmount,
         dices = dices,
         wasSuccess = wasSuccess,
         isDetailed = check.isDetailed}

type Msg = UnresolvedDetailsToggle DiceCheck | ResolvedDetailsToggle ResolvedDiceCheck

update : Msg -> Model -> Model
update msg model =
    case msg of
        UnresolvedDetailsToggle c -> {model | currentChecks = List.map (toggleDetails c) model.currentChecks}
        ResolvedDetailsToggle c -> {model | previousChecks = List.map (toggleDetails c) model.previousChecks}

toggleDetails : CommonCheck a -> CommonCheck a -> CommonCheck a
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


