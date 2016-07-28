module DiceChecker exposing (..)

import BoardData exposing (..)
import String
import Random
import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (style)

type CheckType = Evade

type alias CommonCheck a = {a | location : Place Neighborhood Location, checkType : CheckType, dicesAmount : Int}
type alias DiceCheck = CommonCheck {requiredSuccesses : Int, successThreshold : Int}
type alias ResolvedDiceCheck = CommonCheck {dices : List (Int, Bool), wasSuccess : Bool}

prepareCheck location checkType dicesAmount requiredSuccesses successThreshold =
    {location = location, checkType = checkType, dicesAmount = dicesAmount, requiredSuccesses = requiredSuccesses, successThreshold = successThreshold}

runCheck : DiceCheck -> (List Int -> a) -> Cmd a
runCheck check wrapper =
    Random.generate wrapper <| Random.list check.dicesAmount (Random.int 1 6)

resolveCheck : DiceCheck -> List Int -> ResolvedDiceCheck
resolveCheck check results =
    let
        rollSuccessful res = res >= check.successThreshold
        dices = List.map (\r -> (r, rollSuccessful r)) results
        wasSuccess = (List.length (List.filter rollSuccessful results)) >= check.requiredSuccesses
    in
        {location = check.location, checkType = check.checkType, dicesAmount = check.dicesAmount, dices = dices, wasSuccess = wasSuccess}


testName checkType =
    case checkType of
        Evade -> "Monster evasion"

testDetails check =
    String.concat [" location: ", (toString check.location),
                   " dices available: ", (toString check.dicesAmount),
                   " successes required: ", (toString check.requiredSuccesses)]

drawDiceCheck : DiceCheck -> Html a
drawDiceCheck check = div [][h1[][text <| testName check.checkType], div[][text <| testDetails check]]