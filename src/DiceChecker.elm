module DiceChecker exposing (..)

import BoardData exposing (..)
import String
import Random
import Html exposing (Html, div, h1, text, span, Attribute)
import Html.Attributes exposing (style)

type CheckType = Evade

type alias WasSuccess = Bool

type alias CommonCheck a = {a | location : Place Neighborhood Location, checkType : CheckType, dicesAmount : Int}
type alias DiceCheck = CommonCheck {requiredSuccesses : Int, successThreshold : Int}
type alias ResolvedDiceCheck = CommonCheck {dices : List (Int, WasSuccess), wasSuccess : WasSuccess}

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

drawResolvedDiceCheck : ResolvedDiceCheck -> Html a
drawResolvedDiceCheck roll = div[][ div[style [("display", "flex"),("flex-flow", "row wrap"),("justify-content", "space-around")]] (List.map singleDice roll.dices)
                                  , span[][text <| String.append (testName roll.checkType) <| toString roll.wasSuccess]]

singleDice : (Int, WasSuccess) -> Html a
singleDice (faceValue, wasSuccess) = span [diceStyle wasSuccess] [ text (toString faceValue) ]

diceStyle: WasSuccess -> Attribute msg
diceStyle wasSuccess =
    let
        styleArgs =
            if wasSuccess then
                [ ("backgroundColor", "white"), ("color", "green"), ("fontSize", "250%")]
            else
                [ ("backgroundColor", "white"), ("color", "red"), ("fontSize", "250%")]
    in
        style styleArgs