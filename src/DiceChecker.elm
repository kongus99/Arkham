module DiceChecker exposing (prepareCheck, runCheck, resolveCheck, DiceCheck, ResolvedDiceCheck, CheckType(..), drawDiceChecks)

import BoardData exposing (..)
import String
import Random
import Html exposing (Html, div, text, Attribute)
import Svg exposing (svg, text', tspan)
import Svg.Attributes exposing (width, height, viewBox, preserveAspectRatio, fontSize, fontFamily, x, y, textLength, lengthAdjust, fill, fontWeight)

diceBoxWidth = 300
leftDiceTextMargin = 25

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

drawDiceChecks : (List DiceCheck, List ResolvedDiceCheck) -> Html a
drawDiceChecks (diceChecks, resolvedDiceChecks) =
    let
        totalWidth = diceBoxWidth * (List.length diceChecks + List.length resolvedDiceChecks)
    in
        svg[width <| toString totalWidth, height "200"](List.concat (List.append (List.indexedMap drawDiceCheck diceChecks)(List.indexedMap drawResolvedDiceCheck resolvedDiceChecks)))

drawDiceCheck : Int -> DiceCheck -> List (Html a)
drawDiceCheck index check =
    let
        margin = leftDiceTextMargin + diceBoxWidth * index
    in
        [info margin 40 <| [text <| testName check.checkType]
        , info margin 80 <| [text <| String.append "Location: " (toString check.location)]
        , info margin 120 <| [text <| String.append "Dices available: " (toString check.dicesAmount)]
        , info margin 160 <| [text <| String.append "Successes required: " (toString check.requiredSuccesses)]]


baseDiceParameters margin height =  [x <| toString margin, y <| toString height, textLength "150", lengthAdjust "spacingAndGlyphs", fontFamily "Verdana"]

info margin height content = text' (baseDiceParameters margin height) content

drawResolvedDiceCheck : Int -> ResolvedDiceCheck -> List (Html a)
drawResolvedDiceCheck index check =
    let
        margin = leftDiceTextMargin + diceBoxWidth * index
    in
        [info margin 40 <| [text <| testName check.checkType] , info margin 100 <| (List.map singleDice check.dices)]

singleDice : (Int, WasSuccess) -> Html a
singleDice (faceValue, wasSuccess) = tspan  [fill (diceStyle wasSuccess), fontWeight "bold"] [ text (toString faceValue) ]

diceStyle: WasSuccess -> String
diceStyle wasSuccess =
    if wasSuccess then
        "green"
    else
        "red"
