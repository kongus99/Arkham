module DiceChecker exposing (prepareCheck, runCheck, resolveCheck, DiceCheck, ResolvedDiceCheck, CheckType(..), drawDiceChecks)

import BoardData exposing (..)
import String
import Random
import Html exposing (Html, div, text, Attribute)
import Svg exposing (svg, text', tspan)
import Svg.Attributes exposing (width, height, viewBox, preserveAspectRatio, fontSize, fontFamily, x, y, textLength, lengthAdjust, fill, fontWeight)

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
    div[](List.append (List.map drawDiceCheck diceChecks)(List.map drawResolvedDiceCheck resolvedDiceChecks))

drawDiceCheck : DiceCheck -> Html a
drawDiceCheck check =
    svg [width "300", height "200", viewBox "0 0 1500 1000", preserveAspectRatio "none"]
    [title check
    , info "40%" (String.concat ["Location: ", (toString check.location)])
    , info "60%" (String.concat ["Dices available: ", (toString check.dicesAmount)])
    , info "80%" (String.concat ["Successes required: ", (toString check.requiredSuccesses)])]

title check = text' [x "33%", y "20%", fontSize "200", textLength "66%", lengthAdjust "spacingAndGlyphs", fontFamily "Verdana"][text (testName check.checkType)]
info height content = text' [x "33%", y height, fontSize "100", textLength "66%", lengthAdjust "spacingAndGlyphs", fontFamily "Verdana"][text content]

drawResolvedDiceCheck : ResolvedDiceCheck -> Html a
drawResolvedDiceCheck check =
    svg [width "300", height "200", viewBox "0 0 1500 1000", preserveAspectRatio "none"]
    [title check
    , results check
    ]

results check =
    text' [x "33%", y "50%", fontSize "100", textLength "66%", lengthAdjust "spacingAndGlyphs", fontFamily "Verdana"] (List.map singleDice check.dices)

singleDice : (Int, WasSuccess) -> Html a
singleDice (faceValue, wasSuccess) = tspan  [fill (diceStyle wasSuccess), fontWeight "bold"] [ text (toString faceValue) ]

diceStyle: WasSuccess -> String
diceStyle wasSuccess =
    if wasSuccess then
        "green"
    else
        "red"
