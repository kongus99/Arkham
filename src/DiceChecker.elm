module DiceChecker exposing (prepareCheck, runCheck, resolveCheck, DiceCheck, ResolvedDiceCheck, CheckType(..), drawDiceChecks)

import BoardData exposing (..)
import String
import Random
import Html exposing (Html, text, Attribute)
import Svg exposing (svg, text', tspan, image, rect)
import Svg.Attributes exposing (..)

diceBoxWidth = 300
diceBoxHeight = 200
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
        checksToPerform = List.indexedMap drawDiceCheck diceChecks
        checksPerformed = List.indexedMap drawResolvedDiceCheck resolvedDiceChecks
    in
        svg[width <| toString totalWidth, height <| toString diceBoxHeight](List.concat (List.append checksToPerform checksPerformed))

drawDiceCheck : Int -> DiceCheck -> List (Html a)
drawDiceCheck index check =
    let
        textMargin = leftDiceTextMargin + diceBoxWidth * index
        imageMargin = (diceBoxWidth // 2 - 8) + diceBoxWidth * index
        imageHeight = (diceBoxHeight // 2 - 8)
        fifthOfHeight = diceBoxHeight // 5
    in
        [
--          rect [x "0", y "0", width "300", height "200", fill "none", strokeWidth "2", stroke "black"][]
        image [xlinkHref "sneak.png", x <| toString imageMargin, y <| toString imageHeight, height "16", width "16"][]
        , info textMargin fifthOfHeight <| [text <| testName check.checkType]
        , info textMargin (2 * fifthOfHeight) <| [text <| String.append "Location: " (toString check.location)]
        , info textMargin (3 * fifthOfHeight) <| [text <| String.append "Dices available: " (toString check.dicesAmount)]
        , info textMargin (4 * fifthOfHeight) <| [text <| String.append "Successes required: " (toString check.requiredSuccesses)]]


baseDiceParameters margin height =  [x <| toString margin, y <| toString height, textLength "150", lengthAdjust "spacingAndGlyphs", fontFamily "Verdana"]

info margin height content = text' (baseDiceParameters margin height) content

drawResolvedDiceCheck : Int -> ResolvedDiceCheck -> List (Html a)
drawResolvedDiceCheck index check =
    let
        margin = leftDiceTextMargin + diceBoxWidth * index
    in
        [info margin (diceBoxHeight // 5) <| [text <| testName check.checkType] , info margin (diceBoxHeight // 2) <| (List.map singleDice check.dices)]

singleDice : (Int, WasSuccess) -> Html a
singleDice (faceValue, wasSuccess) = tspan  [fill (diceStyle wasSuccess), fontWeight "bold"] [ text (toString faceValue) ]

diceStyle: WasSuccess -> String
diceStyle wasSuccess =
    if wasSuccess then
        "green"
    else
        "red"
