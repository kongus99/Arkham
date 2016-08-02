module DiceChecker exposing (prepareCheck, runCheck, resolveCheck, DiceCheck, ResolvedDiceCheck, CheckType(..), view, Model, initialChecks, addResolvedCheck, clearPreviousChecks)

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
type alias IsDetailed = Bool

type alias CommonCheck a = {a | location : Place Neighborhood Location, checkType : CheckType, dicesAmount : Int, isDetailed : IsDetailed}
type alias DiceCheck = CommonCheck {requiredSuccesses : Int, successThreshold : Int}
type alias ResolvedDiceCheck = CommonCheck {dices : List (Int, WasSuccess), wasSuccess : WasSuccess}

type alias Model = { currentChecks : List DiceCheck, previousChecks : List ResolvedDiceCheck}
initialChecks = { currentChecks = [], previousChecks = []}

addResolvedCheck resolved model = {model | previousChecks = List.reverse (resolved :: (List.reverse model.previousChecks))}

clearPreviousChecks model = {model | previousChecks = []}


testName checkType =
    case checkType of
        Evade -> "Monster evasion"

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
        UnresolvedDetailsToggle c -> model
        ResolvedDetailsToggle c -> model

view : Model -> Html a
view model =
    let
        (diceChecks, resolvedDiceChecks) = (model.currentChecks, model.previousChecks)
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
        image [xlinkHref "sneak.png", x <| toString imageMargin, y <| toString imageHeight, height "16", width "16"][]
--        , rect [x "0", y "0", width "300", height "200", fill "none", strokeWidth "2", stroke "black"][]
        , info textMargin fifthOfHeight <| [text <| testName check.checkType]
        , info textMargin (2 * fifthOfHeight) <| [text <| String.append "Location: " (toString check.location)]
        , info textMargin (3 * fifthOfHeight) <| [text <| String.append "Dices available: " (toString check.dicesAmount)]
        , info textMargin (4 * fifthOfHeight) <| [text <| String.append "Successes required: " (toString check.requiredSuccesses)]]


baseDiceParameters margin height =
    [x <| toString margin, y <| toString height, textLength "150", lengthAdjust "spacingAndGlyphs", fontFamily "Verdana", class "diceThrowInfo"]

info margin height content =
    text' (baseDiceParameters margin height) content

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
