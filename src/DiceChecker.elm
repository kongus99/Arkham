module DiceChecker exposing (prepareCheck, runCheck, resolveCheck, view, Model, initialChecks, addResolvedCheck, clearPreviousChecks, Msg, update, clearPendingChecks)

import BoardData exposing (..)
import String
import Random
import Html exposing (Html, text, Attribute)
import Svg exposing (svg, text', tspan, image, rect)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)

diceBoxWidth = 300
diceBoxHeight = 200
leftDiceTextMargin = 25
iconSize = 16

type alias Model = { currentChecks : List DiceCheck, previousChecks : List ResolvedDiceCheck}

initialChecks = { currentChecks = [], previousChecks = []}

addResolvedCheck resolved model = {model | previousChecks = List.reverse (resolved :: (List.reverse model.previousChecks))}

clearPreviousChecks model = {model | previousChecks = []}

clearPendingChecks model = {model | currentChecks = []}

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
        UnresolvedDetailsToggle c -> {model | currentChecks = List.map (toggleDetails c) model.currentChecks}
        ResolvedDetailsToggle c -> {model | previousChecks = List.map (toggleDetails c) model.previousChecks}

toggleDetails : CommonCheck a -> CommonCheck a -> CommonCheck a
toggleDetails expectedCheck check =
    if expectedCheck == check then
        {check | isDetailed = not check.isDetailed}
    else
        check

view : Model -> Html Msg
view model =
    let
        (diceChecks, resolvedDiceChecks) = (model.currentChecks, model.previousChecks)
        totalWidth = diceBoxWidth * (List.length diceChecks + List.length resolvedDiceChecks)
        checksToPerform = List.indexedMap drawDiceCheck diceChecks
        checksPerformed = List.indexedMap drawResolvedDiceCheck resolvedDiceChecks
    in
        svg[width <| toString totalWidth, height <| toString diceBoxHeight](List.concat (List.append checksToPerform checksPerformed))

drawDiceCheck : Int -> DiceCheck -> List (Html Msg)
drawDiceCheck index check =
    let
        rectangleX = diceBoxWidth * index
        rectangleY = 0
        textMargin = round (diceBoxWidth / 2)  + diceBoxWidth * index
        imageMargin = (diceBoxWidth // 2 - round (iconSize / 2)) + diceBoxWidth * index
        imageHeight = (diceBoxHeight // 2 - round (iconSize / 2))
        fifthOfHeight = diceBoxHeight // 5
    in
        if check.isDetailed then
            [ rect [x <| toString rectangleX, y <| toString rectangleY, width <| toString diceBoxWidth, height <| toString diceBoxHeight, fill "white", stroke "black"][]
            , info textMargin fifthOfHeight <| [text <| testName check.checkType]
            , info textMargin (2 * fifthOfHeight) <| [text <| String.append "Location: " (toString check.location)]
            , info textMargin (3 * fifthOfHeight) <| [text <| String.append "Dices available: " (toString check.dicesAmount)]
            , info textMargin (4 * fifthOfHeight) <| [text <| String.append "Successes required: " (toString check.requiredSuccesses)]
            , rect [x <| toString rectangleX, y <| toString rectangleY, width <| toString diceBoxWidth, height <| toString diceBoxHeight, opacity "0.0", onClick <| UnresolvedDetailsToggle check][]]
        else
            [icon imageMargin imageHeight "sneak.png" [onClick <| UnresolvedDetailsToggle check]]

info margin height content =
    text' [x <| toString margin, y <| toString height, textLength <| toString (5 * diceBoxWidth / 6), lengthAdjust "spacingAndGlyphs", fontFamily "Verdana", textAnchor "middle"] content

icon xPos yPos link attributes =
    let
        commonAttributes = [xlinkHref link, x <| toString xPos, y <| toString yPos, height <| toString iconSize, width <| toString iconSize]
    in
        image (List.append commonAttributes attributes) []

drawResolvedDiceCheck : Int -> ResolvedDiceCheck -> List (Html Msg)
drawResolvedDiceCheck index check =
    let
        textMargin = round (diceBoxWidth / 2)  + diceBoxWidth * index
        imageMargin = (diceBoxWidth // 2 - round (iconSize / 2)) + diceBoxWidth * index
        imageHeight = (diceBoxHeight // 2 - round (iconSize / 2))
        rectangleX = diceBoxWidth * index
        rectangleY = 0
    in
        case (check.wasSuccess, check.isDetailed) of
            (_, True) -> [ rect [x <| toString rectangleX, y <| toString rectangleY, width <| toString diceBoxWidth, height <| toString diceBoxHeight, fill "white", stroke "black"][]
                         , info textMargin (diceBoxHeight // 5) <| [text <| testName check.checkType]
                         , info textMargin (diceBoxHeight // 2) <| (List.map singleDice check.dices)
                         , rect [x <| toString rectangleX, y <| toString rectangleY, width <| toString diceBoxWidth, height <| toString diceBoxHeight, opacity "0.0", onClick <| ResolvedDetailsToggle check][]]
            (True, _) -> [icon imageMargin imageHeight "ok.jpg" [onClick <| ResolvedDetailsToggle check]]
            (False, _) -> [icon imageMargin imageHeight "notOk.png" [onClick <| ResolvedDetailsToggle check]]

singleDice : (Int, WasSuccess) -> Html a
singleDice (faceValue, wasSuccess) = tspan  [fill (diceStyle wasSuccess), fontWeight "bold"] [ text (toString faceValue) ]

diceStyle: WasSuccess -> String
diceStyle wasSuccess =
    if wasSuccess then
        "green"
    else
        "red"
