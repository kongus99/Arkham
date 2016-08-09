module MainModule exposing (..)

import BoardData exposing (..)
import Graphics exposing (Point, boardDim)
import MonsterBowl exposing (Monster)
import AllDict exposing (AllDict)
import Html exposing (Html, span, button, div)
import Svg exposing (svg, image, Attribute, Svg)
import Svg.Attributes exposing (width, height, xlinkHref, x, y, class)
import Html.Events exposing (on, onDoubleClick)
import Html.App as App
import Json.Decode as Json exposing ((:=), bool, andThen, object2)
import List
import List.Extra exposing (zip, break)
import Paths
import Movement
import DiceChecker exposing (..)

type alias ClickData = {clickUpdate : Model -> Model}

type alias Model = { movement : Movement.Model, investigator : Investigator, monsters : AllDict Place (List Monster) String, monsterBowl : Maybe MonsterBowl.Bowl }
initialModel = { movement = Movement.initialModel, investigator = defaultInvestigator, monsters = AllDict.empty placeOrder, monsterBowl = Nothing }

type Msg = UnspecifiedClick Point |
           Click ClickData|
           DoubleClick Place |
           ResolveDiceCheck UnresolvedCheck (List Int)

move place (shiftKey, ctrlKey) model =
    case (shiftKey, ctrlKey) of
        (False, False) ->
            {model | movement = Movement.moveTo place model.monsters model.investigator model.movement}
        (False, True) ->
            let
               (maybeMonster, bowl) = MonsterBowl.drawMonster model.monsterBowl
               monsterList = Maybe.withDefault [] (AllDict.get place model.monsters)
           in
               case maybeMonster of
                      Nothing -> model
                      Just m -> {model | monsters = AllDict.insert place (m :: monsterList) model.monsters, monsterBowl = Just bowl}
        (_, _) ->
            let
                monsterList = Maybe.withDefault [] (AllDict.get place model.monsters)
            in
                case monsterList of
                    [] ->  {model | monsters = AllDict.remove place model.monsters}
                    x :: [] -> {model | monsters = AllDict.remove place model.monsters}
                    x :: xs -> {model | monsters = AllDict.insert place xs model.monsters}

checkerClick msg model =
    {model | movement = Movement.update msg model.movement}

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        UnspecifiedClick p ->
            let
                x = Debug.log "clicked" p
            in
                (model, Cmd.none)
        DoubleClick place ->
            if Movement.pathEnd model.movement == place && Movement.isValidPath model.investigator model.movement then
                applyMoveToModel (Movement.finalizeMovement ResolveDiceCheck model.movement) model
            else
                (model, Cmd.none)
        Click data ->
            (data.clickUpdate model, Cmd.none)
        ResolveDiceCheck check results ->
            case check.checkType of
                Evade ->
                    let
                        resolved = DiceChecker.resolveCheck check results
                    in
                        applyMoveToModel (Movement.evadeCheck resolved ResolveDiceCheck model.movement) model

applyMoveToModel (movement, cmd) model=
    ({model | movement = movement}, cmd)

view : Model -> Html Msg
view model =
    div[][wholeBoard model]

wholeBoard : Model -> Html Msg
wholeBoard model =
    svg [ width <| toString boardDim.width , height <| toString boardDim.height] (List.concat[ [boardImage]
                                                    , Graphics.positionCircle model.movement.start model.investigator (\i -> class "ccc")True
                                                    , Graphics.positionCircle (Movement.pathEnd model.movement) model.investigator (\i -> class "ccc") False
                                                    , List.concatMap Graphics.monsterSquare (AllDict.toList model.monsters)
                                                    , movementLines model
                                                    , List.map (Graphics.localeCircle localeMsg) allLocation
                                                    , List.map (Graphics.streetRectangle streetMsg) allNeighborhood
                                                    , List.map (App.map <| (\msg -> Click <| ClickData <| checkerClick msg)) (DiceChecker.view model.movement.evadeTests)
                                                    ])
boardImage =
  image [xlinkHref "board.jpg", x "0", y "0", width "1606", height "2384", on "click" (Json.map UnspecifiedClick offsetPosition)][]

--Msg generators
onGeneralClick : Place -> Attribute Msg
onGeneralClick p =  on "click" ((object2(,)("ctrlKey" := bool)("shiftKey" := bool)) `andThen` msgForGeneralClick p)

msgForGeneralClick place (ctrl, shift) =
    Json.succeed <| Click <| ClickData (move place (shift, ctrl))

localeMsg : Location -> List(Attribute Msg)
localeMsg l =
    [onDoubleClick <| DoubleClick <| Locale l, onGeneralClick <| Locale l]

streetMsg : Neighborhood -> List(Attribute Msg)
streetMsg n =
    [onDoubleClick <| DoubleClick <| Street n, onGeneralClick <| Street n]

-- Movement lines
movementLines : Model -> List (Svg c)
movementLines model =
    let
        color = if Movement.isValidPath model.investigator model.movement then "green" else "red"
        lines = zip (model.movement.start :: model.movement.path) model.movement.path
    in
        List.map (Graphics.movement color) lines

-- mouse position
offsetPosition : Json.Decoder Point
offsetPosition =
    Json.object2 Point ("offsetX" := Json.int) ("offsetY" := Json.int)

main =
    App.program { init = ( initialModel, Cmd.none ), view = view, update = update, subscriptions = \_ -> Sub.none }
