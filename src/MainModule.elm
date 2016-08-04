module MainModule exposing (..)

import BoardData exposing (..)
import Graphics exposing (Point)
import MonsterBowl exposing (Monster)
import AllDict exposing (AllDict)
import Html exposing (Html, span, button, div)
import Svg exposing (svg, image, Attribute, Svg)
import Svg.Attributes exposing (width, height, xlinkHref, x, y)
import Html.Events exposing (on, onDoubleClick)
import Html.App as App
import Json.Decode as Json exposing ((:=), bool, andThen, object2)
import List
import List.Extra exposing (zip, break)
import Paths
import Movement
import DiceChecker exposing (..)

type alias ClickData = {shiftKey : Bool, ctrlKey : Bool, place : Place Neighborhood Location}

type alias Model = { movement : Movement.Model, investigator : Investigator, monsters : AllDict (Place Neighborhood Location) (List Monster) String, monsterBowl : Maybe MonsterBowl.Bowl }
initialModel = { movement = Movement.initialModel, investigator = firstInvestigator, monsters = AllDict.empty placeOrder, monsterBowl = Nothing }

type Msg = UnspecifiedClick Point |
           Click ClickData |
           DoubleClick (Place Neighborhood Location) |
           ResolveDiceCheck DiceCheck (List Int) |
           CheckerClick DiceChecker.Msg


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
            case (data.shiftKey, data.ctrlKey) of
                (False, False) ->
                    ({model | movement = Movement.moveTo data.place model.monsters model.investigator model.movement}, Cmd.none)
                (False, True) ->
                    let
                       (maybeMonster, bowl) = MonsterBowl.drawMonster model.monsterBowl
                       monsterList = Maybe.withDefault [] (AllDict.get data.place model.monsters)
                   in
                       case maybeMonster of
                              Nothing -> (model, Cmd.none)
                              Just m -> ({model | monsters = AllDict.insert data.place (m :: monsterList) model.monsters, monsterBowl = Just bowl}, Cmd.none)
                (_, _) ->
                    let
                        monsterList = Maybe.withDefault [] (AllDict.get data.place model.monsters)
                    in
                        case monsterList of
                            [] ->  ({model | monsters = AllDict.remove data.place model.monsters}, Cmd.none)
                            x :: [] -> ({model | monsters = AllDict.remove data.place model.monsters}, Cmd.none)
                            x :: xs -> ({model | monsters = AllDict.insert data.place xs model.monsters}, Cmd.none)
        ResolveDiceCheck check results ->
            case check.checkType of
                Evade ->
                    let
                        resolved = DiceChecker.resolveCheck check results
                    in
                        applyMoveToModel (Movement.evadeCheck resolved ResolveDiceCheck model.movement) model
        CheckerClick c -> ({model | movement = Movement.updateEvade c model.movement}, Cmd.none)

applyMoveToModel (movement, cmd) model=
    ({model | movement = movement}, cmd)

view : Model -> Html Msg
view model =
    div[][wholeBoard model]

wholeBoard : Model -> Html Msg
wholeBoard model =
    svg [ width "1606", height "2384" ] (List.concat[ [boardImage]
                                                    , Graphics.positionCircle model.movement.start model.investigator True
                                                    , Graphics.positionCircle (Movement.pathEnd model.movement) model.investigator False
                                                    , List.concatMap Graphics.monsterSquare (AllDict.toList model.monsters)
                                                    , movementLines model
                                                    , List.map (Graphics.localeCircle localeMsg) allLocation
                                                    , List.map (Graphics.streetRectangle streetMsg) allNeighborhood
                                                    , List.map (App.map CheckerClick) (DiceChecker.view model.movement.evadeTests)
                                                    ])
boardImage =
  image [xlinkHref "board.jpg", x "0", y "0", width "1606", height "2384", on "click" (Json.map UnspecifiedClick offsetPosition)][]

--Msg generators
onCtrlClick : Place Neighborhood Location -> Html.Attribute Msg
onCtrlClick p =  on "click" ((object2(,)("ctrlKey" := bool)("shiftKey" := bool)) `andThen` msgForCtrlClick p)

msgForCtrlClick place (ctrl, shift) =
    Json.succeed <| Click <| ClickData shift ctrl place
--    if ctrl then Json.succeed <| CtrlClick place else Json.succeed <| Click place

localeMsg : Location -> List(Attribute Msg)
localeMsg l =
    [onDoubleClick <| DoubleClick <| Locale l, onCtrlClick <| Locale l]

streetMsg : Neighborhood -> List(Attribute Msg)
streetMsg n =
    [onDoubleClick <| DoubleClick <| Street n, onCtrlClick <| Street n]

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
