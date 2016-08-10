module MainModule exposing (..)

import BoardData exposing (..)
import Graphics exposing (Point, boardDim)
import MonsterBowl exposing (Monster)
import AllDict exposing (AllDict)
import Html exposing (Html, span, button, div)
import Svg exposing (svg, image, Attribute, Svg)
import Svg.Attributes exposing (width, height, xlinkHref, x, y, class)
import Html.Events exposing (on, onDoubleClick)
import Json.Decode as Json exposing ((:=), bool, andThen, object2)
import Investigators
import Html.App as App

type alias ClickData = {clickUpdate : Model -> Model}

type alias Model = { investigators : Investigators.Model, monsters : AllDict Place (List Monster) String, monsterBowl : Maybe MonsterBowl.Bowl }
initialModel = Model Investigators.initialModel (AllDict.empty placeOrder) Nothing

type Msg = UnspecifiedClick Point |
           Click ClickData|
           DoubleClick Place |
           ResolveDiceCheck (UnresolvedCheck, List Int)

locationClick place (shiftKey, ctrlKey) model =
    case (shiftKey, ctrlKey) of
        (False, False) ->
            {model | investigators = Investigators.move place model.monsters model.investigators}
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

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        UnspecifiedClick p ->
            let
                x = Debug.log "clicked" p
            in
                (model, Cmd.none)
        DoubleClick place ->
            resolveInvestigatorUpdate (Investigators.finalizeMovement place) model
        Click data ->
            (data.clickUpdate model, Cmd.none)
        ResolveDiceCheck (check, results) ->
            resolveInvestigatorUpdate (Investigators.resolveCheck check results) model

resolveInvestigatorUpdate updater model =
    let
        (newInvestigators, cmd) = updater model.investigators
    in
        ({model | investigators = newInvestigators}, Cmd.map ResolveDiceCheck cmd)

view : Model -> Html Msg
view model =
    div[][wholeBoard model]

wholeBoard : Model -> Html Msg
wholeBoard model =
    svg [ width <| toString boardDim.width , height <| toString boardDim.height] (List.concat[ [boardImage]
                                                    , Investigators.investigatorView model.investigators
                                                    , List.concatMap Graphics.monsterSquare (AllDict.toList model.monsters)
                                                    , List.map (Graphics.localeCircle localeMsg) allLocation
                                                    , List.map (Graphics.streetRectangle streetMsg) allNeighborhood
                                                    , Investigators.checkersView msgForCheckerClick model.investigators
                                                    ])
boardImage =
  image [xlinkHref "board.jpg", x "0", y "0", width "1606", height "2384", on "click" (Json.map UnspecifiedClick offsetPosition)][]

--Msg generators
onLocationClick : Place -> Attribute Msg
onLocationClick p =  on "click" ((object2(,)("ctrlKey" := bool)("shiftKey" := bool)) `andThen` msgForLocationClick p)

msgForLocationClick place (ctrl, shift) =
    Json.succeed <| Click <| ClickData (locationClick place (shift, ctrl))

msgForCheckerClick msg =
    Click <| ClickData <| (\m -> {m | investigators = Investigators.showCheckDetails msg m.investigators})

localeMsg : Location -> List(Attribute Msg)
localeMsg l =
    [onDoubleClick <| DoubleClick <| Locale l, onLocationClick <| Locale l]

streetMsg : Neighborhood -> List(Attribute Msg)
streetMsg n =
    [onDoubleClick <| DoubleClick <| Street n, onLocationClick <| Street n]

-- mouse position
offsetPosition : Json.Decoder Point
offsetPosition =
    Json.object2 Point ("offsetX" := Json.int) ("offsetY" := Json.int)

main =
    App.program { init = ( initialModel, Cmd.none ), view = view, update = update, subscriptions = \_ -> Sub.none }
