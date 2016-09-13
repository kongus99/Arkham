module MainModule exposing (..)

import BoardData exposing (..)
import Graphics
import Graphics.Common exposing (Point, boardDim, sideDim)
import MonsterBowl exposing (Monster)
import AllDict exposing (AllDict)
import Html exposing (Html, span, button, div)
import Svg exposing (svg, image, Attribute, Svg, text)
import Svg.Attributes exposing (width, height, xlinkHref, x, y, class, type')
import Html.Events exposing (on, onDoubleClick, onClick)
import Json.Decode as Json exposing ((:=), bool, andThen, object2)
import Investigators
import Html.App as App
import DiceChecker

type alias ClickData = {clickUpdate : Model -> Model}

type alias Model = { phase : Phase, investigators : Investigators.Model, monsters : AllDict Place (List Monster) String, monsterBowl : Maybe MonsterBowl.Bowl }
initialModel = Model Movement Investigators.initialModel (AllDict.empty placeOrder) Nothing

type Msg = UnspecifiedClick Point |
           Click ClickData|
           EndTurn |
           ResolveDiceCheck (Investigator, List ResolvedCheck)

locationClick : Place -> ( Bool, Bool ) -> Model -> Model
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

investigatorClick investigator model =
    { model | investigators = Investigators.select investigator model.investigators}

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        UnspecifiedClick p ->
            let
                x = Debug.log "clicked" p
            in
                (model, Cmd.none)
        EndTurn ->
            ({model | phase = nextPhase model.phase}, Cmd.map ResolveDiceCheck (Investigators.prepareChecks model.investigators))
        Click data ->
            (data.clickUpdate model, Cmd.none)
        ResolveDiceCheck result ->
            ({model | investigators = Investigators.resolveChecks result model.investigators}, Cmd.none)

view : Model -> Html Msg
view model =
    div[][wholeBoard model]

wholeBoard : Model -> Html Msg
wholeBoard model =
    div[class "parent"][ svg [ width <| toString boardDim.width , height <| toString boardDim.height] (List.concat
                                                    [ [boardImage]
                                                    , Investigators.investigatorBoardView model.investigators
                                                    , List.concatMap Graphics.monsterSquare (AllDict.toList model.monsters)
                                                    , List.map (Graphics.localeCircle localeMsg) allLocation
                                                    , List.map (Graphics.streetRectangle streetMsg) allNeighborhood
                                                    , Investigators.checkersView msgForCheckerClick model.investigators
                                                    ])
         , div[][
            div[][button[type' "button", onClick EndTurn][text "End Turn"], span[][text <| toString model.phase]]
           ,svg [ width <| toString sideDim.width , height <| toString sideDim.height] (Investigators.investigatorSideView investigatorMsg model.investigators)]]

boardImage =
  image [xlinkHref "board.jpg", x "0", y "0", width <| toString boardDim.width, height <| toString boardDim.height, on "click" (Json.map UnspecifiedClick offsetPosition)][]

--Msg generators
localeMsg : Location -> List(Attribute Msg)
localeMsg l =
    [onLocationClick <| Locale l]

streetMsg : Neighborhood -> List(Attribute Msg)
streetMsg n =
    [onLocationClick <| Street n]

investigatorMsg : Investigator -> Attribute Msg
investigatorMsg i =
    onClick <| Click <| ClickData <| investigatorClick i

--Msg generator helpers
onLocationClick : Place -> Attribute Msg
onLocationClick p =  on "click" ((object2(,)("ctrlKey" := bool)("shiftKey" := bool)) `andThen` msgForLocationClick p)

msgForLocationClick : Place -> ( Bool, Bool ) -> Json.Decoder Msg
msgForLocationClick place (ctrl, shift) =
    Json.succeed <| Click <| ClickData (locationClick place (shift, ctrl))

msgForCheckerClick: DiceChecker.Msg -> Msg
msgForCheckerClick msg =
    Click <| ClickData <| (\m -> {m | investigators = Investigators.showCheckDetails msg m.investigators})

-- mouse position
offsetPosition : Json.Decoder Point
offsetPosition =
    Json.object2 Point ("offsetX" := Json.int) ("offsetY" := Json.int)

main =
    App.program { init = ( initialModel, Cmd.none ), view = view, update = update, subscriptions = \_ -> Sub.none }
