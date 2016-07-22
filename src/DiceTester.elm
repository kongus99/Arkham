module DiceTester exposing (..)

import DiceRoller

import Array exposing (toList, get, set, repeat, Array)
import Html exposing (Html, button, div, text)
import Html.App as App
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import Random

type alias TestResult = Bool
type alias NumOfSuccesses = Int
type alias NumOfDices = Int


-- MODEL

type alias Model = { rolls : Array DiceRoller.Model, numOfSuccesses : NumOfSuccesses }
initialModel successesRequired availableDices = { rolls = repeat availableDices DiceRoller.initialModel, numOfSuccesses = successesRequired }

-- UPDATE

type Msg = Run | SetNewResult Int DiceRoller.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update message model =
  case message of
    Run ->
      let
        prepareNewResult i = Random.generate (SetNewResult i) (Random.map DiceRoller.NewFace (Random.int 1 6))
        rollCalls = Array.initialize (Array.length model.rolls) prepareNewResult
      in
        (model, Cmd.batch <| toList rollCalls)
    SetNewResult index result ->
        let
            newRoll = DiceRoller.update result DiceRoller.initialModel
            newRolls = set index newRoll model.rolls
        in
            ({model | rolls = newRolls}, Cmd.none)

-- VIEW
view : Model -> Html Msg
view model = div[][ div[style [("display", "flex"),("flex-flow", "row wrap"),("justify-content", "space-around")]] (toList (Array.indexedMap mapper model.rolls))
                  , button [ onClick Run ] [ text "Run" ]]

mapper : Int -> DiceRoller.Model -> Html Msg
mapper i r =
    App.map (SetNewResult i) (DiceRoller.view r)

main =
    App.program { init = (initialModel 1 5, Cmd.none), view = view, update = update, subscriptions = \_ -> Sub.none }
