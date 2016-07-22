module DiceTester exposing (..)

import DiceRoller

--import Array exposing (toList, get, set, repeat, Array)
import Html exposing (Html, button, div, text)
import Html.App as App
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import Random

type alias TestResult = Bool
type alias NumOfSuccesses = Int
type alias NumOfDices = Int

-- MODEL

type alias Model = { rolls : List DiceRoller.Model, numOfSuccesses : NumOfSuccesses }
initialModel successesRequired availableDices = { rolls = List.repeat availableDices DiceRoller.initialModel, numOfSuccesses = successesRequired }

-- UPDATE

type Msg = Run Int | SetNewResult (List Int)

update : Msg -> Model -> (Model, Cmd Msg)
update message model =
  case message of
    Run numOfTests->
        (model, Random.generate SetNewResult <| Random.list numOfTests (Random.int 1 6))
    SetNewResult newRolls ->
        let
            createNewRoll res =
                DiceRoller.update (DiceRoller.NewFace res) DiceRoller.initialModel
        in
            ({model | rolls = List.map createNewRoll newRolls}, Cmd.none)

-- VIEW
view : Model -> Html Msg
view model = div[][ div[style [("display", "flex"),("flex-flow", "row wrap"),("justify-content", "space-around")]] (List.map DiceRoller.view model.rolls)
                  , button [ onClick (Run 5)] [ text "Run" ]]

main =
    App.program { init = (initialModel 1 5, Cmd.none), view = view, update = update, subscriptions = \_ -> Sub.none }
