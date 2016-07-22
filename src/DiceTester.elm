module DiceTester exposing (..)

import Dice

--import Array exposing (toList, get, set, repeat, Array)
import Html exposing (Html, button, div, text)
import Html.App as App
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import Random

type alias TestResult = Bool
type alias NumOfTests = Int
type alias SuccessThreshold = Int
type alias DiceThrowResult = Int
type alias NumOfSuccesses = Int
-- MODEL

type alias Model = { rolls : List Dice.Model}
initialModel availableDices =
    { rolls = List.repeat availableDices Dice.initialModel }

-- UPDATE

type Msg = Run SuccessThreshold NumOfTests | SetNewResult SuccessThreshold (List DiceThrowResult)

update : Msg -> Model -> (Model, Cmd Msg)
update message model =
  case message of
    Run threshold numOfTests ->
        (model, Random.generate (SetNewResult threshold) <| Random.list numOfTests (Random.int 1 6))
    SetNewResult threshold newRolls ->
        let
            createNewRoll res =
                Dice.update (Dice.NewFace res (res >= threshold)) Dice.initialModel
        in
            ({model | rolls = List.map createNewRoll newRolls}, Cmd.none)

-- VIEW
view : Model -> Html Msg
view model = div[][ div[style [("display", "flex"),("flex-flow", "row wrap"),("justify-content", "space-around")]] (List.map Dice.view model.rolls)
                  , button [ onClick (Run 4 5)] [ text "Run" ]]

main =
    App.program { init = (initialModel 5, Cmd.none), view = view, update = update, subscriptions = \_ -> Sub.none }
