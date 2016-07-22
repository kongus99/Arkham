module DiceTester exposing (..)

import Dice

import Html exposing (Html, button, div, text, span)
import Html.App as App
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import Random
import Task

type alias TestResult = Bool
type alias NumOfTests = Int
type alias SuccessThreshold = Int
type alias DiceThrowResult = Int
type alias NumOfSuccesses = Int
-- MODEL

type alias Model = { rolls : List Dice.Model, isSuccess : TestResult}
initialModel availableDices =
    { rolls = List.repeat availableDices Dice.initialModel, isSuccess = False }

-- UPDATE
runTest : NumOfTests -> (List Int -> a) -> Cmd a
runTest numOfTests wrapper =
    Random.generate wrapper <| Random.list numOfTests (Random.int 1 6)

type Msg = SetNewResult SuccessThreshold NumOfSuccesses (List DiceThrowResult)

update : Msg -> Model -> Model
update message model =
  case message of
    SetNewResult threshold required newRolls ->
        let
            rollSuccessful res = res >= threshold
            isSuccess = (List.length (List.filter rollSuccessful newRolls)) >= required
            createNewRoll res =
                Dice.update (Dice.NewFace res (rollSuccessful res)) Dice.initialModel
        in
            {model | rolls = List.map createNewRoll newRolls, isSuccess = isSuccess}

-- VIEW
view : Model -> Html a
view model = div[][ div[style [("display", "flex"),("flex-flow", "row wrap"),("justify-content", "space-around")]] (List.map Dice.view model.rolls)
                  , span[][text <| toString model.isSuccess]]
