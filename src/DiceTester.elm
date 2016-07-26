module DiceTester exposing (..)

import Dice

import Html exposing (Html, button, div, text, span)
import Html.App as App
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import Random
import Task
import String

type alias TestResult = Bool
type alias NumOfTests = Int
type alias SuccessThreshold = Int
type alias DiceThrowResult = Int
type alias NumOfSuccesses = Int
-- MODEL

type alias Model = { rolls : List Dice.Model, isSuccess : TestResult, testName : String}
initialModel availableDices name =
    { rolls = List.repeat availableDices Dice.initialModel, isSuccess = False, testName = name }

createModel name threshold required newRolls =
    let
        rollSuccessful res = res >= threshold
        isSuccess = (List.length (List.filter rollSuccessful newRolls)) >= required
        createNewRoll res =
            Dice.update (Dice.NewFace res (rollSuccessful res)) Dice.initialModel
    in
        {rolls = List.map createNewRoll newRolls, isSuccess = isSuccess, testName = name }

-- UPDATE
runTest : NumOfTests -> (List Int -> a) -> Cmd a
runTest numOfTests wrapper =
    Random.generate wrapper <| Random.list numOfTests (Random.int 1 6)

-- VIEW
view : Model -> Html a
view model = div[][ div[style [("display", "flex"),("flex-flow", "row wrap"),("justify-content", "space-around")]] (List.map Dice.view model.rolls)
                  , span[][text <| String.append model.testName <| toString model.isSuccess]]
