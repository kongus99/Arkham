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

type alias Model = { rolls : List Dice.Model, required : NumOfSuccesses, isSuccess : TestResult}
initialModel availableDices required =
    { rolls = List.repeat availableDices Dice.initialModel, required = required, isSuccess = False }

-- UPDATE

type Msg = Run SuccessThreshold NumOfTests | SetNewResult SuccessThreshold (List DiceThrowResult) | SetState TestResult

update : Msg -> Model -> (Model, Cmd Msg)
update message model =
  case message of
    Run threshold numOfTests ->
        (model, Random.generate (SetNewResult threshold) <| Random.list numOfTests (Random.int 1 6))
    SetNewResult threshold newRolls ->
        let
            rollSuccessful res = res >= threshold
            isSuccess = (List.length (List.filter rollSuccessful newRolls)) >= model.required
            newTask = Task.perform (\_ -> Debug.crash "This failure cannot happen.") identity (Task.succeed (SetState isSuccess))
            createNewRoll res =
                Dice.update (Dice.NewFace res (rollSuccessful res)) Dice.initialModel
        in
            ({model | rolls = List.map createNewRoll newRolls}, newTask)
    SetState s ->
        ({model | isSuccess = s}, Cmd.none)


-- VIEW
view : Model -> Html Msg
view model = div[][ div[style [("display", "flex"),("flex-flow", "row wrap"),("justify-content", "space-around")]] (List.map Dice.view model.rolls)
                  , button [ onClick (Run 4 5)] [ text "Run" ]
                  , span[][text <| toString model.isSuccess]]

main =
    App.program { init = (initialModel 5 3, Cmd.none), view = view, update = update, subscriptions = \_ -> Sub.none }
