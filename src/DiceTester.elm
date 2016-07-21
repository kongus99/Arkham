module DiceTester exposing (..)

import DiceRoller exposing (..)
--import List exposing (map)

import Array exposing (repeat, Array, indexedMap, toList, map, get, set)
import Html exposing (Html, button, div, text, h1, span)
import Html.App as App
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)

type alias TestResult = Bool
type alias NumOfSuccesses = Int
type alias NumOfDices = Int


-- MODEL

type alias Model = { rolls : Array DiceRoller.Model, numOfSuccesses : NumOfSuccesses, numOfDices : NumOfDices }
initialModel successesRequired availableDices = { rolls = repeat availableDices DiceRoller.initialModel, numOfSuccesses = successesRequired, numOfDices = availableDices }

-- UPDATE

type Msg = Run | SingleTest Int DiceRoller.Msg

update message model =
  case message of
    Run ->
      let
        futureRolls = map snd <| map (DiceRoller.update DiceRoller.Roll) model.rolls
        transform index cmd =
            Cmd.map (SingleTest index) cmd
        futureRollCmd = indexedMap transform futureRolls
      in
        (model, Cmd.batch <| toList futureRollCmd)
    SingleTest index result ->
        let
            oldRoll = Maybe.withDefault DiceRoller.initialModel <| get index model.rolls
            newRoll =  fst <| DiceRoller.update result oldRoll

        in
            ({model | rolls = set index newRoll model.rolls}, Cmd.none)

-- VIEW
view : Model -> Html Msg
view model = div[][ div[style [("display", "flex"),("flex-flow", "row wrap"),("justify-content", "space-around")]] (toList (Array.indexedMap mapper model.rolls))
                  , button [ onClick Run ] [ text "Run" ]]


mapper : Int -> DiceRoller.Model -> Html Msg
mapper i r =
    App.map (SingleTest i) (DiceRoller.view r)

main =
    App.program { init = (initialModel 1 20, Cmd.none), view = view, update = update, subscriptions = \_ -> Sub.none }
