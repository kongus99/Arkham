import Movement
import DiceTester
import Html exposing (Html, button, div, text)
import Html.App as App
import Html.Events exposing (onClick)
import Array exposing (Array)
import MonsterBowl
import AllDict
-- MODEL

type alias Model = { board : Movement.Model, tester : DiceTester.Model, monsterBowl : Maybe MonsterBowl.Bowl }

initialModel = { board = Movement.initialModel, tester = DiceTester.initialModel 0, monsterBowl = Nothing }

-- UPDATE

type Msg = BoardMove Movement.Msg | SneakTest Int (List Int)

update message model =
  case message of
    BoardMove msg ->
      case msg of
          Movement.Submit ->
            let
                firstMonster = List.head (AllDict.toList model.board.obstructions)
                numOfTests = case firstMonster of
                                Just (_, monster) -> model.board.investigator.sneak - monster.awareness
                                _ -> 0
             in
                ({model | tester = DiceTester.initialModel numOfTests}, DiceTester.runTest numOfTests (SneakTest 1))
          Movement.AddObstruction place _ ->
            let
                (maybeMonster, bowl) = MonsterBowl.drawMonster model.monsterBowl
                updatedBoard = Movement.update (Movement.AddObstruction place maybeMonster) model.board
            in
                ({model | board = updatedBoard, monsterBowl = Just bowl}, Cmd.none)
          _ -> ({ model | board = Movement.update msg model.board }, Cmd.none)
    SneakTest requiredSuccesses results ->
        let
            updateTester = DiceTester.update (DiceTester.SetNewResult 5 requiredSuccesses results) model.tester
            newBoard = if updateTester.isSuccess then Movement.update Movement.Submit model.board else model.board
        in
            ({model | tester = updateTester, board = newBoard}, Cmd.none)
-- VIEW

view : Model -> Html Msg
view model =
  div[][
        div [] [DiceTester.view model.tester]
       ,div [] [App.map BoardMove (Movement.view model.board)]
       ]

main =
    App.program { init = ( initialModel, Cmd.none ), view = view, update = update, subscriptions = \_ -> Sub.none }
