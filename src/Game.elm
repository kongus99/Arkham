import Movement
import DiceTester
import Html exposing (Html, button, div, text)
import Html.App as App
import Html.Events exposing (onClick)
import Array exposing (Array)
-- MODEL

type alias Model = { board : Movement.Model, tester : DiceTester.Model }

initialModel = { board = Movement.initialModel, tester = DiceTester.initialModel 0 }

-- UPDATE

type Msg = BoardMove Movement.Msg | SneakTest (List Int)

update message model =
  case message of
    BoardMove msg ->
      case msg of
          Movement.Submit -> ({model | tester = DiceTester.initialModel 5}, DiceTester.runTest 5 SneakTest)
          _ -> ({ model | board = Movement.update msg model.board }, Cmd.none)
    SneakTest results ->
        let
            updateTester = DiceTester.update (DiceTester.SetNewResult 5 3 results) model.tester
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
