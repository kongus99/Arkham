import Movement
import DiceTester
import Html exposing (Html, button, div, text)
import Html.App as App
import Html.Events exposing (onClick)
import Array exposing (Array)
import MonsterBowl exposing (Monster)
import AllDict
import List.Extra exposing (zip)
import BoardData exposing(..)
-- MODEL

type alias TestData = {testsToPerform :Int, location : Place Neighborhood Location, testName : String, requiredSuccesses : Int}

type alias Model = { board : Movement.Model, testers : List DiceTester.Model, monsterBowl : Maybe MonsterBowl.Bowl }

initialModel = { board = Movement.initialModel, testers = [], monsterBowl = Nothing }

-- UPDATE

type Msg = BoardMove Movement.Msg | SneakTest (List TestData) (List Int)

update message model =
  case message of
    BoardMove msg ->
      case msg of
          Movement.Submit ->
            let
                tests = prepareSneakTestData model.board
                numOfTests = List.sum <| List.map (\t -> t.testsToPerform) tests
             in
                (model, DiceTester.runTest numOfTests (SneakTest tests))
          Movement.AddObstruction place _ ->
            let
                (maybeMonster, bowl) = MonsterBowl.drawMonster model.monsterBowl
                updatedBoard = Movement.update (Movement.AddObstruction place maybeMonster) model.board
            in
                ({model | board = updatedBoard, monsterBowl = Just bowl}, Cmd.none)
          _ -> ({ model | board = Movement.update msg model.board }, Cmd.none)
    SneakTest tests results ->
        let
            testsWithRolls = testsWithResults results tests
            testers = List.map (\(res, test) -> DiceTester.createModel test.testName test.testsToPerform test.requiredSuccesses res) testsWithRolls
            failedTests = List.filter (\t -> not t.isSuccess) testers
            newBoard = if List.isEmpty failedTests then Movement.update Movement.Submit model.board else model.board
        in
            ({model | testers = testers, board = newBoard}, Cmd.none)

testsWithResults : List Int -> List TestData -> List (List Int, TestData)
testsWithResults results testData =
    let
        indexes = List.scanl (+) 0 (List.map (\t -> t.testsToPerform) testData)
        intervals = zip indexes <| Maybe.withDefault [] <| List.tail indexes
        splitResults = List.map (\(start, end) -> List.drop start (List.take end results)) intervals
    in
        zip splitResults testData

prepareSneakTestData : Movement.Model -> List TestData
prepareSneakTestData model =
    let
        monsters = AllDict.toList model.obstructions
        monstersOnPath = List.filter (\(p, m) -> not <| List.isEmpty model.path && List.member p (model.start :: model.path)) monsters
    in
        List.map (\(p, m) -> {testsToPerform =  max 0 (model.investigator.sneak - m.awareness), location = p, testName = "Sneak", requiredSuccesses= 1}) monstersOnPath
-- VIEW

view : Model -> Html Msg
view model =
  div[][
        div [] (List.map DiceTester.view model.testers)
       ,div [] [App.map BoardMove (Movement.view model.board)]
       ]

main =
    App.program { init = ( initialModel, Cmd.none ), view = view, update = update, subscriptions = \_ -> Sub.none }
