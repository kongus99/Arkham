module DiceRoller exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.App as Html
import Html.Events exposing (..)
import Random
--import Graphics exposing(Color)



main =
  Html.program
    { init = (initialModel, Cmd.none)
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }

-- MODEL

type alias Model = { dieFace : Int, successThreshold : Int }

initialModel = Model 1 5

-- UPDATE

type Msg
  = Roll
  | NewFace Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Roll ->
      (model, Random.generate NewFace (Random.int 1 6))

    NewFace newFace ->
      (Model newFace model.successThreshold, Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
  span []
    [ span [resultStyle model] [ text (toString model.dieFace) ]]

resultStyle: Model -> Attribute msg
resultStyle m =
    let
        styleArgs =
            if m.dieFace < m.successThreshold then
                [ ("backgroundColor", "white"), ("color", "red"), ("fontSize", "250%")]
            else
                [ ("backgroundColor", "white"), ("color", "green"), ("fontSize", "250%")]
    in
        style styleArgs