module DiceRoller exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.App as Html
import Html.Events exposing (..)
import Random

-- MODEL

type alias Model = { dieFace : Int, successThreshold : Int }

initialModel = Model 1 5

-- UPDATE

type Msg
  = NewFace Int

update : Msg -> Model -> Model
update msg model =
  case msg of
    NewFace newFace ->
      Model newFace model.successThreshold

-- VIEW

view : Model -> Html a
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