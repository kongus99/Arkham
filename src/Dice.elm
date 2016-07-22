module Dice exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.App as Html
import Html.Events exposing (..)
import Random

-- MODEL

type alias Model = { dieFace : Int, isSuccess : Bool }

initialModel = Model 1 False

-- UPDATE

type Msg
  = NewFace Int Bool

update : Msg -> Model -> Model
update msg model =
  case msg of
    NewFace newFace isSuccess->
      Model newFace isSuccess

-- VIEW

view : Model -> Html a
view model =
  span []
    [ span [resultStyle model] [ text (toString model.dieFace) ]]

resultStyle: Model -> Attribute msg
resultStyle m =
    let
        styleArgs =
            if not m.isSuccess then
                [ ("backgroundColor", "white"), ("color", "red"), ("fontSize", "250%")]
            else
                [ ("backgroundColor", "white"), ("color", "green"), ("fontSize", "250%")]
    in
        style styleArgs