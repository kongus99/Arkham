import BoardData exposing (..)
import Graphics exposing (Point)
import DiceTester exposing (TestData)
import MonsterBowl exposing (Monster)
import AllDict exposing (AllDict)
import Html exposing (Html, span, button)
import Svg exposing (svg, image, Attribute, Svg)
import Svg.Attributes exposing (width, height, xlinkHref, x, y)
import Html.Events exposing (on, onDoubleClick)
import Html.App as App
import Json.Decode as Json exposing ((:=), bool, andThen)
import List
import List.Extra exposing (zip, break)
import Paths


type alias M1 = { start : (Place Neighborhood Location), path : List (Place Neighborhood Location)}
m1Model = { start = Locale Train_Station, path = [] }

type alias Model = { movement : M1, investigator : Investigator, monsters : AllDict (Place Neighborhood Location) Monster String, monsterBowl : Maybe MonsterBowl.Bowl }
initialModel = { movement = m1Model, investigator = firstInvestigator, monsters = AllDict.empty placeOrder, monsterBowl = Nothing }

type Msg = UnspecifiedClick Point |
           Click (Place Neighborhood Location) |
           CtrlClick (Place Neighborhood Location) |
           DoubleClick (Place Neighborhood Location) |
           ResolveDiceTests (List TestData) (List Int)

path : Place Neighborhood Location -> Place Neighborhood Location -> List Neighborhood -> List (Place Neighborhood Location)
path p1 p2 excluded =
    if p1 == p2 then [] else
        let
           getNeighborhood p = case p of
                                Street n -> n
                                Locale l -> parent l
           start = getNeighborhood p1
           end = getNeighborhood p2
           path = List.map Street (Paths.pathBFS (Paths.createPathData end adjacent excluded) start)
        in
          case (p1, p2) of
            (Street n1, Street n2) -> Maybe.withDefault [] (List.tail path)
            (Street n, Locale l) ->   if path /= [] then List.append (Maybe.withDefault [] (List.tail path)) [p2] else []
            (Locale l, Street n) -> if List.member start excluded then [] else path
            (Locale l1, Locale l2) -> if path /= [] then List.append path [p2] else []
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        UnspecifiedClick p ->
            let
                x = Debug.log "clicked" p
            in
                (model, Cmd.none)
        DoubleClick place ->
            if isValidPath model && pathEnd model == place then
                ({model | movement = updateMove place [] model.movement}, Cmd.none)
            else
                (model, Cmd.none)
        Click place ->
            let
                toNeighborhood p =
                    case p of
                        Street s -> Just s
                        _ -> Nothing
                currentEnd = pathEnd model
                newPath = if model.movement.start == place then []
                          else if List.member place model.movement.path then List.append (fst <| break (\x -> x == place) model.movement.path) [place]
                          else if isAdjacent currentEnd place then
                            List.append model.movement.path [place]
                          else
                            path model.movement.start place <| List.filterMap toNeighborhood (AllDict.keys model.monsters)
            in
                ({model | movement = updateMove model.movement.start newPath model.movement}, Cmd.none)
        CtrlClick place ->
                if AllDict.member place model.monsters then
                    ({model | monsters = AllDict.remove place model.monsters}, Cmd.none)
                else
                    let
                        (maybeMonster, bowl) = MonsterBowl.drawMonster model.monsterBowl
                    in
                        case maybeMonster of
                               Nothing -> (model, Cmd.none)
                               Just m -> ({model | monsters = AllDict.insert place m model.monsters, monsterBowl = Just bowl}, Cmd.none)
        _ -> (model, Cmd.none)

pathEnd model =
     Maybe.withDefault model.movement.start <| List.head <| List.reverse model.movement.path

isValidPath model =
    List.length model.movement.path <= model.investigator.movementPoints

updateMove start path old =
    {old | path = path, start = start}

view : Model -> Html Msg
view model = svg [ width "1606", height "2384" ] (List.concat
                                                [ [boardImage]
                                                , (Graphics.positionCircle model.movement.start model.investigator True)
                                                , (Graphics.positionCircle (pathEnd model) model.investigator False)
                                                , (List.concatMap Graphics.obstructionSquare (AllDict.toList model.monsters))
                                                , (movementLines model)
                                                , (List.map (Graphics.localeCircle localeMsg) allLocation)
                                                , (List.map (Graphics.streetRectangle streetMsg) allNeighborhood)
                                                ])

boardImage =
  image [xlinkHref "board.jpg", x "0", y "0", width "1606", height "2384", on "click" (Json.map UnspecifiedClick offsetPosition)][]

--Msg generators

onCtrlClick : Place Neighborhood Location -> Html.Attribute Msg
onCtrlClick p =  on "click" (("ctrlKey" := bool) `andThen` msgForCtrlClick p)

msgForCtrlClick place ctrl =
    if ctrl then Json.succeed <| CtrlClick place else Json.succeed <| Click place

localeMsg : Location -> List(Attribute Msg)
localeMsg l =
    [onDoubleClick <| DoubleClick <| Locale l, onCtrlClick <| Locale l]

streetMsg : Neighborhood -> List(Attribute Msg)
streetMsg n =
    [onDoubleClick <| DoubleClick <| Street n, onCtrlClick <| Street n]

-- Movement lines
movementLines : Model -> List (Svg c)
movementLines model =
    let
        color = if isValidPath model then "green" else "red"
        lines = zip (model.movement.start :: model.movement.path) model.movement.path
    in
        List.map (Graphics.movement color) lines

-- mouse position
offsetPosition : Json.Decoder Point
offsetPosition =
    Json.object2 Point ("offsetX" := Json.int) ("offsetY" := Json.int)

main =
    App.program { init = ( initialModel, Cmd.none ), view = view, update = update, subscriptions = \_ -> Sub.none }
