module Selection exposing (..)

type Selection a = Selected a | NotSelected a

map : (a -> b) -> Maybe (a -> b) -> List (Selection a) -> List (Selection b)
map selected notSelected selections =
    let
        innerMap f1 f2 s =
             case s of
                Selected a -> Selected <| f1 a
                NotSelected a -> NotSelected <| f2 a
    in
        List.map (innerMap selected <| Maybe.withDefault selected notSelected) selections

unpack s =
    case s of
        Selected a -> a
        NotSelected a -> a

isSelected s =
    case s of
        Selected a -> True
        NotSelected a -> False