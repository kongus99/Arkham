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
selectNew : (a -> Bool) -> List (Selection a) -> List (Selection a)
selectNew selectCheck elements =
    let
        selectIfNecessary s =
            let
                e = unpack s
            in
                if selectCheck e then Selected e else NotSelected e
    in
        List.map selectIfNecessary elements

unpack s =
    case s of
        Selected a -> a
        NotSelected a -> a

isSelected s =
    case s of
        Selected a -> True
        NotSelected a -> False