module Paths exposing (pathBFS, createPathData)

import List exposing (..)
import Maybe exposing (withDefault)

type alias PathData a =
    { goal : a
    , adjacent : a -> List a
    , excluded : List a
    }
createPathData: a -> (a -> List a) -> List a -> PathData a
createPathData g adj excl=
    { goal = g
    , adjacent = adj
    , excluded = excl
    }

pathBFS: PathData a -> a -> List a
pathBFS data start =
    let
        pathExpanded = pathExpand data [] [(Nothing, start)]
    in
        case pathExpanded of
            [] -> []
            x :: xs -> reverse (pathCollapse x xs)

pathExpand: PathData a -> List (Maybe a, a) -> List (Maybe a, a) -> List (Maybe a, a)
pathExpand data visitedWithParent unvisitedWithParent =
    case unvisitedWithParent of
        [] -> []
        x :: xs ->
            if snd x == data.goal then x :: visitedWithParent else
            let
                currentNode = snd x
                allUsed = map snd (append visitedWithParent unvisitedWithParent)
                isUnused x = not (member x (append data.excluded allUsed))
                unused = filter isUnused (data.adjacent currentNode)
                unusedWithParent = map (\n -> (Just currentNode, n)) unused
            in
                pathExpand {data | excluded = (currentNode :: data.excluded)} (x :: visitedWithParent) (append xs unusedWithParent)

pathCollapse: (Maybe a, a) -> List (Maybe a, a) -> List a
pathCollapse currentData pathToCollapse =
    let
        (maybeParent, current) = currentData
    in
        case (maybeParent, pathToCollapse) of
            (Nothing, _) -> [current]
            (_, []) -> [current]
            (Just parent, (maybeNextParent, supposedCurrentParent) :: xs) ->
                if parent == supposedCurrentParent
                then current :: pathCollapse (maybeNextParent, supposedCurrentParent) xs
                else pathCollapse currentData xs