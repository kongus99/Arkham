module MonsterBowl exposing (Monster, Bowl, drawMonster)

import Array exposing (Array, fromList, get, length)

type alias Monster = { name : String, awareness : Int }

m n a = {name = n, awareness = a}

allMonsters = m "Cultist" -3 :: m "Deep One" 0 :: m "Zombie" 1 :: []

type alias Bowl = { monsters : Array Monster, index : Int }

initialBowl = {monsters = fromList allMonsters, index = 0 }
drawMonster : Maybe Bowl -> (Maybe Monster, Bowl)
drawMonster maybeBowl =
    case maybeBowl of
        Nothing ->
            (get 0 initialBowl.monsters, {initialBowl | index = 1})
        Just b ->
            (get b.index initialBowl.monsters, {b | index = (b.index + 1) % (length b.monsters)})
