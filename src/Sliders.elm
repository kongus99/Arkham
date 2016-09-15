module Sliders exposing (createSliders, getSelectedAdjustments, getUnselectedAdjustments, SkillSet(..), Sliders, getSkillValue, Skill(..))

import List.Extra as Lists

type Skill = Speed | Sneak | Fight | Will | Lore | Luck

type SkillSet = SpeedSneak | FightWill | LoreLuck

type alias Sliders = { speed :Int, sneak :Int
                     , fight :Int, will :Int
                     , lore :Int, luck :Int
                     , speedSneak :Int
                     , fightWill :Int
                     , loreLuck : Int
                     , focus : Int}

createSliders sp sn fi wi lo lu fo = Sliders sp sn fi wi lo lu 0 0 0 fo

getSelectedAdjustments sliders =
    (SpeedSneak, sliders.speedSneak) :: (FightWill, sliders.fightWill) :: (LoreLuck, sliders.loreLuck) :: []

getUnselectedAdjustments sliders =
    let
        generateUnselected set number =
            List.filter (\n -> n /= number) [0,1,2,3] |> List.map (\n -> (set, n))
    in
        List.concat [generateUnselected SpeedSneak sliders.speedSneak, generateUnselected FightWill sliders.fightWill, generateUnselected LoreLuck sliders.loreLuck]

getSkillValue skill sliders =
    case skill of
        Speed -> sliders.speed + sliders.speedSneak
        Sneak -> sliders.sneak - sliders.speedSneak
        Fight -> sliders.fight + sliders.fightWill
        Will  -> sliders.will  - sliders.fightWill
        Lore  -> sliders.lore  + sliders.loreLuck
        Luck  -> sliders.luck  - sliders.loreLuck