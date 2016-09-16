module Sliders exposing (createSliders, getCurrentAdjustments, getPossibleAdjustments, SkillSet(..), Sliders, getSkillValue, Skill(..), SkillAdjustments, initialAdjustments)

import List.Extra as Lists
import AllDict exposing (AllDict)

type Skill = Speed | Sneak | Fight | Will | Lore | Luck

type SkillSet = SpeedSneak | FightWill | LoreLuck

type alias Sliders = { speed :Int, sneak :Int
                     , fight :Int, will :Int
                     , lore :Int, luck :Int
                     , focus : Int}

type alias SkillAdjustments = { speedSneak :Int, fightWill :Int, loreLuck : Int }

initialAdjustments = SkillAdjustments 0 0 0

createSliders sp sn fi wi lo lu fo = Sliders sp sn fi wi lo lu fo

getCurrentAdjustments adjustments =
    (SpeedSneak, adjustments.speedSneak) :: (FightWill, adjustments.fightWill) :: (LoreLuck, adjustments.loreLuck) :: []

getPossibleAdjustments adjustments =
    let
        generateUnselected (set, number) =
            List.filter (\n -> n /= number) [0,1,2,3] |> List.map (\n -> (set, n))
    in
        List.concat <| List.map generateUnselected <| getCurrentAdjustments adjustments

getSkillValue skill (sliders, adjustments) =
    case skill of
        Speed -> sliders.speed + adjustments.speedSneak
        Sneak -> sliders.sneak - adjustments.speedSneak
        Fight -> sliders.fight + adjustments.fightWill
        Will  -> sliders.will  - adjustments.fightWill
        Lore  -> sliders.lore  + adjustments.loreLuck
        Luck  -> sliders.luck  - adjustments.loreLuck