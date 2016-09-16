module Skills exposing (initSkills, getCurrentAdjustments, getPossibleAdjustments, SkillSet(..), Skills, getSkillValue, Skill(..), SkillAdjustments, initialAdjustments, adjustSkill)

import List.Extra as Lists

type Skill = Speed | Sneak | Fight | Will | Lore | Luck

type SkillSet = SpeedSneak | FightWill | LoreLuck

type alias Skills = { speed :Int, sneak :Int
                     , fight :Int, will :Int
                     , lore :Int, luck :Int
                     , focus : Int}

type alias SkillAdjustments = { speedSneak :Int, fightWill :Int, loreLuck : Int }

initialAdjustments = SkillAdjustments 0 0 0

initSkills sp sn fi wi lo lu fo = Skills sp sn fi wi lo lu fo

getCurrentAdjustments adjustments =
    (SpeedSneak, adjustments.speedSneak) :: (FightWill, adjustments.fightWill) :: (LoreLuck, adjustments.loreLuck) :: []

getPossibleAdjustments adjustments =
    let
        generateUnselected (set, number) =
            List.filter (\n -> n /= number) [0,1,2,3] |> List.map (\n -> (set, n))
    in
        List.concat <| List.map generateUnselected <| getCurrentAdjustments adjustments

adjustSkill (set, value) (inv, adj) =
    let
        updateAdjustment oldVal =
            if abs (oldVal - value) > inv.skills.focus then oldVal else value
    in
        case set of
            SpeedSneak-> {adj | speedSneak = updateAdjustment adj.speedSneak }
            FightWill -> {adj | fightWill = updateAdjustment adj.fightWill }
            LoreLuck  -> {adj | loreLuck = updateAdjustment adj.loreLuck }


getSkillValue skill (skills, adjustments) =
    case skill of
        Speed -> skills.speed + adjustments.speedSneak
        Sneak -> skills.sneak - adjustments.speedSneak
        Fight -> skills.fight + adjustments.fightWill
        Will  -> skills.will  - adjustments.fightWill
        Lore  -> skills.lore  + adjustments.loreLuck
        Luck  -> skills.luck  - adjustments.loreLuck