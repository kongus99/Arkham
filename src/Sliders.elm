module Sliders exposing (createSliders, allAdjustments, SkillSet(..))


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

allAdjustments = List.concat <| (allSkillSetAdjustments SpeedSneak) :: (allSkillSetAdjustments FightWill) ::  (allSkillSetAdjustments LoreLuck) :: []

allSkillSetAdjustments set = (set, 0) :: (set, 1) :: (set, 2) :: (set, 3) :: []
