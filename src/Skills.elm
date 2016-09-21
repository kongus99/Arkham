module Skills exposing (initSkills, getCurrentAdjustments, getPossibleAdjustments, SkillSet(..), Skills, getSkillValue, Skill(..), SkillAdjustments, initialAdjustments, adjustSkill, approveSkills)

import List.Extra as Lists
import AllDict exposing (AllDict)

type Skill = Speed | Sneak | Fight | Will | Lore | Luck

type SkillSet = SpeedSneak | FightWill | LoreLuck

type alias Skills = { speed :Int, sneak :Int
                     , fight :Int, will :Int
                     , lore :Int, luck :Int
                     , focus : Int}

type alias SkillAdjustments = { currentAdjustments : AllDict SkillSet Int String, futureAdjustments : AllDict SkillSet Int String }

initialAdjustments = SkillAdjustments (AllDict.fromList toString ((SpeedSneak, 0) ::(FightWill, 0) :: (LoreLuck, 0) :: [])) (AllDict.empty toString)

initSkills sp sn fi wi lo lu fo = Skills sp sn fi wi lo lu fo

getCurrentAdjustment : SkillSet -> SkillAdjustments -> Int
getCurrentAdjustment skillSet adjustments =
    let
        current = Maybe.withDefault 0 <| AllDict.get skillSet adjustments.currentAdjustments
    in
        Maybe.withDefault current <| AllDict.get skillSet adjustments.futureAdjustments

getCurrentAdjustments : SkillAdjustments -> List (SkillSet, Int)
getCurrentAdjustments adjustments = (SpeedSneak, getCurrentAdjustment SpeedSneak adjustments)
                                  ::(FightWill, getCurrentAdjustment FightWill adjustments)
                                  ::(LoreLuck, getCurrentAdjustment LoreLuck adjustments)
                                  ::[]
getPossibleAdjustments : SkillAdjustments -> List (SkillSet, Int)
getPossibleAdjustments adjustments =
    let
        generateUnselected (set, number) =
            List.filter (\n -> n /= number) [0,1,2,3] |> List.map (\n -> (set, n))
    in
        List.concat <| List.map generateUnselected <| getCurrentAdjustments adjustments

adjustSkill : (SkillSet, Int) -> (Int, SkillAdjustments) -> SkillAdjustments
adjustSkill (set, value) (focus, adj) =
    let
        newFutureAdjustments = AllDict.insert set value adj.futureAdjustments
        usedFocus = getUsedFocus newFutureAdjustments adj.currentAdjustments
    in
        if usedFocus <= focus then {adj | futureAdjustments = newFutureAdjustments} else adj

getUsedFocus future current =
    let
        futureKeys = AllDict.keys future
        getDiff key =
            let
                fValue = Maybe.withDefault 0 <| AllDict.get key future
                cValue = Maybe.withDefault 0 <| AllDict.get key current
            in
               abs (fValue - cValue)
    in
        List.foldl (+) 0 (List.map getDiff futureKeys)

approveSkills : SkillAdjustments -> SkillAdjustments
approveSkills adjustments =
    {adjustments | futureAdjustments = AllDict.empty toString, currentAdjustments = AllDict.union adjustments.futureAdjustments adjustments.currentAdjustments }

getSkillValue : Skill -> (Skills, SkillAdjustments) -> Int
getSkillValue skill (skills, adjustments) =
    case skill of
        Speed -> skills.speed + getCurrentAdjustment SpeedSneak adjustments
        Sneak -> skills.sneak - getCurrentAdjustment SpeedSneak adjustments
        Fight -> skills.fight + getCurrentAdjustment FightWill adjustments
        Will  -> skills.will  - getCurrentAdjustment FightWill adjustments
        Lore  -> skills.lore  + getCurrentAdjustment LoreLuck adjustments
        Luck  -> skills.luck  - getCurrentAdjustment LoreLuck adjustments