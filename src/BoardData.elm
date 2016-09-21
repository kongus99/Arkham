module BoardData exposing (..)

import List exposing (member, map)
import String exposing (split, slice, join)
import Skills exposing (Skills, initSkills)
type CheckType = Evade

type Phase = Upkeep | Movement

nextPhase phase = case phase of
                    Upkeep -> Movement
                    Movement -> Upkeep

type alias WasSuccess = Bool

type alias Throw = {dices : Int, numOfSuccesses : Int}
type alias ThrowResult = {dices : List (Int, WasSuccess), wasSuccess : WasSuccess}

type alias LocationCheck b a = {a | location : Place, checkType : CheckType, throws : List b}
type alias UnresolvedCheck = LocationCheck Throw {successThreshold : Int}
type alias ResolvedCheck = LocationCheck ThrowResult {wasSuccess : WasSuccess}

type alias Investigator = { name : String, start : Location, sanity : Int, stamina : Int, skills : Skills, card : String}

allInvestigators = Investigator "Amanda Sharpe" Bank_of_Arkham           5 5 (initSkills 1 4 1 4 1 4 3) "assets/investigators/AmandaSharpe.png"
                :: Investigator "Ashcan Pete" River_Docks                4 6 (initSkills 0 6 2 5 0 3 1) "assets/investigators/AshcanPete.png"
                :: Investigator "Bob Jenkins" General_Store              4 6 (initSkills 2 3 1 6 0 4 1) "assets/investigators/BobJenkins.png"
                :: Investigator "Carolyn Fern" Arkham_Asylum             6 4 (initSkills 0 3 1 4 2 5 2) "assets/investigators/CarolynFern.png"
                :: Investigator "Darell Simmons" Newspaper               4 6 (initSkills 2 3 2 4 0 4 2) "assets/investigators/DarrellSimmons.png"
                :: Investigator "Dexter Drake" Ye_Olde_Magick_Shoppe     5 5 (initSkills 2 4 1 3 2 3 2) "assets/investigators/DexterDrake.png"
                :: Investigator "Gloria Goldberg" Velma's_Diner          6 4 (initSkills 1 3 0 5 1 5 2) "assets/investigators/GloriaGoldberg.png"
                :: Investigator "Harvey Walters" Administration_Building 7 3 (initSkills 0 5 0 3 3 4 2) "assets/investigators/HarveyWalters.png"
                :: []

type Neighborhood = Downtown | Easttown | French_Hill |
                    Merchant_District | Miskatonic_University | Northside |
                    Rivertown | Southside | Uptown
allNeighborhood = [Downtown, Easttown, French_Hill, Merchant_District, Miskatonic_University, Northside, Rivertown, Southside, Uptown]

type Location =  Arkham_Asylum | Bank_of_Arkham | Independence_Square |
                 Hibb's_Roadhouse | Police_Station | Velma's_Diner |
                 Inner_Sanctum | Silver_Twilight_Lodge | The_Witch_House |
                 River_Docks | The_Unnamable | Unvisited_Isle |
                 Administration_Building | Library | Science_Building |
                 Curiositie_Shoppe | Newspaper | Train_Station |
                 Black_Cave | General_Store | Graveyard |
                 Historical_Society | Ma's_Boarding_House | South_Church |
                 St_Mary's_Hospital | Woods | Ye_Olde_Magick_Shoppe

allLocation = [Arkham_Asylum, Bank_of_Arkham, Independence_Square,
               Hibb's_Roadhouse, Police_Station, Velma's_Diner,
               Inner_Sanctum, Silver_Twilight_Lodge, The_Witch_House,
               River_Docks, The_Unnamable, Unvisited_Isle,
               Administration_Building, Library, Science_Building,
               Curiositie_Shoppe, Newspaper, Train_Station,
               Black_Cave, General_Store, Graveyard,
               Historical_Society, Ma's_Boarding_House, South_Church,
               St_Mary's_Hospital, Woods, Ye_Olde_Magick_Shoppe]

type Place = Street Neighborhood | Locale Location

placeOrder : Place -> String
placeOrder p = toString p


adjacent : Neighborhood -> List Neighborhood
adjacent n =
    case n of
        Downtown -> [Easttown, Merchant_District, Northside]
        Easttown -> [Downtown, Rivertown]
        French_Hill -> [Rivertown, Miskatonic_University, Southside]
        Merchant_District -> [Northside, Downtown, Rivertown, Miskatonic_University]
        Miskatonic_University -> [Merchant_District, French_Hill, Uptown]
        Northside -> [Downtown, Merchant_District]
        Rivertown -> [Easttown, Merchant_District, French_Hill]
        Southside -> [French_Hill, Uptown]
        Uptown -> [Southside, Miskatonic_University]

isAdjacent : Place -> Place -> Bool
isAdjacent p1 p2 =
    case (p1, p2) of
        (Street n1, Street n2) -> member n2 <| adjacent n1
        (Street n, Locale l) -> n == parent l
        (Locale l, Street n) -> n == parent l
        (Locale l1, Locale l2) -> False


parent : Location -> Neighborhood
parent l =
    case l of
        Arkham_Asylum -> Downtown
        Bank_of_Arkham -> Downtown
        Independence_Square -> Downtown
        Hibb's_Roadhouse -> Easttown
        Police_Station-> Easttown
        Velma's_Diner-> Easttown
        Inner_Sanctum -> French_Hill
        Silver_Twilight_Lodge -> French_Hill
        The_Witch_House -> French_Hill
        River_Docks -> Merchant_District
        The_Unnamable -> Merchant_District
        Unvisited_Isle -> Merchant_District
        Administration_Building -> Miskatonic_University
        Library -> Miskatonic_University
        Science_Building -> Miskatonic_University
        Curiositie_Shoppe -> Northside
        Newspaper -> Northside
        Train_Station -> Northside
        Black_Cave -> Rivertown
        General_Store -> Rivertown
        Graveyard -> Rivertown
        Historical_Society -> Southside
        Ma's_Boarding_House -> Southside
        South_Church -> Southside
        St_Mary's_Hospital -> Uptown
        Woods -> Uptown
        Ye_Olde_Magick_Shoppe -> Uptown

consistsOf : Neighborhood -> List Location
consistsOf n =
    case n of
    Downtown -> [Arkham_Asylum, Bank_of_Arkham, Independence_Square]
    Easttown -> [Hibb's_Roadhouse, Police_Station, Velma's_Diner]
    French_Hill -> [Inner_Sanctum, Silver_Twilight_Lodge, The_Witch_House]
    Merchant_District -> [River_Docks, The_Unnamable, Unvisited_Isle]
    Miskatonic_University -> [Administration_Building, Library, Science_Building]
    Northside -> [Curiositie_Shoppe, Newspaper, Train_Station]
    Rivertown -> [Black_Cave, General_Store, Graveyard]
    Southside -> [Historical_Society, Ma's_Boarding_House, South_Church]
    Uptown -> [St_Mary's_Hospital, Woods, Ye_Olde_Magick_Shoppe]



