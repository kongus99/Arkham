module BoardData exposing (..)

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
type Place a b
    = Street a
    | Locale b


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



