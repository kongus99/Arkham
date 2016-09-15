module Graphics.Common exposing (..)

import BoardData exposing(..)
import Sliders exposing(SkillSet(..))

type alias Point = {x : Int, y : Int}
type alias Dimension = {width : Int, height : Int}
type alias Circle = {cx : Int, cy : Int, radius : Int}
type alias Rectangle = {x : Int, y : Int, width : Int, height : Int}
type alias Ellipse = {x : Int, y : Int, xRadius : Int, yRadius : Int}
type alias Color = String

rectangleMiddle r =
    Point (r.x + (r.width // 2)) (r.y + (r.height // 2))

circleMiddle c =
    Point c.cx c.cy

neighborhoodRectangle: Neighborhood -> Rectangle
neighborhoodRectangle n =
    case n of
        Northside ->             Rectangle 296  396 180 80
        Downtown ->              Rectangle 591  393 192 82
        Easttown ->              Rectangle 731  621 208 82
        Merchant_District ->     Rectangle 363  808 264 80
        Rivertown ->             Rectangle 720  808 182 78
        Miskatonic_University -> Rectangle 379 1177 236 84
        French_Hill ->           Rectangle 721 1219 195 84
        Uptown ->                Rectangle 469 1669 152 84
        Southside ->             Rectangle 753 1673 180 82

locationCircle: Location -> Circle
locationCircle location =
    case location of
         Train_Station ->          Circle  264  112 64
         Independence_Square ->    Circle  924  112 64
         Bank_of_Arkham ->         Circle  487  112 64
         Arkham_Asylum ->          Circle  706  112 64
         Newspaper ->              Circle  126  286 64
         Hibb's_Roadhouse ->       Circle  927  356 64
         Velma's_Diner ->          Circle 1148  351 64
         Curiositie_Shoppe ->      Circle  125  473 64
         Unvisited_Isle ->         Circle  124  759 64
         Police_Station ->         Circle 1148  551 64
         Graveyard ->              Circle 1148  845 64
         River_Docks ->            Circle  122  938 64
         The_Unnamable ->          Circle  342 1025 64
         General_Store ->          Circle  898 1017 64
         Black_Cave ->             Circle 1150 1030 64
         Science_Building ->       Circle  122 1195 64
         The_Witch_House ->        Circle 1150 1312 64
         Silver_Twilight_Lodge ->  Circle  936 1383 64
         Library ->                Circle  447 1382 64
         Administration_Building ->Circle  243 1450 64
         St_Mary's_Hospital ->     Circle  122 1701 64
         Ma's_Boarding_House ->    Circle 1149 1640 64
         South_Church ->           Circle 1058 1901 64
         Historical_Society ->     Circle  816 1976 64
         Woods ->                  Circle  562 1976 64
         Ye_Olde_Magick_Shoppe ->  Circle  282 1947 64
         _ ->                      Circle    0    0  0

sliderEllipse: (SkillSet, Int) -> Ellipse
sliderEllipse (set, adjustment) =
    case (set, adjustment) of
        (SpeedSneak, 0) -> Ellipse 128 545 15 25
        (SpeedSneak, 1) -> Ellipse 231 545 15 25
        (SpeedSneak, 2) -> Ellipse 339 545 15 25
        (SpeedSneak, 3) -> Ellipse 443 545 15 25
        (FightWill , 0) -> Ellipse 179 618 15 25
        (FightWill , 1) -> Ellipse 285 618 15 25
        (FightWill , 2) -> Ellipse 391 618 15 25
        (FightWill , 3) -> Ellipse 496 618 15 25
        (LoreLuck  , 0) -> Ellipse 128 693 15 25
        (LoreLuck  , 1) -> Ellipse 231 693 15 25
        (LoreLuck  , 2) -> Ellipse 339 693 15 25
        (LoreLuck  , 3) -> Ellipse 443 693 15 25
        _               -> Ellipse   0   0  0  0

middle place =
    case place of
        Street s -> rectangleMiddle <| neighborhoodRectangle s
        Locale l -> circleMiddle <| locationCircle l

boardDim = Dimension 1606 2384
checkDim = Dimension 150 225
fullInvestigatorDim = Dimension 350 493
sideDim = Dimension 600 1200
smallInvestigatorDim = Dimension 150 100
investigatorCardDim = Dimension 525 750