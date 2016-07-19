import String
import Element exposing (Element)
import BoardData exposing (..)
import Movement exposing (..)
import List exposing (length)

import ElmTest exposing (..)

tests : Test
tests =
    suite "Path"
        [
          test "that starts in the same neighborhood"
            (assertEqual 0 (length (path (Street Downtown) (Street Downtown) [])))
        , test "that starts in the same location"
            (assertEqual 0 (length (path (Locale Arkham_Asylum) (Locale Arkham_Asylum) [])))
        , test "that starts in different locations, same neighborhood"
            (assertEqual ([Street Downtown, Locale Bank_of_Arkham]) (path (Locale Arkham_Asylum) (Locale Bank_of_Arkham) []))
        , test "that starts in adjacent neighborhood"
            (assertEqual ([Street Southside]) (path (Street Uptown) (Street Southside) []))
        , test "that starts in neighborhood with distance 2"
            (assertEqual ([Street Southside, Street French_Hill]) (path (Street Uptown) (Street French_Hill) []))
        , test "that starts in neighborhood with max distance"
            (assertEqual ([Street Downtown,Street Merchant_District,Street Miskatonic_University,Street Uptown]) (path (Street Easttown) (Street Uptown) []))
        , test "that starts in neighborhood with max distance 2"
            (assertEqual ([Street French_Hill,Street Rivertown,Street Merchant_District,Street Northside]) (path (Street Southside) (Street Northside) []))
        , test "that starts and ends in locations with max distance"
            (assertEqual ([Street Southside,Street French_Hill,Street Rivertown,Street Easttown,Street Downtown,Locale Arkham_Asylum]) (path (Locale South_Church) (Locale Arkham_Asylum) []))
        , test "that starts in Train_Station and ends in Graveyard"
            (assertEqual ([Street Northside,Street Merchant_District,Street Rivertown, Locale Graveyard]) (path (Locale Train_Station) (Locale Graveyard) []))
        ]

main =
    runSuiteHtml tests