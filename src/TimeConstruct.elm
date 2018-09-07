module TimeConstruct exposing (constructTime, constructTimeUtc)

import Iso8601
import Time


constructTime : Time.Zone -> Time.Posix -> Int -> Int -> Int -> Int -> Int -> Int -> Time.Posix
constructTime zone timeForOffset year month day hour minute second =
    let
        result =
            Iso8601.toTime <|
                Debug.log "date" <|
                    String.pad 4 '0' (String.fromInt year)
                        ++ "-"
                        ++ String.pad 2 '0' (String.fromInt month)
                        ++ "-"
                        ++ String.pad 2 '0' (String.fromInt day)
                        ++ "T"
                        ++ String.pad 2 '0' (String.fromInt hour)
                        ++ ":"
                        ++ String.pad 2 '0' (String.fromInt minute)
                        ++ ":"
                        ++ String.pad 2 '0' (String.fromInt second)
                        ++ zoneOffsetToString (zoneOffsetForTime zone timeForOffset)
    in
    case result of
        Ok newTime ->
            newTime

        Err _ ->
            Debug.todo "badly formatted date"


constructTimeUtc : Int -> Int -> Int -> Int -> Int -> Int -> Time.Posix
constructTimeUtc year month day hour minute second =
    let
        result =
            Iso8601.toTime <|
                Debug.log "dateutc" <|
                    String.pad 4 '0' (String.fromInt year)
                        ++ "-"
                        ++ String.pad 2 '0' (String.fromInt month)
                        ++ "-"
                        ++ String.pad 2 '0' (String.fromInt day)
                        ++ "T"
                        ++ String.pad 2 '0' (String.fromInt hour)
                        ++ ":"
                        ++ String.pad 2 '0' (String.fromInt minute)
                        ++ ":"
                        ++ String.pad 2 '0' (String.fromInt second)
                        ++ "Z"
    in
    case result of
        Ok newTime ->
            newTime

        Err _ ->
            Debug.todo "badly formatted dateutc"



-- internal functions


zoneOffsetToString : Int -> String
zoneOffsetToString zoneOffset =
    let
        hourPart : Int
        hourPart =
            abs zoneOffset // 60

        minutePart : Int
        minutePart =
            abs zoneOffset - (hourPart * 60)

        sign : String
        sign =
            if zoneOffset >= 0 then
                "+"

            else
                "-"
    in
    sign ++ String.pad 2 '0' (String.fromInt hourPart) ++ ":" ++ String.pad 2 '0' (String.fromInt minutePart)


zoneOffsetForTime : Time.Zone -> Time.Posix -> Int
zoneOffsetForTime zone time =
    let
        zoneYears =
            Time.toYear zone time

        zoneMonths =
            case Time.toMonth zone time of
                Time.Jan ->
                    1

                Time.Feb ->
                    2

                Time.Mar ->
                    3

                Time.Apr ->
                    4

                Time.May ->
                    5

                Time.Jun ->
                    6

                Time.Jul ->
                    7

                Time.Aug ->
                    8

                Time.Sep ->
                    9

                Time.Oct ->
                    10

                Time.Nov ->
                    11

                Time.Dec ->
                    12

        zoneDays =
            Time.toDay zone time

        zoneHours =
            Time.toHour zone time

        zoneMinutes =
            Time.toMinute zone time

        zoneSeconds =
            Time.toSecond zone time

        zoneTime =
            constructTimeUtc zoneYears zoneMonths zoneDays zoneHours zoneMinutes zoneSeconds
    in
    (Time.posixToMillis zoneTime - Time.posixToMillis time) // 60000
