module TimeConstruct exposing (constructTime)

import Iso8601
import Time


constructTime : Time.Zone -> Time.Posix -> Int -> Int -> Int -> Int -> Int -> Int
constructTime zone timeForOffset year month day hour minute second =
    Iso8601.toTime <|
        pad 4 '0' String.fromInt year
            ++ "-"
            ++ pad 2 '0' String.fromInt month
            ++ "-"
            ++ pad 2 '0' String.fromInt day
            ++ "T"
            ++ pad 2 '0' String.fromInt hour
            ++ ":"
            ++ pad 2 '0' String.fromInt minute
            ++ ":"
            ++ pad 2 '0' String.fromInt second
            ++ zoneOffsetToString (TimeUtil.zoneOffsetForTime zone timeForOffset)



-- internal functions


zoneOffsetToString : Int -> String
zoneOffsetToString zoneOffset =
    let
        hourPart : Int
        hourPart =
            abs zoneOffset / 60

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
    sign ++ String.fromInt hourPart ++ ":" ++ String.fromInt minutePart
