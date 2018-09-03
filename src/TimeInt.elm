module TimeInt exposing (toHour, toMinute, toMonth, toMonthDay, toMonthWeek, toSecond, toWeekday, toYearDay, toYearWeek)

import Time


toSecond =
    Time.toSecond


toMinute =
    Time.toMinute


toHour =
    Time.toHour


toWeekday : Time.Weekday -> Time.Zone -> Time.Posix -> Int
toWeekday firstDayOfWeek zone time =
    let
        offset =
            case firstDayOfWeek of
                Time.Sun ->
                    0

                Time.Mon ->
                    6

                Time.Tue ->
                    5

                Time.Wed ->
                    4

                Time.Thu ->
                    3

                Time.Fri ->
                    2

                Time.Sat ->
                    1

        unshifted =
            case Time.toWeekday zone time of
                Time.Sun ->
                    0

                Time.Mon ->
                    1

                Time.Tue ->
                    2

                Time.Wed ->
                    3

                Time.Thu ->
                    4

                Time.Fri ->
                    5

                Time.Sat ->
                    6
    in
    modBy (unshifted + offset) 7


toMonthDay =
    Time.toDay


toYearDay : Zone -> Posix -> Int
toYearDay zone time =
    let
        monthsBeforeThisOne : List Int
        monthsBeforeThisOne =
            List.range 1 (toMonth zone time - 1)

        daysBeforeThisMonth : Int
        daysBeforeThisMonth =
            monthsBeforeThisOne
                |> List.map (daysInMonth (Time.toYear zone time))
                |> List.sum
    in
    daysBeforeThisMonth + toMonthDay zone time


toMonthWeek : Time.Weekday -> Zone -> Posix -> Int
toMonthWeek firstDayOfWeek zone time =
    let
        daysSoFar : Int
        daysSoFar =
            toMonthDay zone time

        firstDay : Time.Posix
        firstDay =
            Iso8601.toTime <|
                pad 4 '0' String.fromInt (toYear zone time)
                    ++ "-"
                    ++ pad 2 '0' String.fromInt (toMonth zone time)
                    ++ "-01T00:00:00Z"

        firstDayOffset : Int
        firstDayOffset =
            toWeekday firstDayOfWeek zone firstDay
    in
    (daysSoFar + firstDayOffset) // 7 + 1


toYearWeek : Time.Weekday -> Zone -> Posix -> Int
toYearWeek firstDayOfWeek zone time =
    let
        daysSoFar : Int
        daysSoFar =
            toYearDay zone time

        firstDay : Time.Posix
        firstDay =
            Iso8601.toTime <|
                pad 4 '0' String.fromInt (toYear zone time) ++ "-01-01T00:00:00Z"

        firstDayOffset : Int
        firstDayOffset =
            toWeekday firstDayOfWeek zone firstDay
    in
    (daysSoFar + firstDayOffset) // 7 + 1


toMonth =
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



-- internal methods


daysInMonth : Int -> Int -> Int
daysInMonth year month =
    case month of
        1 ->
            31

        2 ->
            if isLeapYear year then
                29

            else
                28

        3 ->
            31

        4 ->
            30

        5 ->
            31

        6 ->
            30

        7 ->
            31

        8 ->
            31

        9 ->
            30

        10 ->
            31

        11 ->
            30

        12 ->
            31

        _ ->
            Debug.todo "Months can't be over 12"


isLeapYear : Int -> Bool
isLeapYear year =
    if modBy 4 year /= 0 then
        False

    else if modBy 100 year /= 0 then
        True

    else if modBy 400 year /= 0 then
        False

    else
        True
