module TimeInt exposing (toHour, toMinute, toMonth, toMonthDay, toMonthWeek, toSecond, toWeekday, toYear, toYearDay, toYearWeek)

import Time
import TimeConstruct
import TimeUtil


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


toYearDay : Time.Zone -> Time.Posix -> Int
toYearDay zone time =
    let
        monthsBeforeThisOne : List Int
        monthsBeforeThisOne =
            List.range 1 (toMonth zone time - 1)

        daysBeforeThisMonth : Int
        daysBeforeThisMonth =
            monthsBeforeThisOne
                |> List.map (TimeUtil.daysInMonth (Time.toYear zone time))
                |> List.sum
    in
    daysBeforeThisMonth + toMonthDay zone time


toMonthWeek : Time.Weekday -> Time.Zone -> Time.Posix -> Int
toMonthWeek firstDayOfWeek zone time =
    let
        daysSoFar : Int
        daysSoFar =
            toMonthDay zone time

        firstDay : Time.Posix
        firstDay =
            TimeConstruct.constructTime zone time (toYear zone time) (toMonth zone time) 1 0 0 0

        firstDayOffset : Int
        firstDayOffset =
            toWeekday firstDayOfWeek zone firstDay
    in
    (daysSoFar + firstDayOffset) // 7


toYearWeek : Time.Weekday -> Time.Zone -> Time.Posix -> Int
toYearWeek firstDayOfWeek zone time =
    let
        daysSoFar : Int
        daysSoFar =
            toYearDay zone time

        firstDay : Time.Posix
        firstDay =
            TimeConstruct.constructTime zone time (toYear zone time) 1 1 0 0 0

        firstDayOffset : Int
        firstDayOffset =
            toWeekday firstDayOfWeek zone firstDay
    in
    (daysSoFar + firstDayOffset) // 7


toMonth : Time.Zone -> Time.Posix -> Int
toMonth zone time =
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


toYear =
    Time.toYear
