module TimeUtil exposing (daysInMonth, isLeapYear, monthToInt, weekdayToInt)

import Time


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


weekdayToInt : Time.Weekday -> Time.Weekday -> Int
weekdayToInt firstDayOfWeek weekday =
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
            case weekday of
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


monthToInt : Time.Month -> Int
monthToInt month =
    case month of
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
