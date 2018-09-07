module TimeUtil exposing (daysInMonth, isLeapYear)

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
