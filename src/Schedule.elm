module Schedule exposing (Rule(..), RuleParts, Schedule, UntilRule, nextRecurrenceAfter)

import Set
import Time
import TimeConstruct
import TimeInt
import TimeUtil


type alias Schedule =
    { rules : List Rule
    , startTime : Time.Posix
    , until : Time.Posix
    , zone : Time.Zone
    , firstDayOfWeek : Time.Weekday
    }


nextRecurrenceAfter : Time.Posix -> Schedule -> Maybe Time.Posix
nextRecurrenceAfter time schedule =
    Maybe.map Time.millisToPosix
        (List.minimum <|
            List.filterMap
                (\rule ->
                    let
                        nextRecurrenceAfterHelperShort =
                            nextRecurrenceAfterHelper time schedule schedule.startTime
                    in
                    case rule of
                        Minutely interval ruleParts ->
                            nextRecurrenceAfterHelperShort ruleParts interval MinuteInterval

                        Hourly interval ruleParts ->
                            nextRecurrenceAfterHelperShort ruleParts interval HourInterval

                        Daily interval ruleParts ->
                            nextRecurrenceAfterHelperShort ruleParts interval DayInterval

                        Weekly interval ruleParts ->
                            nextRecurrenceAfterHelperShort ruleParts interval WeekInterval

                        Monthly interval ruleParts ->
                            nextRecurrenceAfterHelperShort ruleParts interval MonthInterval

                        Yearly interval ruleParts ->
                            nextRecurrenceAfterHelperShort ruleParts interval YearInterval

                        SpecificTime specificTime ->
                            if Time.posixToMillis specificTime > Time.posixToMillis time then
                                Just (Time.posixToMillis specificTime)

                            else
                                Nothing
                )
                schedule.rules
        )


nextRecurrenceAfterHelper : Time.Posix -> Schedule -> Time.Posix -> RuleParts -> Int -> IntervalUnit -> Maybe Int
nextRecurrenceAfterHelper time schedule baseTime ruleParts interval intervalUnit =
    let
        timeMillis =
            Time.posixToMillis time

        newTime =
            addRuleParts baseTime ruleParts schedule

        newTimeMillis =
            Time.posixToMillis newTime
    in
    if newTimeMillis >= Time.posixToMillis schedule.until then
        Nothing

    else
        let
            newTimeValidation =
                addRuleParts newTime ruleParts schedule

            newTimeMillisValidation =
                Time.posixToMillis newTimeValidation
        in
        if newTimeMillis == newTimeMillisValidation then
            if newTimeMillis > timeMillis then
                Just newTimeMillis

            else
                nextRecurrenceAfterHelper time schedule (addInterval schedule.firstDayOfWeek schedule.zone newTime interval intervalUnit) ruleParts interval intervalUnit

        else
            nextRecurrenceAfterHelperValidationOnly time schedule newTimeValidation ruleParts interval intervalUnit


nextRecurrenceAfterHelperValidationOnly : Time.Posix -> Schedule -> Time.Posix -> RuleParts -> Int -> IntervalUnit -> Maybe Int
nextRecurrenceAfterHelperValidationOnly time schedule timeToBeValidated ruleParts interval intervalUnit =
    let
        timeMillis =
            Time.posixToMillis time

        timeToBeValidatedMillis =
            Time.posixToMillis timeToBeValidated

        timeValidation =
            addRuleParts timeToBeValidated ruleParts schedule

        timeValidationMillis =
            Time.posixToMillis timeValidation
    in
    if timeValidationMillis >= Time.posixToMillis schedule.until then
        Nothing

    else if timeToBeValidatedMillis == timeValidationMillis then
        if timeValidationMillis > timeMillis then
            Just timeValidationMillis

        else
            nextRecurrenceAfterHelper time schedule (addInterval schedule.firstDayOfWeek schedule.zone timeValidation interval intervalUnit) ruleParts interval intervalUnit

    else
        nextRecurrenceAfterHelperValidationOnly time schedule timeValidation ruleParts interval intervalUnit


addRuleParts : Time.Posix -> RuleParts -> Schedule -> Time.Posix
addRuleParts time ruleParts schedule =
    let
        addRulePart =
            addRulePartHelper schedule.firstDayOfWeek schedule.zone
    in
    time
        |> addRulePart ruleParts.year Year True
        |> addRulePart ruleParts.monthOfYear MonthOfYear False
        |> addRulePart ruleParts.weekOfYear WeekOfYear False
        |> addRulePart ruleParts.weekOfMonth WeekOfMonth False
        |> addRulePart ruleParts.dayOfYear DayOfYear False
        |> addRulePart ruleParts.dayOfMonth DayOfMonth False
        |> addRulePart ruleParts.dayOfWeek DayOfWeek False
        |> addRulePart ruleParts.hourOfDay HourOfDay False
        |> addRulePart ruleParts.minuteOfHour MinuteOfHour False
        |> addRulePart ruleParts.secondOfMinute SecondOfMinute False


addRulePartHelper : Time.Weekday -> Time.Zone -> RulePart -> TimeUnit -> Bool -> Time.Posix -> Time.Posix
addRulePartHelper firstDayOfWeek zone rulePart timeUnit discardLesserUnits time =
    case List.head (sortRulePart (getTimeUnit firstDayOfWeek zone time timeUnit) rulePart discardLesserUnits) of
        Just timePart ->
            shiftAndRoundTime firstDayOfWeek zone time timePart timeUnit

        Nothing ->
            time


type UntilRule
    = UntilTime Time.Posix



--    | UntilCount Int


type alias Interval =
    Int


type Rule
    = Minutely Interval RuleParts
    | Hourly Interval RuleParts
    | Daily Interval RuleParts
    | Weekly Interval RuleParts
    | Monthly Interval RuleParts
    | Yearly Interval RuleParts
    | SpecificTime Time.Posix


type IntervalUnit
    = MinuteInterval
    | HourInterval
    | DayInterval
    | WeekInterval
    | MonthInterval
    | YearInterval


addMillis : Time.Posix -> Int -> Time.Posix
addMillis time millis =
    Time.millisToPosix <| millis + Time.posixToMillis time


addInterval : Time.Weekday -> Time.Zone -> Time.Posix -> Int -> IntervalUnit -> Time.Posix
addInterval firstDayOfWeek zone time interval intervalUnit =
    let
        timeMillis =
            Time.posixToMillis time
    in
    case intervalUnit of
        MinuteInterval ->
            let
                newTime =
                    addMillis time (interval * minute)
            in
            TimeConstruct.constructTime zone newTime (TimeInt.toYear zone newTime) (TimeInt.toMonth zone newTime) (TimeInt.toMonthDay zone newTime) (TimeInt.toHour zone newTime) (TimeInt.toMinute zone newTime) 0

        HourInterval ->
            let
                newTime =
                    addMillis time (interval * hour)
            in
            TimeConstruct.constructTime zone newTime (TimeInt.toYear zone newTime) (TimeInt.toMonth zone newTime) (TimeInt.toMonthDay zone newTime) (TimeInt.toHour zone newTime) 0 0

        DayInterval ->
            let
                newTime =
                    addMillis time (interval * day)
            in
            TimeConstruct.constructTime zone newTime (TimeInt.toYear zone newTime) (TimeInt.toMonth zone newTime) (TimeInt.toMonthDay zone newTime) 0 0 0

        WeekInterval ->
            let
                newTime =
                    addMillis time (interval * 7 * day)
            in
            TimeConstruct.constructTime zone newTime (TimeInt.toYear zone newTime) (TimeInt.toMonth zone newTime) (TimeInt.toMonthDay zone newTime - TimeInt.toWeekday firstDayOfWeek zone newTime) 0 0 0

        MonthInterval ->
            let
                newTime =
                    addMonths zone time interval
            in
            TimeConstruct.constructTime zone newTime (TimeInt.toYear zone newTime) (TimeInt.toMonth zone newTime) 1 0 0 0

        YearInterval ->
            let
                newTime =
                    addYears zone time interval
            in
            TimeConstruct.constructTime zone newTime (TimeInt.toYear zone newTime) 1 1 0 0 0


addMonths : Time.Zone -> Time.Posix -> Int -> Time.Posix
addMonths zone time num =
    if num > 0 then
        addMonths zone (addMillis time (day * TimeUtil.daysInMonth (TimeInt.toYear zone time) (1 + TimeInt.toMonth zone time))) (num - 1)

    else
        time


addYears : Time.Zone -> Time.Posix -> Int -> Time.Posix
addYears zone time num =
    if num > 0 then
        addYears zone
            (if TimeUtil.isLeapYear (TimeInt.toYear zone time) then
                addMillis time (day * 366)

             else
                addMillis time (day * 365)
            )
            (num - 1)

    else
        time


type TimeUnit
    = SecondOfMinute
    | MinuteOfHour
    | HourOfDay
    | DayOfWeek
    | DayOfMonth
    | DayOfYear
    | WeekOfMonth
    | WeekOfYear
    | MonthOfYear
    | Year


getTimeUnit : Time.Weekday -> Time.Zone -> Time.Posix -> TimeUnit -> Int
getTimeUnit firstDayOfWeek zone time timeUnit =
    case timeUnit of
        SecondOfMinute ->
            TimeInt.toSecond zone time

        MinuteOfHour ->
            TimeInt.toMinute zone time

        HourOfDay ->
            TimeInt.toHour zone time

        DayOfWeek ->
            TimeInt.toWeekday firstDayOfWeek zone time

        DayOfMonth ->
            TimeInt.toMonthDay zone time

        DayOfYear ->
            TimeInt.toYearDay zone time

        WeekOfMonth ->
            TimeInt.toMonthWeek firstDayOfWeek zone time

        WeekOfYear ->
            TimeInt.toYearWeek firstDayOfWeek zone time

        MonthOfYear ->
            TimeInt.toMonth zone time

        Year ->
            TimeInt.toYear zone time


second =
    1000


minute =
    60 * second


hour =
    60 * minute


day =
    24 * hour


timeDiff : Int -> Int -> Int -> Int
timeDiff current desired cycleLength =
    if desired >= current then
        desired - current

    else
        (cycleLength - current) + desired


shiftAndRoundTime : Time.Weekday -> Time.Zone -> Time.Posix -> Int -> TimeUnit -> Time.Posix
shiftAndRoundTime firstDayOfWeek zone time desiredTimePart timeUnit =
    Time.millisToPosix <|
        case timeUnit of
            SecondOfMinute ->
                Time.posixToMillis time + second * timeDiff (TimeInt.toSecond zone time) desiredTimePart 60

            MinuteOfHour ->
                Time.posixToMillis time + minute * timeDiff (TimeInt.toMinute zone time) desiredTimePart 60

            HourOfDay ->
                Time.posixToMillis time + hour * timeDiff (TimeInt.toHour zone time) desiredTimePart 24

            DayOfWeek ->
                Time.posixToMillis time + day * timeDiff (TimeInt.toWeekday firstDayOfWeek zone time) desiredTimePart 7

            DayOfMonth ->
                Time.posixToMillis time + day * timeDiff (TimeInt.toMonthDay zone time) desiredTimePart (TimeUtil.daysInMonth (TimeInt.toYear zone time) (TimeInt.toMonth zone time))

            DayOfYear ->
                Time.posixToMillis time
                    + day
                    * timeDiff (TimeInt.toYearDay zone time)
                        desiredTimePart
                        (if TimeUtil.isLeapYear (TimeInt.toYear zone time) then
                            366

                         else
                            365
                        )

            WeekOfMonth ->
                Time.posixToMillis <| shiftAndRoundTimeByWeekOfMonth firstDayOfWeek zone time desiredTimePart

            WeekOfYear ->
                Time.posixToMillis <| shiftAndRoundTimeByWeekOfYear firstDayOfWeek zone time desiredTimePart

            MonthOfYear ->
                let
                    currentMonth =
                        TimeInt.toMonth zone time

                    currentYear =
                        TimeInt.toYear zone time

                    newYear =
                        if desiredTimePart >= currentMonth then
                            currentYear

                        else
                            currentYear + 1
                in
                Time.posixToMillis <| TimeConstruct.constructTime zone time newYear desiredTimePart 1 0 0 0

            Year ->
                Time.posixToMillis <| TimeConstruct.constructTime zone time desiredTimePart 1 1 0 0 0



-- TODO: make shiftAndRoundTimeByWeekOfMonth/Year more efficient


shiftAndRoundTimeByWeekOfMonth : Time.Weekday -> Time.Zone -> Time.Posix -> Int -> Time.Posix
shiftAndRoundTimeByWeekOfMonth firstDayOfWeek zone time desiredWeekOfMonth =
    let
        current =
            TimeInt.toMonthWeek firstDayOfWeek zone time
    in
    if current == desiredWeekOfMonth then
        TimeConstruct.constructTime zone time (TimeInt.toYear zone time) (TimeInt.toMonth zone time) (TimeInt.toMonthDay zone time) 0 0 0

    else if desiredWeekOfMonth < current then
        shiftAndRoundTimeByWeekOfMonth firstDayOfWeek zone (TimeConstruct.constructTime zone time (TimeInt.toYear zone time) (1 + TimeInt.toMonth zone time) 1 0 0 0) desiredWeekOfMonth

    else
        shiftAndRoundTimeByWeekOfMonth firstDayOfWeek zone (Time.millisToPosix (day + Time.posixToMillis time)) desiredWeekOfMonth


shiftAndRoundTimeByWeekOfYear : Time.Weekday -> Time.Zone -> Time.Posix -> Int -> Time.Posix
shiftAndRoundTimeByWeekOfYear firstDayOfWeek zone time desiredWeekOfYear =
    let
        current =
            TimeInt.toYearWeek firstDayOfWeek zone time
    in
    if current == desiredWeekOfYear then
        TimeConstruct.constructTime zone time (TimeInt.toYear zone time) (TimeInt.toMonth zone time) 1 0 0 0

    else if desiredWeekOfYear < current then
        shiftAndRoundTimeByWeekOfYear firstDayOfWeek zone (TimeConstruct.constructTime zone time (1 + TimeInt.toYear zone time) 1 1 0 0 0) desiredWeekOfYear

    else
        shiftAndRoundTimeByWeekOfYear firstDayOfWeek zone (Time.millisToPosix ((27 * day) + Time.posixToMillis time)) desiredWeekOfYear


type alias RuleParts =
    { secondOfMinute : List Int -- 0 to 59
    , minuteOfHour : List Int -- 0 to 59
    , hourOfDay : List Int -- 0 to 59
    , dayOfWeek : List Int -- 0 to 6
    , dayOfMonth : List Int -- 1 to 31
    , dayOfYear : List Int -- 1 to 366
    , weekOfMonth : List Int -- 0 to 5
    , weekOfYear : List Int -- 0 to 53
    , monthOfYear : List Int -- 1 to 12
    , year : List Int -- 1970 to 9999
    }


type alias RulePart =
    List Int


sortRulePart : Int -> List Int -> Bool -> List Int
sortRulePart partition rulePart discardLesserUnits =
    rulePart
        |> Set.fromList
        |> Set.toList
        |> List.partition (\x -> x >= partition)
        |> (\( greater, lesser ) ->
                if discardLesserUnits then
                    greater

                else
                    greater ++ lesser
           )


type SortedRuleParts
    = SortedRuleParts RuleParts
