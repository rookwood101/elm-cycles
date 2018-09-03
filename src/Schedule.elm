module Schedule

import Time

type alias Schedule =
    { rules : List Rule
    , startTime : Time.Posix
    , until : UntilRule
    , zone : Time.Zone
    , firstDayOfWeek : Time.Weekday
    }

nextRecurrenceAfter : Time.Posix -> Schedule -> Time.Posix
nextRecurrenceAfter time schedule =
    List.minimum <| List.map
    ( \rule ->
         case rule of
            Minutely interval ruleParts ->
                addRuleParts time ruleParts schedule
                if afterCurrentTime then
                    return
                else 
                    add interval * minutes (and round down the seconds)
                    addRuleParts
    ) schedule.rules
    
addRuleParts : Time.Posix -> RuleParts -> Schedule -> Maybe Time.Posix
addRuleParts time ruleParts schedule =
    let time1 = 

type UntilRule
    = UntilTime Time.Posix
    | UntilForever
--    | UntilCount Int

type alias Interval = Int

type Rule
    = Minutely Interval RuleParts
    | Hourly Interval RuleParts
    | Daily Interval RuleParts
    | Weekly Interval RuleParts
    | Monthly Interval RuleParts
    | Yearly Interval RuleParts
    | SpecificTime Time.Posix

type TimePeriod
    = Minute Int
    = Hour Int
    = Day Int
    = Week Int
    = Month Int
    = Year Int

type alias RuleParts =
    { secondOfMinute : ( List Int ) -- 0 to 59
    , minuteOfHour : ( List Int ) -- 0 to 59
    , hourOfDay : ( List Int ) -- 0 to 59
    , dayOfWeek : ( List Int ) -- 0 to 6
    , dayOfMonth : ( List Int ) -- 0 to 31
    , dayOfYear : ( List Int ) -- 0 to 365
    , weekOfMonth : ( List Int ) -- 0 to 5
    , weekOfYear : ( List Int ) -- 0 to 53
    , monthOfYear : ( List Int ) -- 0 to 11
    }

sortRuleParts : RuleParts -> SortedRuleParts
sortRuleParts ruleParts time schedule =
    SortedRuleParts
        { secondOfMinute = sortRulePart (TimeInt.toSecond time) ruleParts.secondOfMinute
        , minuteOfHour = sortRulePart (TimeInt.toMinute time) ruleParts.minuteOfHour
        , hourOfDay = sortRulePart (TimeInt.toHour time) ruleParts.hourOfDay
        , dayOfWeek = sortRulePart (TimeInt.toWeekday time) ruleParts.dayOfWeek
        , dayOfMonth = sortRulePart (TimeInt.toMonthDay time) ruleParts.dayOfMonth
        , dayOfYear = sortRulePart (TimeInt.toYearDay time) ruleParts.dayOfYear
        , weekOfMonth = sortRulePart (TimeInt.toMonthWeek time schedule.firstDayOfWeek) ruleParts.weekOfMonth
        , weekOfYear = sortRulePart (TimeInt.toYearWeek time schedule.firstDayOfWeek) ruleParts.weekOfYear
        , monthOfYear = sortRulePart (TimeInt.toMonth time) ruleParts.monthOfYear
        }


sortRulePart : Int -> List Int -> List Int
sortRulePart partition rulePart =
    rulePart |> Set.fromList |> Set.toList
        |> List.partition (\x -> x >= partition) |> (\(greater, lesser) -> greater ++ lesser)

type SortedRuleParts
    = SortedRuleParts RuleParts
