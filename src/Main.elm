module Main exposing (main)

import Browser
import DateFormat
import Debug
import Form
import Form.Base.TextField
import Form.Error
import Form.Value
import Form.View
import FormCustomView
import Html exposing (..)
import Html.Attributes as Attributes
import Html.Events as Events
import Iso8601
import List.Extra
import OrderedDict as Dict
import Schedule
import Task
import Time
import TimeConstruct
import TimeUtil


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias RulePartRawValues =
    { timeUnit : Form.Value.Value String
    , timeUnitValue : Form.Value.Value String
    }


type alias RuleRawValues =
    { isPeriodic : Form.Value.Value Bool
    , specificTime : Form.Value.Value String
    , timeIntervalUnit : Form.Value.Value String
    , timeInterval : Form.Value.Value Float
    , ruleParts : List (Form.View.Model RulePartRawValues)
    }


type alias FormRawValues =
    { title : Form.Value.Value String
    , description : Form.Value.Value String
    , startTime : Form.Value.Value String
    , endTime : Form.Value.Value String
    , firstDayOfWeek : Form.Value.Value String
    , rules : List (Form.View.Model RuleRawValues)
    }


type alias Task =
    { name : String
    , description : String
    , schedule : Schedule.Schedule
    }


type alias Model =
    { tasks : List Task
    , inputName : String
    , inputDescription : String
    , inputRegularity : String
    , inputStartDate : String
    , timeZone : Time.Zone
    , timeNow : Time.Posix
    , taskRaw : Form.View.Model FormRawValues
    , isFormValid : Bool
    }


initSchedule : Schedule.Schedule
initSchedule =
    { rules =
        [ Schedule.Daily 1
            (Schedule.RuleParts [ 13 ] [] [] [] [] [] [] [] [] [])
        ]
    , startTime = TimeConstruct.constructTimeUtc 2018 9 1 13 0 0
    , until = TimeConstruct.constructTimeUtc 2018 10 1 13 0 0
    , zone = Time.utc
    , firstDayOfWeek = Time.Mon
    }


blankTaskForm =
    Form.View.idle
        { title = Form.Value.blank
        , description = Form.Value.blank
        , startTime = Form.Value.blank
        , endTime = Form.Value.blank
        , firstDayOfWeek = Form.Value.filled "Monday"
        , rules = [ blankRuleForm ]
        }


blankRuleForm =
    Form.View.idle
        { isPeriodic = Form.Value.filled True
        , specificTime = Form.Value.blank
        , timeIntervalUnit = Form.Value.blank
        , timeInterval = Form.Value.blank
        , ruleParts = [ blankRulePartForm ]
        }


blankRulePartForm =
    Form.View.idle
        { timeUnit = Form.Value.blank
        , timeUnitValue = Form.Value.blank
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        [ Task "Laundry" "Don't forget the rags!" initSchedule ]
        ""
        ""
        ""
        ""
        Time.utc
        (TimeConstruct.constructTimeUtc 2018 9 6 23 28 0)
        blankTaskForm
        True
    , Task.perform UpdateTimeNow (Task.map2 (\zone posix -> ( zone, posix )) Time.here Time.now)
    )



-- UPDATE


type Msg
    = Null
    | AddTask
    | InputName String
    | InputDescription String
    | InputRegularity String
    | InputStartDate String
    | UpdateTimeNow ( Time.Zone, Time.Posix )
    | UpdateTaskDetails (Form.View.Model FormRawValues)
    | UpdateTaskRule Int (Form.View.Model RuleRawValues)
    | UpdateRulePart Int Int (Form.View.Model RulePartRawValues)
    | NewAddTask
    | AddRule
    | AddRulePart Int
    | RemoveRule Int
    | RemoveRulePart Int Int
    | TaskFormOutput Task
    | RuleFormOutput (Schedule.RuleParts -> Schedule.Rule)
    | RulePartFormOutput TimeUnit Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        setTaskRaw r taskRaw =
            { r | taskRaw = taskRaw }

        setValues r values =
            { r | values = values }

        setRules r rules =
            { r | rules = rules }

        setRuleParts r ruleParts =
            { r | ruleParts = ruleParts }
    in
    case msg of
        Null ->
            ( model, Cmd.none )

        AddTask ->
            case makeTask model of
                Just task ->
                    ( resetInputs { model | tasks = task :: model.tasks }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        InputName name ->
            ( { model | inputName = name }, Cmd.none )

        InputDescription description ->
            ( { model | inputDescription = description }, Cmd.none )

        InputRegularity regularity ->
            ( { model | inputRegularity = regularity }, Cmd.none )

        InputStartDate startDate ->
            ( { model | inputStartDate = startDate }, Cmd.none )

        UpdateTimeNow ( zone, posix ) ->
            ( { model | timeZone = zone, timeNow = posix }, Cmd.none )

        UpdateTaskDetails newForm ->
            ( { model | taskRaw = newForm }, Cmd.none )

        UpdateTaskRule ruleIndex newForm ->
            ( List.Extra.setAt ruleIndex newForm model.taskRaw.values.rules
                |> setRules model.taskRaw.values
                |> setValues model.taskRaw
                |> setTaskRaw model
            , Cmd.none
            )

        UpdateRulePart ruleIndex rulePartIndex newForm ->
            let
                updateRulesList newValue =
                    List.Extra.setAt ruleIndex newValue model.taskRaw.values.rules

                maybeRule =
                    List.Extra.getAt ruleIndex model.taskRaw.values.rules
            in
            case maybeRule of
                Just rule ->
                    ( List.Extra.setAt rulePartIndex newForm rule.values.ruleParts
                        |> setRuleParts rule.values
                        |> setValues rule
                        |> updateRulesList
                        |> setRules model.taskRaw.values
                        |> setValues model.taskRaw
                        |> setTaskRaw model
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        NewAddTask ->
            let
                taskDetailsResult =
                    (Form.fill (taskDetailsForm model) model.taskRaw.values).result

                taskRulesResults =
                    List.map (.values >> Form.fill (taskRuleForm model) >> .result) model.taskRaw.values.rules

                rulePartsResults ruleIndex rule =
                    List.map (.values >> Form.fill (rulePartForm model ruleIndex) >> .result) rule.values.ruleParts

                taskRulePartsResults : List (Result ( Form.Error.Error, List Form.Error.Error ) Msg)
                taskRulePartsResults =
                    List.concat <| List.indexedMap rulePartsResults model.taskRaw.values.rules

                resultIsOk r =
                    case r of
                        Ok _ ->
                            True

                        _ ->
                            False

                listAllOk =
                    List.all resultIsOk
            in
            if resultIsOk taskDetailsResult && listAllOk taskRulesResults && listAllOk taskRulePartsResults then
                --( { model | isFormValid = True }, Cmd.none )
                ( { model
                    | isFormValid = True
                    , tasks =
                        case makeTaskNew model of
                            Just task ->
                                task :: model.tasks

                            Nothing ->
                                let
                                    debug =
                                        Debug.log "couldn't create task" model
                                in
                                model.tasks

                    --                    , taskRaw = blankTaskForm
                  }
                , Cmd.none
                )

            else
                ( { model | isFormValid = False }, Cmd.none )

        AddRule ->
            ( model.taskRaw.values.rules
                ++ [ blankRuleForm ]
                |> setRules model.taskRaw.values
                |> setValues model.taskRaw
                |> setTaskRaw model
            , Cmd.none
            )

        AddRulePart ruleIndex ->
            let
                updateRulesList newValue =
                    List.Extra.setAt ruleIndex newValue model.taskRaw.values.rules

                maybeRule =
                    List.Extra.getAt ruleIndex model.taskRaw.values.rules
            in
            case maybeRule of
                Just rule ->
                    ( (rule.values.ruleParts ++ [ blankRulePartForm ])
                        |> setRuleParts rule.values
                        |> setValues rule
                        |> updateRulesList
                        |> setRules model.taskRaw.values
                        |> setValues model.taskRaw
                        |> setTaskRaw model
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        RemoveRule ruleIndex ->
            ( List.Extra.removeAt ruleIndex model.taskRaw.values.rules
                |> setRules model.taskRaw.values
                |> setValues model.taskRaw
                |> setTaskRaw model
            , Cmd.none
            )

        RemoveRulePart ruleIndex rulePartIndex ->
            let
                updateRulesList newValue =
                    List.Extra.setAt ruleIndex newValue model.taskRaw.values.rules

                maybeRule =
                    List.Extra.getAt ruleIndex model.taskRaw.values.rules
            in
            case maybeRule of
                Just rule ->
                    ( List.Extra.removeAt rulePartIndex rule.values.ruleParts
                        |> setRuleParts rule.values
                        |> setValues rule
                        |> updateRulesList
                        |> setRules model.taskRaw.values
                        |> setValues model.taskRaw
                        |> setTaskRaw model
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        TaskFormOutput _ ->
            ( model, Cmd.none )

        RuleFormOutput _ ->
            ( model, Cmd.none )

        RulePartFormOutput _ _ ->
            ( model, Cmd.none )



-- TODO : this


makeTask : Model -> Maybe Task
makeTask model =
    let
        name =
            Just model.inputName

        description =
            Just model.inputDescription

        zoneOffsetString =
            TimeConstruct.zoneOffsetToString (TimeConstruct.zoneOffsetForTime model.timeZone model.timeNow)

        startTime =
            Result.toMaybe <| Iso8601.toTime <| model.inputStartDate ++ "T00:00:00" ++ zoneOffsetString

        schedule =
            Just { initSchedule | zone = model.timeZone, startTime = Maybe.withDefault model.timeNow startTime }
    in
    Maybe.map3 Task name description schedule


valueWithDefault v default =
    case Form.Value.raw v of
        Just string ->
            string

        Nothing ->
            default


type alias RuleRawValuesNotForm =
    { isPeriodic : Form.Value.Value Bool
    , specificTime : Form.Value.Value String
    , timeIntervalUnit : Form.Value.Value String
    , timeInterval : Form.Value.Value Float
    , ruleParts : List RulePartRawValues
    }


makeTaskNew : Model -> Maybe Task
makeTaskNew model =
    let
        detailsRaw =
            model.taskRaw.values

        rulePartsRaw : RuleRawValues -> RuleRawValuesNotForm
        rulePartsRaw ruleRaw =
            { isPeriodic = ruleRaw.isPeriodic
            , specificTime = ruleRaw.specificTime
            , timeIntervalUnit = ruleRaw.timeIntervalUnit
            , timeInterval = ruleRaw.timeInterval
            , ruleParts = List.map .values ruleRaw.ruleParts
            }

        rulesRaw =
            List.map (.values >> rulePartsRaw) detailsRaw.rules

        title =
            Form.Value.raw detailsRaw.title

        description =
            Just <| valueWithDefault detailsRaw.description ""

        startTime =
            detailsRaw.startTime
                |> Form.Value.raw
                |> Maybe.andThen
                    (dateStringToTime model.timeZone model.timeNow >> Result.toMaybe)

        endTime =
            valueWithDefault detailsRaw.endTime "3000-01-01"
                |> dateStringToTime model.timeZone model.timeNow
                |> Result.toMaybe

        --TODO: 3000??
        firstDayOfWeek =
            detailsRaw.firstDayOfWeek
                |> Form.Value.raw
                |> Maybe.andThen
                    (\s -> Dict.get s stringToWeekday)

        addRulePartRaw : RulePartRawValues -> Maybe Schedule.RuleParts -> Maybe Schedule.RuleParts
        addRulePartRaw rulePartRaw ruleParts =
            let
                timeUnit =
                    Form.Value.raw rulePartRaw.timeUnit

                timeUnitValue =
                    Maybe.andThen String.toInt (Form.Value.raw <| rulePartRaw.timeUnitValue)
            in
            Maybe.andThen
                (\rPs ->
                    Maybe.andThen
                        (\tUnit ->
                            Maybe.andThen
                                (\tValue ->
                                    case tUnit of
                                        "Second of Minute" ->
                                            Just { rPs | secondOfMinute = tValue :: rPs.secondOfMinute }

                                        "Minute of Hour" ->
                                            Just { rPs | minuteOfHour = tValue :: rPs.minuteOfHour }

                                        "Hour of Day" ->
                                            Just { rPs | hourOfDay = tValue :: rPs.hourOfDay }

                                        "Day of Week" ->
                                            Just { rPs | dayOfWeek = tValue :: rPs.dayOfWeek }

                                        "Day of Month" ->
                                            Just { rPs | dayOfMonth = tValue :: rPs.dayOfMonth }

                                        "Day of Year" ->
                                            Just { rPs | dayOfYear = tValue :: rPs.dayOfYear }

                                        "Week of Month" ->
                                            Just { rPs | weekOfMonth = tValue :: rPs.weekOfMonth }

                                        "Week of Year" ->
                                            Just { rPs | weekOfYear = tValue :: rPs.weekOfYear }

                                        "Month of Year" ->
                                            Just { rPs | monthOfYear = tValue :: rPs.monthOfYear }

                                        "Year" ->
                                            Just { rPs | year = tValue :: rPs.year }

                                        _ ->
                                            Nothing
                                )
                                timeUnitValue
                        )
                        timeUnit
                )
                ruleParts

        ruleRawToRule ruleRaw =
            Maybe.andThen
                (\isPeriodic ->
                    if isPeriodic then
                        let
                            ruleParts =
                                List.foldl
                                    addRulePartRaw
                                    (Just
                                        { secondOfMinute = []
                                        , minuteOfHour = []
                                        , hourOfDay = []
                                        , dayOfWeek = []
                                        , dayOfMonth = []
                                        , dayOfYear = []
                                        , weekOfMonth = []
                                        , weekOfYear = []
                                        , monthOfYear = []
                                        , year = []
                                        }
                                    )
                                    ruleRaw.ruleParts

                            timeInterval =
                                Maybe.map round (Form.Value.raw ruleRaw.timeInterval)

                            timeUnit =
                                Form.Value.raw ruleRaw.timeIntervalUnit
                        in
                        Maybe.andThen
                            (\rPs ->
                                Maybe.andThen
                                    (\tInterval ->
                                        Maybe.andThen
                                            (\tUnit ->
                                                case tUnit of
                                                    "Minutes" ->
                                                        Just (Schedule.Minutely tInterval rPs)

                                                    "Hours" ->
                                                        Just (Schedule.Hourly tInterval rPs)

                                                    "Days" ->
                                                        Just (Schedule.Daily tInterval rPs)

                                                    "Weeks" ->
                                                        Just (Schedule.Weekly tInterval rPs)

                                                    "Months" ->
                                                        Just (Schedule.Monthly tInterval rPs)

                                                    "Years" ->
                                                        Just (Schedule.Yearly tInterval rPs)

                                                    _ ->
                                                        Nothing
                                            )
                                            timeUnit
                                    )
                                    timeInterval
                            )
                            ruleParts

                    else
                        Maybe.andThen
                            (dateStringToTime model.timeZone model.timeNow
                                >> Result.toMaybe
                                >> Maybe.map Schedule.SpecificTime
                            )
                            (Form.Value.raw ruleRaw.specificTime)
                )
                (Form.Value.raw ruleRaw.isPeriodic)

        rulesWithMaybes =
            List.map ruleRawToRule rulesRaw

        rules =
            if
                List.any
                    (\maybeRule ->
                        case maybeRule of
                            Nothing ->
                                True

                            _ ->
                                False
                    )
                    rulesWithMaybes
            then
                Nothing

            else
                Just (List.filterMap identity rulesWithMaybes)

        schedule =
            Maybe.map5 Schedule.Schedule rules startTime endTime (Just model.timeZone) firstDayOfWeek
    in
    Maybe.map3 Task title description schedule


resetInputs : Model -> Model
resetInputs model =
    { model | inputName = "", inputDescription = "", inputRegularity = "", inputStartDate = "" }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ Attributes.class "page" ]
        [ div [ Attributes.class "new-task" ]
            [ formInput "name" "text" "Name" [] InputName model.inputName
            , formInput "description" "text" "Description" [] InputDescription model.inputDescription
            , formInput "regularity" "number" "Regularity" [] InputRegularity model.inputRegularity
            , formInput "startDate" "date" "Start Date" [] InputStartDate model.inputStartDate
            , div [] [ button [ Events.onClick AddTask ] [ text "Add" ] ]
            ]
        , hr [] []
        , div [ Attributes.class "tasks" ] (List.map (viewTask model) model.tasks)
        , newTaskForm model
        ]


formInput : String -> String -> String -> List (Attribute Msg) -> (String -> Msg) -> String -> Html Msg
formInput id type_ display extraAttributes inputMsg value =
    div []
        [ label [ Attributes.for id ] [ text display ]
        , input ([ Attributes.type_ type_, Attributes.id id, Attributes.name id, Events.onInput inputMsg, Attributes.value value ] ++ extraAttributes) []
        ]


viewTask : Model -> Task -> Html Msg
viewTask model task =
    div []
        [ h3 [] [ text task.name ]
        , p [] [ text <| taskDateFormatter model.timeZone task.schedule.startTime ]
        , p []
            [ text <|
                "Next: "
                    ++ (let
                            nextRecurrence =
                                Schedule.nextRecurrenceAfter model.timeNow task.schedule
                        in
                        case nextRecurrence of
                            Just recurrenceTime ->
                                taskDateFormatter model.timeZone recurrenceTime

                            Nothing ->
                                "Unavailable"
                       )
            ]
        , p [] [ text task.description ]
        ]


taskDateFormatter =
    DateFormat.format
        [ DateFormat.dayOfWeekNameFull
        , DateFormat.text " "
        , DateFormat.dayOfMonthSuffix
        , DateFormat.text " of "
        , DateFormat.monthNameFull
        , DateFormat.text " "
        , DateFormat.yearNumber
        , DateFormat.text " at "
        , DateFormat.hourNumber
        , DateFormat.text ":"
        , DateFormat.minuteFixed
        , DateFormat.text ":"
        , DateFormat.secondFixed
        , DateFormat.amPmLowercase
        ]



-- FORMS


dateStringToTime : Time.Zone -> Time.Posix -> String -> Result String Time.Posix
dateStringToTime zone time dateString =
    let
        zoneOffsetString =
            TimeConstruct.zoneOffsetToString (TimeConstruct.zoneOffsetForTime zone time)
    in
    Result.mapError (\_ -> "Invalid date string") <| Iso8601.toTime <| dateString ++ "T00:00:00" ++ zoneOffsetString


stringToWeekday =
    Dict.fromList
        [ ( "Monday", Time.Mon )
        , ( "Tuesday", Time.Tue )
        , ( "Wednesday", Time.Wed )
        , ( "Thursday", Time.Thu )
        , ( "Friday", Time.Fri )
        , ( "Saturday", Time.Sat )
        , ( "Sunday", Time.Sun )
        ]


stringToMonth =
    Dict.fromList
        [ ( "January", Time.Jan )
        , ( "February", Time.Feb )
        , ( "March", Time.Mar )
        , ( "April", Time.Apr )
        , ( "May", Time.May )
        , ( "June", Time.Jun )
        , ( "July", Time.Jul )
        , ( "August", Time.Aug )
        , ( "September", Time.Sep )
        , ( "October", Time.Oct )
        , ( "November", Time.Nov )
        , ( "December", Time.Dec )
        ]


type TimeIntervalUnit
    = Minutely
    | Hourly
    | Daily
    | Weekly
    | Monthly
    | Yearly


stringToTimeIntervalUnit =
    Dict.fromList
        [ ( "Minutes", Minutely )
        , ( "Hours", Hourly )
        , ( "Days", Daily )
        , ( "Weeks", Weekly )
        , ( "Months", Monthly )
        , ( "Years", Yearly )
        ]


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


stringToTimeUnit =
    Dict.fromList
        [ ( "Second of Minute", SecondOfMinute )
        , ( "Minute of Hour", MinuteOfHour )
        , ( "Hour of Day", HourOfDay )
        , ( "Day of Week", DayOfWeek )
        , ( "Day of Month", DayOfMonth )
        , ( "Day of Year", DayOfYear )
        , ( "Week of Month", WeekOfMonth )
        , ( "Week of Year", WeekOfYear )
        , ( "Month of Year", MonthOfYear )
        , ( "Year", Year )
        ]


type TimeUnitValue
    = IntMinMax Int Int
    | Weekday
    | Month
    | IntAll


timeUnitToParser firstDayOfWeek timeUnit =
    let
        intMinMaxParse min max value =
            Result.fromMaybe "Must be an integer" (String.toInt value)
                |> Result.andThen
                    (\valueInt ->
                        if valueInt >= min && valueInt <= max then
                            Ok valueInt

                        else
                            Err "Integer must be in range"
                    )

        getParserAndOptions min max =
            ( intMinMaxParse min max, pairList <| List.map String.fromInt <| List.range 0 59 )
    in
    case timeUnit of
        SecondOfMinute ->
            getParserAndOptions 0 59

        MinuteOfHour ->
            getParserAndOptions 0 59

        HourOfDay ->
            getParserAndOptions 0 23

        DayOfWeek ->
            ( intMinMaxParse 0 6, List.map (\( x, y ) -> ( y, x )) <| Dict.toList <| Dict.map (\_ weekday -> String.fromInt <| TimeUtil.weekdayToInt firstDayOfWeek weekday) stringToWeekday )

        DayOfMonth ->
            getParserAndOptions 1 31

        DayOfYear ->
            getParserAndOptions 1 366

        WeekOfMonth ->
            getParserAndOptions 0 5

        WeekOfYear ->
            getParserAndOptions 0 53

        MonthOfYear ->
            ( intMinMaxParse 1 12, List.map (\( x, y ) -> ( y, x )) <| Dict.toList <| Dict.map (\_ month -> String.fromInt <| TimeUtil.monthToInt month) stringToMonth )

        Year ->
            getParserAndOptions 2000 2200


createSpecificTimeRule : Schedule.Rule -> (Schedule.RuleParts -> Schedule.Rule)
createSpecificTimeRule rule =
    \ruleParts -> rule


createPeriodicRule : Int -> TimeIntervalUnit -> (Schedule.RuleParts -> Schedule.Rule)
createPeriodicRule timeInterval timeIntervalUnit =
    case timeIntervalUnit of
        Minutely ->
            Schedule.Minutely timeInterval

        Hourly ->
            Schedule.Hourly timeInterval

        Daily ->
            Schedule.Daily timeInterval

        Weekly ->
            Schedule.Weekly timeInterval

        Monthly ->
            Schedule.Monthly timeInterval

        Yearly ->
            Schedule.Yearly timeInterval


newTaskForm : Model -> Html Msg
newTaskForm model =
    div [ Attributes.class "new-task-form" ]
        [ FormCustomView.asHtmlCustom
            { onChange = UpdateTaskDetails
            , action = ""
            , loading = ""
            , validation = Form.View.ValidateOnBlur
            }
            (taskDetailsForm model)
            model.taskRaw
        , div [ Attributes.class "rules" ]
            (List.indexedMap
                (\ruleIndex taskRuleRaw ->
                    div [ Attributes.class "rule", Attributes.style "border" "1px solid blue", Attributes.style "margin" "10px", Attributes.style "padding" "10px" ]
                        [ button [ Events.onClick (RemoveRule ruleIndex), Attributes.style "float" "right" ] [ text "-" ]
                        , FormCustomView.asHtmlCustom
                            { onChange = UpdateTaskRule ruleIndex
                            , action = ""
                            , loading = ""
                            , validation = Form.View.ValidateOnBlur
                            }
                            (taskRuleForm model)
                            taskRuleRaw
                        , case
                            Maybe.andThen
                                (\rule ->
                                    Maybe.andThen
                                        (\isPeriodic_ ->
                                            if isPeriodic_ then
                                                Just True

                                            else
                                                Nothing
                                        )
                                        (Form.Value.raw rule.values.isPeriodic)
                                )
                                (List.Extra.getAt ruleIndex model.taskRaw.values.rules)
                          of
                            Nothing ->
                                text ""

                            Just _ ->
                                div [ Attributes.class "rule-parts" ]
                                    (List.indexedMap
                                        (\rulePartIndex rulePartRaw ->
                                            div [ Attributes.class "rule-part", Attributes.style "border" "1px solid red", Attributes.style "margin" "10px", Attributes.style "padding" "10px" ]
                                                [ button [ Events.onClick (RemoveRulePart ruleIndex rulePartIndex), Attributes.style "float" "right" ] [ text "-" ]
                                                , FormCustomView.asHtmlCustom
                                                    { onChange = UpdateRulePart ruleIndex rulePartIndex
                                                    , action = ""
                                                    , loading = ""
                                                    , validation = Form.View.ValidateOnBlur
                                                    }
                                                    (rulePartForm model ruleIndex)
                                                    rulePartRaw
                                                ]
                                        )
                                        taskRuleRaw.values.ruleParts
                                        ++ [ button [ Events.onClick (AddRulePart ruleIndex), Attributes.style "margin-left" "10px" ] [ text "Add Rule Part" ] ]
                                    )
                        ]
                )
                model.taskRaw.values.rules
                ++ [ button [ Events.onClick AddRule, Attributes.style "margin-left" "10px" ] [ text "Add Rule" ] ]
            )
        , button [ Events.onClick NewAddTask ] [ text "Add Task" ]
        , text <|
            if model.isFormValid then
                ""

            else
                "Form Invalid"
        ]


attributesHelper : String -> String -> Form.Base.TextField.Attributes
attributesHelper label placeholder =
    { label = label
    , placeholder = placeholder
    }


pairList : List a -> List ( a, a )
pairList =
    List.map (\value -> ( value, value ))


taskDetailsForm model =
    let
        title =
            Form.textField
                { parser = Ok
                , value = .title
                , update = \value values -> { values | title = value }
                , attributes = attributesHelper "Title" "Task Title"
                }

        description =
            Form.optional <|
                Form.textField
                    { parser = Ok
                    , value = .description
                    , update = \value values -> { values | description = value }
                    , attributes = attributesHelper "Description" "Task Description"
                    }

        startTime =
            Form.dateField
                { parser = dateStringToTime model.timeZone model.timeNow
                , value = .startTime
                , update = \value values -> { values | startTime = value }
                , attributes = attributesHelper "Start Time" ""
                }

        endTime =
            Form.optional <|
                Form.dateField
                    { parser = dateStringToTime model.timeZone model.timeNow
                    , value = .endTime
                    , update = \value values -> { values | endTime = value }
                    , attributes = attributesHelper "End Time" "Optional"
                    }

        firstDayOfWeek =
            Form.selectField
                { parser = \value -> Dict.get value stringToWeekday |> Result.fromMaybe "Invalid day of week"
                , value = .firstDayOfWeek
                , update = \value values -> { values | firstDayOfWeek = value }
                , attributes =
                    { label = "First Day of Week"
                    , placeholder = ""
                    , options = pairList <| Dict.keys stringToWeekday
                    }
                }
    in
    Form.succeed
        (\title_ description_ startTime_ endTime_ firstDayOfWeek_ ->
            Schedule.Schedule [] startTime_ (Maybe.withDefault (Time.millisToPosix 32503680000) endTime_) model.timeZone firstDayOfWeek_
                |> Task title_ (Maybe.withDefault "" description_)
                |> TaskFormOutput
        )
        |> Form.append title
        |> Form.append description
        |> Form.append startTime
        |> Form.append endTime
        |> Form.append firstDayOfWeek


taskRuleForm model =
    let
        isPeriodic =
            Form.checkboxField
                { parser = Ok
                , value = .isPeriodic
                , update = \value values -> { values | isPeriodic = value }
                , attributes =
                    { label = "Periodic?" }
                }

        specificTime =
            Form.dateField
                { parser = dateStringToTime model.timeZone model.timeNow
                , value = .specificTime
                , update = \value values -> { values | specificTime = value }
                , attributes =
                    attributesHelper "Specific Time" "A time this task should occur"
                }

        timeIntervalUnit =
            Form.selectField
                { parser = \value -> Dict.get value stringToTimeIntervalUnit |> Result.fromMaybe "Invalid time period"
                , value = .timeIntervalUnit
                , update = \value values -> { values | timeIntervalUnit = value }
                , attributes =
                    { label = ""
                    , placeholder = "Time Period"
                    , options = pairList <| Dict.keys stringToTimeIntervalUnit
                    }
                }

        timeInterval =
            Form.numberField
                { parser = round >> Ok
                , value = .timeInterval
                , update = \value values -> { values | timeInterval = value }
                , attributes =
                    { label = "Every"
                    , placeholder = ""
                    , step = 1
                    , min = Just 1
                    , max = Nothing
                    }
                }
    in
    isPeriodic
        |> Form.andThen
            (\isPeriodic_ ->
                if isPeriodic_ then
                    Form.succeed (\interval unit -> RuleFormOutput <| createPeriodicRule interval unit)
                        |> Form.append timeInterval
                        |> Form.append timeIntervalUnit

                else
                    Form.succeed (Schedule.SpecificTime >> createSpecificTimeRule >> RuleFormOutput)
                        |> Form.append specificTime
            )


rulePartForm model ruleIndex =
    let
        timeUnitValue ( parser, options ) =
            Form.selectField
                { parser = parser
                , value = .timeUnitValue
                , update = \value values -> { values | timeUnitValue = value }
                , attributes =
                    { label = "On"
                    , placeholder = ""
                    , options = options
                    }
                }

        timeUnit =
            Form.selectField
                { parser = \value -> Dict.get value stringToTimeUnit |> Result.fromMaybe "Invalid time unit"
                , value = .timeUnit
                , update = \value values -> { values | timeUnit = value }
                , attributes =
                    { label = ""
                    , placeholder = "Time Unit"
                    , options = pairList <| Dict.keys stringToTimeUnit
                    }
                }
    in
    timeUnit
        |> Form.andThen
            (\timeUnit_ ->
                Form.succeed (RulePartFormOutput timeUnit_)
                    |> Form.append (timeUnitValue <| timeUnitToParser Time.Mon timeUnit_)
            )
