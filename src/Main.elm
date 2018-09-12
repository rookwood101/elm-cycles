module Main exposing (main)

import Array
import Browser
import DateFormat
import Debug
import Dict
import Focus
import Form
import Form.Base.TextField
import Form.Value
import Form.View
import Html exposing (..)
import Html.Attributes as Attributes
import Html.Events as Events
import Iso8601
import Schedule
import Task
import Time
import TimeConstruct


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
    , ruleParts : Array.Array (Form.View.Model RulePartRawValues)
    }


type alias FormRawValues =
    { title : Form.Value.Value String
    , description : Form.Value.Value String
    , startTime : Form.Value.Value String
    , hasEndTime : Form.Value.Value Bool
    , endTime : Form.Value.Value String
    , firstDayOfWeek : Form.Value.Value String
    , rules : Array.Array (Form.View.Model RuleRawValues)
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
        , hasEndTime = Form.Value.blank
        , endTime = Form.Value.blank
        , firstDayOfWeek = Form.Value.blank
        , rules =
            Array.fromList
                [ Form.View.idle
                    { isPeriodic = Form.Value.filled True
                    , specificTime = Form.Value.blank
                    , timeIntervalUnit = Form.Value.blank
                    , timeInterval = Form.Value.blank
                    , ruleParts =
                        Array.fromList
                            [ Form.View.idle
                                { timeUnit = Form.Value.blank
                                , timeUnitValue = Form.Value.blank
                                }
                            ]
                    }
                ]
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
    , Task.perform UpdateTimeNow (Task.map2 (\zone posix -> ( zone, posix )) Time.here Time.now)
    )



-- UPDATE


type Msg
    = AddTask
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
    | AddRulePart


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
            let
                taskRaw =
                    model.taskRaw

                values =
                    model.taskRaw.values
            in
            ( { model
                | taskRaw =
                    { taskRaw
                        | values =
                            { values
                                | rules =
                                    Array.set ruleIndex newForm values.rules
                            }
                    }
              }
            , Cmd.none
            )

        UpdateRulePart ruleIndex rulePartIndex newForm ->
            let
                taskRaw =
                    model.taskRaw

                values =
                    model.taskRaw.values

                maybeRule =
                    Array.get ruleIndex values.rules
            in
            ( { model
                | taskRaw =
                    { taskRaw
                        | values =
                            { values
                                | rules =
                                    case maybeRule of
                                        Just rule ->
                                            let
                                                ruleValues =
                                                    rule.values

                                                newRule =
                                                    { rule
                                                        | values =
                                                            { ruleValues
                                                                | ruleParts =
                                                                    Array.set rulePartIndex newForm ruleValues.ruleParts
                                                            }
                                                    }
                                            in
                                            Array.set ruleIndex newRule values.rules

                                        Nothing ->
                                            values.rules
                            }
                    }
              }
            , Cmd.none
            )

        NewAddTask ->
            ( model, Cmd.none )

        AddRule ->
            ( model, Cmd.none )

        AddRulePart ->
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


stringToWeekday value =
    case value of
        "Monday" ->
            Ok Time.Mon

        "Tuesday" ->
            Ok Time.Tue

        "Wednesday" ->
            Ok Time.Wed

        "Thursday" ->
            Ok Time.Thu

        "Friday" ->
            Ok Time.Fri

        "Saturday" ->
            Ok Time.Sat

        "Sunday" ->
            Ok Time.Sun

        _ ->
            Err "Invalid day of week"


newTaskForm : Model -> Html Msg
newTaskForm model =
    div [ Attributes.class "new-task-form" ]
        ([ Form.View.asHtml
            { onChange = UpdateTaskDetails
            , action = "Create Task"
            , loading = ""
            , validation = Form.View.ValidateOnBlur
            }
            (taskDetailsForm model)
            model.taskRaw
         ]
            ++ (Array.toList <|
                    Array.indexedMap
                        (\ruleIndex taskRuleRaw ->
                            div [ Attributes.class "rule" ]
                                ([ Form.View.asHtml
                                    { onChange = UpdateTaskRule ruleIndex
                                    , action = "Add Rule"
                                    , loading = ""
                                    , validation = Form.View.ValidateOnBlur
                                    }
                                    (taskRuleForm model)
                                    taskRuleRaw
                                 ]
                                    ++ (Array.toList <|
                                            Array.indexedMap
                                                (\rulePartIndex rulePartRaw ->
                                                    div [ Attributes.class "rule-part" ]
                                                        [ Form.View.asHtml
                                                            { onChange = UpdateRulePart ruleIndex rulePartIndex
                                                            , action = "Add Rule Part"
                                                            , loading = ""
                                                            , validation = Form.View.ValidateOnBlur
                                                            }
                                                            (rulePartForm model)
                                                            rulePartRaw
                                                        ]
                                                )
                                                taskRuleRaw.values.ruleParts
                                       )
                                )
                        )
                        model.taskRaw.values.rules
               )
        )


attributesHelper : String -> String -> Form.Base.TextField.Attributes
attributesHelper label placeholder =
    { label = label
    , placeholder = placeholder
    }


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
            Form.textField
                { parser = Ok
                , value = .description
                , update = \value values -> { values | description = value }
                , attributes = attributesHelper "Description" "Task Description"
                }

        startTime =
            Form.textField
                { parser = dateStringToTime model.timeZone model.timeNow
                , value = .startTime
                , update = \value values -> { values | startTime = value }
                , attributes = attributesHelper "Start Time" ""
                }

        endTime =
            Form.optional <|
                Form.textField
                    { parser = dateStringToTime model.timeZone model.timeNow
                    , value = .endTime
                    , update = \value values -> { values | endTime = value }
                    , attributes = attributesHelper "End Time" "Optional"
                    }

        firstDayOfWeek =
            Form.selectField
                { parser = stringToWeekday
                , value = .firstDayOfWeek
                , update = \value values -> { values | firstDayOfWeek = value }
                , attributes =
                    { label = "First Day of Week"
                    , placeholder = ""
                    , options = [ ( "Sunday", "Sunday" ), ( "Monday", "Monday" ) ]
                    }
                }
    in
    Form.succeed (\_ _ _ _ _ -> NewAddTask)
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
            Form.textField
                { parser = dateStringToTime model.timeZone model.timeNow
                , value = .specificTime
                , update = \value values -> { values | specificTime = value }
                , attributes =
                    attributesHelper "Specific Time" "A time this task should occur"
                }

        timeIntervalUnit =
            Form.selectField
                { parser = Ok
                , value = .timeIntervalUnit
                , update = \value values -> { values | timeIntervalUnit = value }
                , attributes =
                    { label = ""
                    , placeholder = "Time Period"
                    , options =
                        List.map
                            (\value -> ( value, value ))
                            [ "Minute", "Hour", "Day", "Week", "Month", "Year" ]
                    }
                }

        timeInterval =
            Form.numberField
                { parser = Ok
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
                    Form.succeed (\_ _ -> AddRule)
                        |> Form.append timeInterval
                        |> Form.append timeIntervalUnit
                        |> Form.group

                else
                    Form.succeed (\_ -> AddRule)
                        |> Form.append specificTime
            )


rulePartForm model =
    let
        timeUnitsToOptions =
            [ ( "Second of Minute", List.map String.fromInt (List.range 0 59) )
            , ( "Minute of Hour", List.map String.fromInt (List.range 0 59) )
            , ( "Hour of Day", List.map String.fromInt (List.range 0 23) )
            , ( "Day of Week", [ "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday" ] )
            , ( "Day of Month", List.map String.fromInt (List.range 1 31) )
            , ( "Day of Year", List.map String.fromInt (List.range 1 366) )
            , ( "Week of Month", List.map String.fromInt (List.range 0 5) )
            , ( "Week of Year", List.map String.fromInt (List.range 0 53) )
            , ( "Month of Year", List.map String.fromInt (List.range 1 12) )
            , ( "Year", List.map String.fromInt (List.range 1970 2100) )
            ]

        timeUnitsToOptionsDict =
            Dict.fromList timeUnitsToOptions

        timeUnitValue options =
            Form.selectField
                { parser = Ok
                , value = .timeUnitValue
                , update = \value values -> { values | timeUnitValue = value }
                , attributes =
                    { label = "On"
                    , placeholder = ""
                    , options = List.map (\value -> ( value, value )) options
                    }
                }

        timeUnit options =
            Form.selectField
                { parser = Ok
                , value = .timeUnit
                , update = \value values -> { values | timeUnit = value }
                , attributes =
                    { label = ""
                    , placeholder = "Time Unit"
                    , options = List.map (\value -> ( value, value )) options
                    }
                }
    in
    timeUnit (List.map (\( key, value ) -> key) timeUnitsToOptions)
        |> Form.andThen
            (\timeUnit_ ->
                let
                    maybeOptions =
                        Dict.get timeUnit_ timeUnitsToOptionsDict
                in
                case maybeOptions of
                    Just options ->
                        Form.succeed (\_ -> AddRulePart)
                            |> Form.append (timeUnitValue options)

                    Nothing ->
                        Debug.todo "invalid timeunit"
            )
