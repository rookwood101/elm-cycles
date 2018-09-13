module Main exposing (main)

import Browser
import DateFormat
import Debug
import Dict
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
    , ruleParts : List (Form.View.Model RulePartRawValues)
    }


type alias FormRawValues =
    { title : Form.Value.Value String
    , description : Form.Value.Value String
    , startTime : Form.Value.Value String
    , hasEndTime : Form.Value.Value Bool
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
        , hasEndTime = Form.Value.blank
        , endTime = Form.Value.blank
        , firstDayOfWeek = Form.Value.blank
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
        False
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
                ( { model | isFormValid = True, tasks = makeTaskNew model :: model.tasks }, Cmd.none )

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
                        , div [ Attributes.class "rule-parts" ]
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
    Form.succeed (\_ _ _ _ _ -> Null)
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
                    Form.succeed (\_ _ -> Null)
                        |> Form.append timeInterval
                        |> Form.append timeIntervalUnit
                        |> Form.group

                else
                    Form.succeed (\_ -> Null)
                        |> Form.append specificTime
            )


rulePartForm model ruleIndex =
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
                        Form.succeed (\_ -> Null)
                            |> Form.append (timeUnitValue options)

                    Nothing ->
                        Debug.todo "invalid timeunit"
            )
