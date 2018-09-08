module Main exposing (Days, Model, Msg(..), Task, formInput, init, main, subscriptions, taskDateFormatter, update, view, viewTask)

import Browser
import DateFormat
import Debug
import Dict
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


type alias Days =
    Int


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
