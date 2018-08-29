module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Events as Events
import Html.Attributes as Attributes
import Time
import Iso8601
import Dict
import DateFormat
import Task
import Debug


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
    , regularity : Days
    , startDate : Time.Posix
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


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        [ Task "Laundry" "Don't forget the rags!" 7 (Time.millisToPosix 999999) ]
        "" "" "" "" Time.utc (Time.millisToPosix 0)
    , Task.perform UpdateTimeNow (Task.map2 (\zone posix -> (zone, posix)) Time.here Time.now)
    )



-- UPDATE


type Msg
    = AddTask (Maybe Task)
    | InputName String
    | InputDescription String
    | InputRegularity String
    | InputStartDate String
    | UpdateTimeNow (Time.Zone, Time.Posix)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddTask maybeTask ->
            case maybeTask of
                Just task ->
                    ( { model | tasks = task :: model.tasks }, Cmd.none )
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
        UpdateTimeNow (zone, posix) ->
            ( { model | timeZone = zone, timeNow = posix }, Cmd.none )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ Attributes.class "page" ]
    [ div [ Attributes.class "tasks" ] (List.map (viewTask model.timeZone) model.tasks)
    , div [ Attributes.class "new-task" ]
        [ formInput "name" "text" "Name" [] InputName
        , formInput "description" "text" "Description" [] InputDescription
        , formInput "regularity" "number" "Regularity" [] InputRegularity
        , formInput "startDate" "date" "Start Date" [] InputStartDate
        , div []
            [ button
                [ Events.onClick <| AddTask <|
                    let name = Just model.inputName
                        description = Just model.inputDescription
                        regularity = String.toInt model.inputRegularity
                        startDate = Result.toMaybe <| Iso8601.toTime <| model.inputStartDate ++ "T00:00:00Z"
                    in Maybe.map4 Task name description regularity startDate
                ]
                [ text "Add" ]
            ]
        ]
    ]

formInput : String -> String -> String -> List (Attribute Msg) -> (String -> Msg) -> Html Msg
formInput id type_ display extraAttributes inputMsg =
    div []
        [ label [ Attributes.for id ] [ text display ]
        , input ([ Attributes.type_ type_, Attributes.id id, Attributes.name id, Events.onInput inputMsg ] ++ extraAttributes) []
        ]

viewTask : Time.Zone -> Task -> Html Msg
viewTask timeZone task = 
    div []
        [ h3 [] [ text task.name ]
        , p [] [ text <| taskDateFormatter timeZone task.startDate ]
        , p [] [ text <| "Every " ++ String.fromInt task.regularity ++ " days" ] -- convert period to string
        , p [] [ text task.description ]
        ]

taskDateFormatter =
    DateFormat.format
        [ DateFormat.dayOfWeekNameFirstThree
        , DateFormat.text " "
        , DateFormat.dayOfMonthSuffix
        , DateFormat.text " of "
        , DateFormat.monthNameFull
        , DateFormat.text " "
        , DateFormat.yearNumber
        ]
