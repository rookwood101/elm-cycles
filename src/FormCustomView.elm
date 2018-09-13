module FormCustomView exposing (asHtmlCustom)

import Form exposing (Form)
import Form.Base.CheckboxField as CheckboxField
import Form.Base.NumberField as NumberField
import Form.Base.RadioField as RadioField
import Form.Base.RangeField as RangeField
import Form.Base.SelectField as SelectField
import Form.Base.TextField as TextField
import Form.Error as Error exposing (Error)
import Form.Value as Value
import Form.View exposing (..)
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Set exposing (Set)


asHtmlCustom : ViewConfig values msg -> Form values msg -> Model values -> Html msg
asHtmlCustom =
    custom
        { form = form
        , textField = inputField "text"
        , emailField = inputField "email"
        , passwordField = inputField "password"
        , searchField = inputField "search"
        , textareaField = textareaField
        , numberField = numberField
        , rangeField = rangeField
        , checkboxField = checkboxField
        , radioField = radioField
        , selectField = selectField
        , group = group
        }


form : FormConfig msg (Html msg) -> Html msg
form { onSubmit, action, loading, state, fields } =
    let
        onSubmitEvent =
            onSubmit
                |> Maybe.map (Events.onSubmit >> List.singleton)
                |> Maybe.withDefault []
    in
    Html.form (Attributes.class "elm-form" :: onSubmitEvent)
        (List.concat
            [ fields
            , [ case state of
                    Error error ->
                        errorMessage error

                    _ ->
                        Html.text ""
              ]
            ]
        )


inputField : String -> TextFieldConfig msg -> Html msg
inputField type_ { onChange, onBlur, disabled, value, error, showError, attributes } =
    Html.input
        ([ Events.onInput onChange
         , Attributes.disabled disabled
         , Attributes.value value
         , Attributes.placeholder attributes.placeholder
         , Attributes.type_ type_
         ]
            |> withMaybeAttribute Events.onBlur onBlur
        )
        []
        |> withLabelAndError attributes.label showError error


textareaField : TextFieldConfig msg -> Html msg
textareaField { onChange, onBlur, disabled, value, error, showError, attributes } =
    Html.textarea
        ([ Events.onInput onChange
         , Attributes.disabled disabled
         , Attributes.placeholder attributes.placeholder
         ]
            |> withMaybeAttribute Events.onBlur onBlur
        )
        [ Html.text value ]
        |> withLabelAndError attributes.label showError error


numberField : NumberFieldConfig msg -> Html msg
numberField { onChange, onBlur, disabled, value, error, showError, attributes } =
    Html.input
        ([ Events.onInput (fromString String.toFloat value >> onChange)
         , Attributes.disabled disabled
         , Attributes.value (value |> Maybe.map String.fromFloat |> Maybe.withDefault "")
         , Attributes.placeholder attributes.placeholder
         , Attributes.type_ "number"
         , Attributes.step (String.fromFloat attributes.step)
         ]
            |> withMaybeAttribute (String.fromFloat >> Attributes.max) attributes.max
            |> withMaybeAttribute (String.fromFloat >> Attributes.min) attributes.min
            |> withMaybeAttribute Events.onBlur onBlur
        )
        []
        |> withLabelAndError attributes.label showError error


rangeField : RangeFieldConfig msg -> Html msg
rangeField { onChange, onBlur, disabled, value, error, showError, attributes } =
    Html.div
        [ Attributes.class "elm-form-range-field" ]
        [ Html.input
            ([ Events.onInput (fromString String.toFloat value >> onChange)
             , Attributes.disabled disabled
             , Attributes.value (value |> Maybe.map String.fromFloat |> Maybe.withDefault "")
             , Attributes.type_ "range"
             , Attributes.step (String.fromFloat attributes.step)
             ]
                |> withMaybeAttribute (String.fromFloat >> Attributes.max) attributes.max
                |> withMaybeAttribute (String.fromFloat >> Attributes.min) attributes.min
                |> withMaybeAttribute Events.onBlur onBlur
            )
            []
        , Html.span [] [ Html.text (value |> Maybe.map String.fromFloat |> Maybe.withDefault "") ]
        ]
        |> withLabelAndError attributes.label showError error


checkboxField : CheckboxFieldConfig msg -> Html msg
checkboxField { onChange, onBlur, value, disabled, error, showError, attributes } =
    [ Html.label []
        [ Html.input
            ([ Events.onCheck onChange
             , Attributes.checked value
             , Attributes.disabled disabled
             , Attributes.type_ "checkbox"
             ]
                |> withMaybeAttribute Events.onBlur onBlur
            )
            []
        , Html.text attributes.label
        ]
    , maybeErrorMessage showError error
    ]
        |> wrapInFieldContainer showError error


radioField : RadioFieldConfig msg -> Html msg
radioField { onChange, onBlur, disabled, value, error, showError, attributes } =
    let
        radio ( key, label ) =
            Html.label []
                [ Html.input
                    ([ Attributes.name attributes.label
                     , Attributes.value key
                     , Attributes.checked (value == key)
                     , Attributes.disabled disabled
                     , Attributes.type_ "radio"
                     , Events.onClick (onChange key)
                     ]
                        |> withMaybeAttribute Events.onBlur onBlur
                    )
                    []
                , Html.text label
                ]
    in
    Html.fieldset [] (List.map radio attributes.options)
        |> withLabelAndError attributes.label showError error


selectField : SelectFieldConfig msg -> Html msg
selectField { onChange, onBlur, disabled, value, error, showError, attributes } =
    let
        toOption ( key, label_ ) =
            Html.option
                [ Attributes.value key
                , Attributes.selected (value == key)
                ]
                [ Html.text label_ ]

        placeholderOption =
            Html.option
                [ Attributes.disabled True
                , Attributes.selected (value == "")
                ]
                [ Html.text ("-- " ++ attributes.placeholder ++ " --") ]
    in
    Html.select
        ([ Events.onInput onChange
         , Attributes.disabled disabled
         ]
            |> withMaybeAttribute Events.onBlur onBlur
        )
        (placeholderOption :: List.map toOption attributes.options)
        |> withLabelAndError attributes.label showError error


group : List (Html msg) -> Html msg
group =
    Html.div [ Attributes.class "elm-form-group" ]


wrapInFieldContainer : Bool -> Maybe Error -> List (Html msg) -> Html msg
wrapInFieldContainer showError error =
    Html.div
        [ Attributes.classList
            [ ( "elm-form-field", True )
            , ( "elm-form-field-error", showError && error /= Nothing )
            ]
        ]


withLabelAndError : String -> Bool -> Maybe Error -> Html msg -> Html msg
withLabelAndError label showError error fieldAsHtml =
    [ fieldLabel label
    , fieldAsHtml
    , maybeErrorMessage showError error
    ]
        |> wrapInFieldContainer showError error


fieldLabel : String -> Html msg
fieldLabel label =
    Html.label [] [ Html.text label ]


maybeErrorMessage : Bool -> Maybe Error -> Html msg
maybeErrorMessage showError maybeError =
    if showError then
        maybeError
            |> Maybe.map errorToString
            |> Maybe.map errorMessage
            |> Maybe.withDefault (Html.text "")

    else
        Html.text ""


errorMessage : String -> Html msg
errorMessage =
    Html.text >> List.singleton >> Html.div [ Attributes.class "elm-form-error" ]


errorToString : Error -> String
errorToString error =
    case error of
        Error.RequiredFieldIsEmpty ->
            "This field is required"

        Error.ValidationFailed validationError ->
            validationError


withMaybeAttribute : (a -> Html.Attribute msg) -> Maybe a -> List (Html.Attribute msg) -> List (Html.Attribute msg)
withMaybeAttribute toAttribute maybeValue attrs =
    Maybe.map (toAttribute >> (\attr -> attr :: attrs)) maybeValue
        |> Maybe.withDefault attrs


fromString : (String -> Maybe a) -> Maybe a -> String -> Maybe a
fromString parse currentValue input =
    if String.isEmpty input then
        Nothing

    else
        parse input
            |> Maybe.map Just
            |> Maybe.withDefault currentValue
