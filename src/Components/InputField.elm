module Components.InputField exposing
    ( InputFieldType(..)
    , PlaceholderLabel(..)
    , toHtml
    , view
    , withDisable
    , withError
    , withExtraText
    , withMsg
    , withType
    , withValue
    )

import Components.SvgIcon
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE



{-
   * InputField Example:
      Components.InputField.view
        |> Components.InputField.withValue model.storeEmail
        |> Components.InputField.withMsg StoreEmail
        |> Components.InputField.withType Components.InputField.Text
        |> Components.InputField.withDisable (model.formState == Loading)
        |> Components.InputField.withError (Components.Error.byFieldName "email" model.errors)
        |> Components.InputField.withExtraText (Components.InputField.Label "Email")
        |> Components.InputField.toHtml

-}


type InputFieldType msg
    = Text
    | Decimal
    | Integer
    | Password ( String, Bool, String -> Bool -> msg )


type PlaceholderLabel
    = Label String
    | Placeholder String


type InputField constraints msg
    = InputField
        { label : PlaceholderLabel
        , showPassword : Bool
        , value : String
        , toMsg : Maybe (String -> msg)
        , type_ : InputFieldType msg
        , isDisabled : Bool
        , error : List String
        }


view : InputField { needsValue : () } msg
view =
    InputField
        { label = Label ""
        , showPassword = False
        , value = ""
        , toMsg = Nothing
        , type_ = Text
        , isDisabled = False
        , error = []
        }


withValue :
    String
    -> InputField { constraints | needsValue : () } msg
    -> InputField { constraints | hasValue : (), needsMsg : () } msg
withValue value (InputField constraints) =
    InputField { constraints | value = value }


withMsg :
    (String -> msg)
    -> InputField { constraints | hasValue : (), needsMsg : () } msg
    -> InputField { constraints | hasMsg : (), needsType : () } msg
withMsg toMsg (InputField constraints) =
    InputField { constraints | toMsg = Just toMsg }


withType :
    InputFieldType msg
    -> InputField { constraints | hasMsg : (), needsType : () } msg
    -> InputField { constraints | hasType : (), needsDisableState : () } msg
withType type_ (InputField constraints) =
    InputField { constraints | type_ = type_ }


withDisable :
    Bool
    -> InputField { constraints | hasType : (), needsDisableState : () } msg
    -> InputField { constraints | hasDisableState : (), needsError : () } msg
withDisable isDisabled (InputField constraints) =
    InputField { constraints | isDisabled = isDisabled }


withError :
    List String
    -> InputField { constraints | hasDisableState : (), needsError : () } msg
    -> InputField { constraints | hasError : (), needsLabel : () } msg
withError lstErrors (InputField constraints) =
    InputField { constraints | error = lstErrors }


withExtraText :
    PlaceholderLabel
    -> InputField { constraints | hasError : (), needsLabel : () } msg
    ->
        InputField
            { hasDisableState : ()
            , hasError : ()
            , hasType : ()
            , hasMsg : ()
            , hasValue : ()
            , hasLabel : ()
            }
            msg
withExtraText label (InputField constraints) =
    InputField { constraints | label = label }


toHtml :
    InputField
        { hasDisableState : ()
        , hasError : ()
        , hasType : ()
        , hasMsg : ()
        , hasValue : ()
        , hasLabel : ()
        }
        msg
    -> Html msg
toHtml (InputField constraints) =
    let
        { label, value, toMsg, type_, isDisabled, error } =
            constraints

        extendedInput : List (Html msg)
        extendedInput =
            if isDisabled then
                [ inputHtml
                    [ HA.class inputTextDisabledStyle ]
                ]

            else if List.isEmpty error then
                [ inputHtml
                    [ HA.class inputTextStyle ]
                ]

            else
                [ [ inputHtml
                        [ HA.class inputTextNegativeStyle ]
                  ]
                , error
                    |> List.map
                        (\error_ ->
                            inputFieldError error_
                        )
                ]
                    |> List.concat

        inputHtml : List (Html.Attribute msg) -> Html msg
        inputHtml attributes =
            Html.input
                (List.concat
                    [ [ case type_ of
                            Text ->
                                HA.type_ "text"

                            Decimal ->
                                HA.type_ "number"

                            Integer ->
                                HA.type_ "number"

                            Password ( _, shouldShow, _ ) ->
                                if shouldShow then
                                    HA.type_ "text"

                                else
                                    HA.type_ "password"
                      , HA.disabled isDisabled
                      , HA.value value
                      ]
                    , case toMsg of
                        Just toMsg_ ->
                            [ HE.onInput toMsg_ ]

                        Nothing ->
                            []
                    , case label of
                        Placeholder placeholder ->
                            [ HA.placeholder placeholder ]

                        _ ->
                            []
                    , attributes
                    ]
                )
                []
    in
    case label of
        Label label_ ->
            Html.label
                [ HA.class "block relative" ]
                (List.concat
                    [ [ Html.text label_ ]
                    , extendedInput
                    , case type_ of
                        Password ( id, shouldShow, toMsg_ ) ->
                            [ labelShowPassword toMsg_ id shouldShow ]

                        _ ->
                            []
                    ]
                )

        Placeholder _ ->
            Html.label [] extendedInput


labelShowPassword : (String -> Bool -> msg) -> String -> Bool -> Html msg
labelShowPassword toMsg id shouldShow =
    Html.label
        [ HA.for id ]
        [ Html.input
            [ HA.type_ "checkbox"
            , HA.id id
            , HE.onCheck <| toMsg id
            , HA.class "h-[0] absolute invisible"
            ]
            []
        , Html.span
            [ HA.class <|
                "absolute right-[5px] w-[25px] right-[5px] top-[35px] cursor-pointer z-index"
                    ++ (if shouldShow then
                            ""

                        else
                            eyeClosedStyle
                       )
            ]
            [ Components.SvgIcon.eye ]
        ]


eyeClosedStyle : String
eyeClosedStyle =
    "before:content-[''] before:block before:absolute before:w-[25px] before:h-[1px] before:bg-sky-500 before:rotate-[45deg] before:top-[12px] before:cursor-pointer"


inputFieldError : String -> Html msg
inputFieldError txt =
    Html.div
        [ HA.class "bg-red-500 text-white flex gap-1 p-1 text-xs items-center" ]
        [ Components.SvgIcon.warning
        , Html.span
            []
            [ Html.text txt ]
        ]


inputTextStyle : String
inputTextStyle =
    "mt-1 py-2 px-3 text-sky-600 border-gray-300 block w-full rounded text-sm focus:border-sky-200 focus:ring-sky-200 pr-[32px] placeholder:text-gray-950"


inputTextNegativeStyle : String
inputTextNegativeStyle =
    "mt-1 py-2 px-3 mb-1 border-red-500 rounded bg-red-100 block w-full text-sm text-red-500 focus:border-red-300 focus:border-red-300 focus:ring-red-500"


inputTextDisabledStyle : String
inputTextDisabledStyle =
    "mt-1 py-2 px-3 border-gray-500 rounded bg-slate-100 block w-full text-sm text-gray-500 focus:border-gray-300 focus:border-gray-300 focus:ring-gray-500"
