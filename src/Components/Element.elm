module Components.Element exposing
    ( InputFieldType(..)
    , Notification(..)
    , formLayout
      -- , hint
    , inputField
    , notification
    )

import Components.SvgIcon
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE


type InputFieldType msg
    = Text
    | Decimal
    | Integer
    | Password


type Notification
    = Info String
    | Warning String
    | Error String
    | Success String


type alias InputFieldProps msg =
    { label : Maybe String
    , value : String
    , toMsg : String -> msg
    , type_ : InputFieldType msg
    , isDisabled : Bool
    , error : List String
    }


formLayout : String -> List (Html msg) -> Html msg
formLayout head children =
    Html.div
        [ HA.class "flex justify-center mt-32" ]
        [ Html.div
            [ HA.class "flex flex-col w-[380px] py-[2px] rounded items-center bg-gradient-to-l from-sky-200" ]
            [ Html.div
                [ HA.class "flex flex-col w-[376px] rounded gap-y-4 bg-white py-10 px-8" ]
                [ Html.h2
                    []
                    [ Html.text head ]
                , Html.form
                    [ HA.class "flex flex-col gap-y-4" ]
                    children
                ]
            ]
        ]


inputField : InputFieldProps msg -> Html msg
inputField { label, value, toMsg, type_, isDisabled, error } =
    let
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
                    [ case type_ of
                        Text ->
                            [ HA.type_ "text" ]

                        Decimal ->
                            [ HA.type_ "number" ]

                        Integer ->
                            [ HA.type_ "number" ]

                        Password ->
                            [ HA.type_ "password" ]
                    , [ HA.disabled isDisabled
                      , HA.value value
                      , HE.onInput toMsg
                      ]
                        ++ attributes
                    ]
                )
                []
    in
    case label of
        Just label_ ->
            Html.label [ HA.class "block" ]
                (List.concat
                    [ [ Html.text label_ ]
                    , extendedInput
                    ]
                )

        Nothing ->
            Html.label [] extendedInput


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
    "mt-1 py-2 px-3 text-sky-600 border-gray-300 block w-full rounded text-sm focus:border-sky-200 focus:ring-sky-200"


inputTextNegativeStyle : String
inputTextNegativeStyle =
    "mt-1 py-2 px-3 mb-1 border-red-500 rounded bg-red-100 block w-full text-sm text-red-500 focus:border-red-300 focus:border-red-300 focus:ring-red-500"


inputTextDisabledStyle : String
inputTextDisabledStyle =
    "mt-1 py-2 px-3 border-gray-500 rounded bg-slate-100 block w-full text-sm text-gray-500 focus:border-gray-300 focus:border-gray-300 focus:ring-gray-500"


notification : Notification -> Html msg
notification notification_ =
    -- TODO add schema for different types of icons, maybe
    let
        { color, txtValue } =
            case notification_ of
                Info infoTxt ->
                    { color = "sky"
                    , txtValue = infoTxt
                    }

                Warning warnTxt ->
                    { color = "yellow"
                    , txtValue = warnTxt
                    }

                Error errDescription ->
                    { color = "red"
                    , txtValue = errDescription
                    }

                Success succTxt ->
                    { color = "green"
                    , txtValue = succTxt
                    }
    in
    Html.div
        [ HA.class <| notificationWrapperStyle color ]
        [ Html.span
            [ HA.class <| notificationIconStyle color ]
            [ Html.text "!" ]
        , Html.p
            []
            [ Html.text txtValue ]
        ]


notificationIconStyle : String -> String
notificationIconStyle color =
    "font-bold min-w-[20px] h-5 flex justify-center items-center border-2 rounded-full mr-2 border-" ++ color ++ "-500"


notificationWrapperStyle : String -> String
notificationWrapperStyle color =
    "leading-4 flex border rounded items-center p-2 rounded items-center p-2 border-" ++ color ++ "-500 bg-" ++ color ++ "-200 text-" ++ color ++ "-500"



-- hint : String -> Html msg
-- hint txt =
--     Html.div
--         [ HA.class "p-3 bg-orange-300" ]
--         [ Html.text txt ]
