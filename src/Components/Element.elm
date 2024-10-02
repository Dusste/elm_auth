module Components.Element exposing
    ( InputFieldType(..)
    , Notification(..)
    , button
    , buttonLinkStyle
    , formLayout
    , hint
    , inputField
    , notification
    , toHtml
    , withDisabled
    , withIcon
    , withLinkStyle
    , withMsg
    , withNegativeStyle
    , withPrimaryStyle
    , withSecondaryStyle
    , withText
    , withUrl
    )

import Components.SvgIcon
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Svg


type InputFieldType
    = Text
    | Decimal
    | Integer
    | Password


type Notification
    = Info String
    | Warning String
    | Error String
    | Success String


type Action msg
    = Url String
    | Msg msg


type Button constraints msg
    = Button
        { label : String
        , action : Action msg
        , styles : String
        , disabled : Bool
        , icon : Maybe (Svg.Svg msg)
        }


type alias InputFieldProps msg =
    { label : Maybe String
    , value : String
    , toMsg : String -> msg
    , type_ : InputFieldType
    , isDisabled : Bool
    , error : List String
    }


formLayout : String -> List (Html msg) -> Html msg
formLayout head elements =
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
                    elements
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

        inputHtml attributes =
            Html.input
                ([ case type_ of
                    Text ->
                        HA.type_ "text"

                    Decimal ->
                        HA.type_ "number"

                    Integer ->
                        HA.type_ "number"

                    Password ->
                        HA.type_ "password"
                 , HA.disabled isDisabled
                 , HA.value value
                 , HE.onInput toMsg
                 ]
                    ++ attributes
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



{-
   * Button Example:
      Element.button
         |> Element.withText "I am button"
         |> Element.withMsg MyMsg `or` Element.withUrl "some/url"
         |> Element.withDisabled False
         |> Element.withPrimaryStyle `or` Element.withSecondaryStyle
         |> Element.toHtml

-}


button : Button { needsText : () } msg
button =
    Button
        { label = ""
        , action = Url "/"
        , styles = buttonPrimaryStyle
        , disabled = False
        , icon = Nothing
        }


withText :
    String
    -> Button { needsText : () } msg
    -> Button { hasText : (), needsInteractivity : () } msg
withText str (Button constraints) =
    Button { constraints | label = str }


withMsg :
    msg
    -> Button { constraints | hasText : (), needsInteractivity : () } msg
    -> Button { constraints | hasText : (), hasInteractivity : (), needsDisableState : () } msg
withMsg message (Button constraints) =
    Button { constraints | action = Msg message }


withUrl :
    String
    -> Button { constraints | hasText : (), needsInteractivity : () } msg
    -> Button { constraints | hasText : (), hasInteractivity : (), needsDisableState : () } msg
withUrl url (Button constraints) =
    Button { constraints | action = Url url }


withDisabled :
    Bool
    -> Button { constraints | hasText : (), hasInteractivity : (), needsDisableState : () } msg
    -> Button { constraints | hasInteractivity : (), hasText : (), hasDisableState : (), needsStyle : () } msg
withDisabled bool (Button constraints) =
    Button { constraints | disabled = bool }


withPrimaryStyle :
    Button { constraints | hasInteractivity : (), hasText : (), hasDisableState : (), needsStyle : () } msg
    -> Button { constraints | hasInteractivity : (), hasText : (), hasDisableState : (), hasStyle : (), needsClosure : () } msg
withPrimaryStyle (Button constraints) =
    Button { constraints | styles = buttonPrimaryStyle }


withSecondaryStyle :
    Button { constraints | hasInteractivity : (), hasText : (), hasDisableState : (), needsStyle : () } msg
    -> Button { constraints | hasInteractivity : (), hasText : (), hasDisableState : (), hasStyle : (), needsClosure : () } msg
withSecondaryStyle (Button constraints) =
    Button { constraints | styles = buttonSecondaryStyle }


withLinkStyle :
    Button { constraints | hasInteractivity : (), hasText : (), hasDisableState : (), needsStyle : () } msg
    -> Button { constraints | hasInteractivity : (), hasText : (), hasDisableState : (), hasStyle : (), needsClosure : () } msg
withLinkStyle (Button constraints) =
    Button { constraints | styles = buttonLinkStyle }


withNegativeStyle :
    Button { constraints | hasInteractivity : (), hasText : (), hasDisableState : (), needsStyle : () } msg
    -> Button { constraints | hasInteractivity : (), hasText : (), hasDisableState : (), hasStyle : (), needsClosure : () } msg
withNegativeStyle (Button constraints) =
    Button { constraints | styles = buttonNegativeStyle }


withIcon :
    Svg.Svg msg
    ->
        Button
            { constraints | hasInteractivity : (), hasText : (), hasDisableState : (), hasStyle : () }
            msg
    -> Button { constraints | hasInteractivity : (), hasText : (), hasDisableState : (), hasStyle : (), needsClosure : () } msg
withIcon icon (Button constraints) =
    Button { constraints | icon = Just icon }


toHtml :
    Button { constraints | needsClosure : () } msg
    -> Html msg
toHtml (Button constraints) =
    let
        { label, action, styles, disabled, icon } =
            constraints
    in
    case action of
        Url url ->
            Html.a
                [ HA.href url, HA.class styles ]
                [ Html.text label ]

        Msg msg ->
            Html.div
                [ HE.onClick msg
                , HA.class <|
                    if disabled then
                        buttonDisabledStyle

                    else
                        styles ++ " cursor-pointer"
                , HA.disabled disabled
                ]
                [ case icon of
                    Just icon_ ->
                        Html.div
                            [ HA.class "flex gap-x-1" ]
                            [ Html.div [ HA.class "w-[20px]" ] [ icon_ ]
                            , Html.text label
                            ]

                    Nothing ->
                        Html.text label
                ]


hint : String -> Html msg
hint txt =
    Html.div
        [ HA.class "p-3 bg-orange-300" ]
        [ Html.text txt ]


buttonPrimaryStyle : String
buttonPrimaryStyle =
    "justify-self-start w-fit flex-start bg-sky-400 border rounded border-sky-400 px-4 py-2 text-white transition-all hover:bg-sky-600 hover:border-sky-600"


buttonSecondaryStyle : String
buttonSecondaryStyle =
    "justify-self-start w-fit flex-start bg-white border border-sky-300 rounded px-4 py-2 text-sky-500 transition-all hover:bg-sky-100"


buttonNegativeStyle : String
buttonNegativeStyle =
    "justify-self-start w-fit flex-start bg-white border rounded border-red-300 px-4 py-2 text-red-500 transition-all hover:bg-red-100"


buttonDisabledStyle : String
buttonDisabledStyle =
    "justify-self-start w-fit flex-start bg-gray-200 border rounded border-gray-300 px-4 py-2 text-gray-300 transition-all hover:bg-gray-100"


buttonLinkStyle : String
buttonLinkStyle =
    "justify-self-start w-fit text-gray-950 transition-all hover:text-gray-500"
