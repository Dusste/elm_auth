module Components.Element exposing
    ( InputFieldType(..)
    , Notification(..)
    , button
    , inputField
    , notification
    , toHtml
    , withDisabled
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
        }


inputField :
    { label : Maybe String
    , value : String
    , toMsg : String -> msg
    , type_ : InputFieldType
    , isDisabled : Bool
    , error : List String
    }
    -> Html msg
inputField { label, value, toMsg, type_, isDisabled, error } =
    let
        extendedInput : List (Html msg)
        extendedInput =
            if List.isEmpty error then
                [ inputHtml [ HA.class inputTextStyle ] ]

            else
                [ [ inputHtml [ HA.class inputTextNegativeStyle ] ]
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
        [ Components.SvgIcon.iconWarning
        , Html.span [] [ Html.text txt ]
        ]


notification : Notification -> Html msg
notification notification_ =
    -- TODO add schema for different types of icons, maybe
    let
        { iconStyle, wrapperStyle, txtValue } =
            case notification_ of
                Info infoTxt ->
                    { iconStyle = "font-bold px-2 text-center border-2 border-sky-500 rounded-full mr-2"
                    , wrapperStyle = "bg-sky-200 text-sky-500 flex border border-sky-500 rounded items-center p-2"
                    , txtValue = infoTxt
                    }

                Warning warnTxt ->
                    { iconStyle = "font-bold px-2 text-center border-2 border-yellow-500 rounded-full mr-2"
                    , wrapperStyle = "bg-yellow-200 text-yellow-500 flex border border-yellow-500 rounded items-center p-2"
                    , txtValue = warnTxt
                    }

                Error errDescription ->
                    { iconStyle = "font-bold px-2 text-center border-2 border-red-500 rounded-full mr-2"
                    , wrapperStyle = "bg-red-200 text-red-500 flex border border-red-500 rounded items-center p-2"
                    , txtValue = errDescription
                    }

                Success succTxt ->
                    { iconStyle = "font-bold px-2 text-center border-2 border-green-500 rounded-full mr-2"
                    , wrapperStyle = "bg-green-200 text-green-500 flex border border-green-500 rounded items-center p-2"
                    , txtValue = succTxt
                    }
    in
    Html.div
        [ HA.class wrapperStyle ]
        [ Html.span [ HA.class iconStyle ] [ Html.text "!" ]
        , Html.p [ HA.class "" ] [ Html.text txtValue ]
        ]


inputTextStyle =
    "mt-1 py-1 px-2 border-gray-300 block w-full text-sm focus:border-sky-200 focus:ring-sky-200"


inputTextNegativeStyle =
    "mt-1 py-1 px-2 border-red-500 bg-red-100 block w-full text-sm focus:border-red-300 focus:border-red-300 focus:ring-red-500"



{-
   * Button Example:
      Element.button
         |> Element.withText "I am button"
         |> Element.withMsg MyMsg `or` |> Element.withUrl "some/url"
         |> Element.withDisabled False
         |> Element.withPrimaryStyle `or` |> Element.withSecondaryStyle
         |> Element.toHtml

-}


button : Button { needsText : () } msg
button =
    Button
        { label = ""
        , action = Url "/"
        , styles = buttonPrimaryStyle
        , disabled = False
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


withNegativeStyle :
    Button { constraints | hasInteractivity : (), hasText : (), hasDisableState : (), needsStyle : () } msg
    -> Button { constraints | hasInteractivity : (), hasText : (), hasDisableState : (), hasStyle : (), needsClosure : () } msg
withNegativeStyle (Button constraints) =
    Button { constraints | styles = buttonNegativeStyle }


toHtml :
    Button { constraints | needsClosure : () } msg
    -> Html msg
toHtml (Button constraints) =
    let
        { label, action, styles } =
            constraints
    in
    case action of
        Url url ->
            Html.a
                [ HA.href url, HA.class styles ]
                [ Html.text label ]

        Msg msg ->
            Html.span
                [ HE.onClick msg, HA.class (styles ++ " cursor-pointer") ]
                [ Html.text label ]


buttonPrimaryStyle =
    "justify-self-start flex-start bg-sky-200 border rounded border-sky-300 px-4 py-1 text-sky-500 transition-all hover:bg-sky-300 hover:text-white"


buttonSecondaryStyle =
    "justify-self-start flex-start bg-white border border-sky-300 rounded px-4 py-1 text-sky-500 transition-all hover:bg-sky-100"


buttonNegativeStyle =
    "justify-self-start flex-start bg-white border border-red-300 px-4 py-1 text-red-500 transition-all hover:bg-red-100"


buttonLinkStyle =
    "py-2 cursor-pointer text-sky-500"


buttonLinkDisabledStyle =
    "py-2 cursor-not-allowed text-gray-500"
