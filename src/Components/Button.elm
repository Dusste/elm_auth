{-
   All credit for the ‘phantom builder pattern’ term goes to
   Jeroen Engels, who first shared it on an Incremental Elm livestream.
-}


module Components.Button exposing
    ( buttonLinkStyle
    , toHtml
    , view
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

import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Svg



{-
   * Button Example:
      Components.Button.view
         |> Element.Button.withText "I am button"
         |> Element.Button.withMsg MyMsg `or` Element.withUrl "some/url"
         |> Element.Button.withDisabled False
         |> Element.Button.withPrimaryStyle `or` Element.withSecondaryStyle
         |> Element.Button.toHtml

-}


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


view : Button { needsText : () } msg
view =
    Button
        { label = ""
        , action = Url "/"
        , styles = buttonPrimaryStyle
        , disabled = False
        , icon = Nothing
        }


withText :
    String
    -> Button { constraints | needsText : () } msg
    -> Button { constraints | hasText : (), needsInteractivity : () } msg
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


buttonPrimaryStyle : String
buttonPrimaryStyle =
    "justify-self-start w-fit flex-start bg-[color:--bg-primary-button] border rounded border-[color:--primary-button-b] px-4 py-2 text-[color:--txt-primary-button] transition-all hover:bg-[color:--bg-primary-button-h] hover:border-[color:--bg-primary-button-h]"


buttonSecondaryStyle : String
buttonSecondaryStyle =
    "justify-self-start w-fit flex-start bg-[color:--bg-secondary-button] border border-[color:--secondary-button-b] rounded px-4 py-2 text-[color:--txt-secondary-button] transition-all hover:bg-[color:--bg-secondary-button-h]"


buttonNegativeStyle : String
buttonNegativeStyle =
    "justify-self-start w-fit flex-start bg-white border rounded border-red-300 px-4 py-2 text-red-500 transition-all hover:bg-red-100"


buttonDisabledStyle : String
buttonDisabledStyle =
    "justify-self-start w-fit flex-start bg-gray-200 border rounded border-gray-300 px-4 py-2 text-gray-300 transition-all hover:bg-gray-100"


buttonLinkStyle : String
buttonLinkStyle =
    "justify-self-start w-fit text-gray-950 dark:text-white transition-all hover:text-gray-500 dark:hover:text-gray-300"
