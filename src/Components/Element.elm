module Components.Element exposing
    ( InputFieldType(..)
    , Notification(..)
    , formLayout
    , notification
    , switch
    )

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



-- TODO elements
-- radio, checkbox, modal, selectbox


formLayout : String -> List (Html msg) -> Html msg
formLayout head children =
    Html.div
        [ HA.class "flex justify-center mt-32" ]
        [ Html.div
            [ HA.class "flex flex-col w-[380px] py-[2px] rounded items-center bg-gradient-to-l from-sky-200" ]
            [ Html.div
                [ HA.class "flex flex-col w-[376px] rounded gap-y-4 bg-white dark:bg-black py-10 px-8" ]
                [ Html.h2
                    [ HA.class "text-gray-950 dark:text-white" ]
                    [ Html.text head ]
                , Html.form
                    [ HA.class "flex flex-col gap-y-4" ]
                    children
                ]
            ]
        ]


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
            [ HA.class "text-inherit" ]
            [ Html.text txtValue ]
        ]


notificationIconStyle : String -> String
notificationIconStyle color =
    "font-bold min-w-[20px] h-5 flex justify-center items-center border-2 rounded-full mr-2 border-" ++ color ++ "-500"


notificationWrapperStyle : String -> String
notificationWrapperStyle color =
    "leading-4 flex border rounded items-center p-2 rounded items-center p-2 border-" ++ color ++ "-500 bg-" ++ color ++ "-200 text-" ++ color ++ "-500"


switch : (Bool -> msg) -> Bool -> Html msg
switch onMsg isDark =
    Html.label
        [ HA.class <|
            "mx-4 px-2 h-[38px] rounded border before:transition-all transition-all before:duration-50 before:absolute items-center flex cursor-pointer relative before:w-[calc(50%-6px)] before:[''] before:h-[28px] before:rounded before:bg-gray-950 "
                ++ (if not isDark then
                        "before:left-[5px] text-gray-950 bg-gray-100"

                    else
                        "bg-gray-950 text-white before:bg-white before:left-[calc(50%+2px)]"
                   )
        ]
        [ Html.text "Light Dark"
        , Html.input
            [ HA.class "h-[0] absolute invisible"
            , HA.type_ "checkbox"
            , HE.onCheck onMsg
            ]
            []
        ]
