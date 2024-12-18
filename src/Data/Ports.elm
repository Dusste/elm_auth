port module Data.Ports exposing (isDarkModeLS, logout, onSessionChange, reportClick, sendDarkMode, storeSession)

import Json.Encode exposing (Value)


port storeSession : Maybe String -> Cmd msg


port sendDarkMode : Bool -> Cmd msg


port isDarkModeLS : (Value -> msg) -> Sub msg


port onSessionChange : (Value -> msg) -> Sub msg


port reportClick : (Value -> msg) -> Sub msg


logout : Cmd msg
logout =
    storeSession Nothing
