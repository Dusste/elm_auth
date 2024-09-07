port module Data.Ports exposing (..)

import Json.Encode exposing (Value)


port storeSession : Maybe String -> Cmd msg


port onSessionChange : (Value -> msg) -> Sub msg


logout : Cmd msg
logout =
    storeSession Nothing
