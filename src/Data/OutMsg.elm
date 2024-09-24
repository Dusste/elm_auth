module Data.OutMsg exposing (..)

import Data.Credentials as Credentials
import Task


type OutMsg
    = RedirectToProfile Credentials.Token
    | ResendEmail String Credentials.Token


msgToCmd : List msg -> Cmd msg
msgToCmd msgs =
    msgs
        |> List.map
            (\m ->
                Task.perform (always m) (Task.succeed ())
            )
        |> Cmd.batch
