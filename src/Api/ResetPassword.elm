module Api.ResetPassword exposing (submitResetPassword)

import Http
import Json.Encode
import Process
import Task


submitResetPassword : String -> String -> (Result Http.Error () -> msg) -> Cmd msg
submitResetPassword password resetCodeParam toMsg =
    Process.sleep 2000
        |> Task.map (\_ -> ())
        |> Task.perform (Ok >> toMsg)



{-
   Http.post
       { url = "/api/reset-password/" ++ resetCodeParam
       , body =
           Http.jsonBody <|
               Json.Encode.string password
       , expect = Http.expectWhatever toMsg
       }
-}
