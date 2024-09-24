module Api.ForgotPassword exposing (submitForgotPassword)

import Http
import Json.Encode
import Process
import Task


submitForgotPassword : String -> (Result Http.Error () -> msg) -> Cmd msg
submitForgotPassword email toMsg =
    Process.sleep 2000
        |> Task.map (\_ -> ())
        |> Task.perform (Ok >> toMsg)



{-
   Http.post
       { url = "/api/forgot-password"
       , body =
           Http.jsonBody <|
               Json.Encode.string email
       , expect = Http.expectWhatever toMsg
       }
-}
