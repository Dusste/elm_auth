module Api.Login exposing (submitLogin)

import Data.Credentials as Credentials
import Http
import Json.Encode
import Process
import Task


submitLogin :
    { email : String, password : String }
    -> (Result Http.Error Credentials.Token -> msg)
    -> Cmd msg
submitLogin { email, password } toMsg =
    Process.sleep 2000
        |> Task.map (\_ -> Credentials.Token "@!#!@#!@#21321321")
        |> Task.perform (Ok >> toMsg)



-- Http.post
--     { url = "/api/login"
--     , body =
--         Http.jsonBody <|
--             Json.Encode.object
--                 [ ( "email", Json.Encode.string email )
--                 , ( "password", Json.Encode.string password )
--                 ]
--     , expect = Http.expectJson toMsg Credentials.tokenDecoder
--     }
