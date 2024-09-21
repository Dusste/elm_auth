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
        |> Task.map (\_ -> Credentials.Token "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwiaWQiOiIxMjMiLCJpc3ZlcmlmaWVkIjp0cnVlLCJlbWFpbCI6ImRvb3NoYW5zdGV2YW5vdmljQGdtYWlsLmNvbSIsImlhdCI6MTUxNjIzOTAyMiwiZmlyc3RuYW1lIjoiRHVzYW4iLCJ2ZXJpZmljYXRpb25zdHJpbmciOiJTdHJpbmciLCJwcm9maWxlcGljdXJsIjoiaHR0cHM6Ly93d3cuZ29vZ2xlLmNvbS9waWN0dXJlIn0.rIY06xdknpOj1iGHDib9ZBJqDl2WF8cxvLFvXJUQ-0c")
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
