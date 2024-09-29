module Api.Profile exposing (submitProfile)

import Data.Credentials as Credentials
import Data.Profile
import Http
import Process
import Task


submitProfile :
    Credentials.Token
    -> { profilePic : String, name : String }
    -> (Result Http.Error Credentials.Token -> msg)
    -> Cmd msg
submitProfile token data toMsg =
    Process.sleep 2000
        |> Task.map (\_ -> Credentials.Token "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwiaWQiOiIxMjMiLCJpc3ZlcmlmaWVkIjp0cnVlLCJlbWFpbCI6ImRvb3NoYW5zdGV2YW5vdmljQGdtYWlsLmNvbSIsImlhdCI6MTUxNjIzOTAyMiwiZmlyc3RuYW1lIjoiRHVzYW4iLCJ2ZXJpZmljYXRpb25zdHJpbmciOiIxMjM0IiwicHJvZmlsZXBpY3VybCI6Imh0dHBzOi8vZmlyZWJhc2VzdG9yYWdlLmdvb2dsZWFwaXMuY29tL3YwL2IvZWxtYXBwc3RvcmFnZS5hcHBzcG90LmNvbS9vL2ltYWdlcyUyRnByb2ZpbGUtcGljLThiNzUyMzFlLWIxOGQtNDY5Zi04NjNiLWM5NzdhOTM5OGIwMS5qcGVnP2FsdD1tZWRpYSZ0b2tlbj1hZDI5MDUyZi0xZTY0LTQwNzktOWJiNi1iNzhiOTdlYmJlMjEifQ.YtmvWx4_Vr9Kd18O2F8N2-P9VxFyb3p_6QAPw_AX0og")
        |> Task.perform (Ok >> toMsg)



{-
   Http.request
       { method = "PUT"
       , headers = [ Credentials.addHeader token ]
       , url = "/api/profile"
       , body = Http.jsonBody (Data.Profile.profileSubmitDataEncoder data)
       , expect = Http.expectJson toMsg Credentials.tokenDecoder
       , timeout = Nothing
       , tracker = Nothing
       }
-}
