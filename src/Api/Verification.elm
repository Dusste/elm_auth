module Api.Verification exposing (apiCallToVerify, requestResendEmail)

import Data.Credentials as Credentials
import Data.Verification
import Http
import Json.Decode
import Process
import Task


apiCallToVerify :
    Credentials.Token
    -> (Result Http.Error Credentials.Token -> msg)
    -> Cmd msg
apiCallToVerify token toMsg =
    Process.sleep 2000
        |> Task.map (\_ -> Credentials.Token "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwiaWQiOiIxMjMiLCJpc3ZlcmlmaWVkIjp0cnVlLCJlbWFpbCI6ImRvb3NoYW5zdGV2YW5vdmljQGdtYWlsLmNvbSIsImlhdCI6MTUxNjIzOTAyMiwiZmlyc3RuYW1lIjoiRHVzYW4iLCJ2ZXJpZmljYXRpb25zdHJpbmciOiIxMjM0IiwicHJvZmlsZXBpY3VybCI6Imh0dHBzOi8vZmlyZWJhc2VzdG9yYWdlLmdvb2dsZWFwaXMuY29tL3YwL2IvZWxtYXBwc3RvcmFnZS5hcHBzcG90LmNvbS9vL2ltYWdlcyUyRnByb2ZpbGUtcGljLThiNzUyMzFlLWIxOGQtNDY5Zi04NjNiLWM5NzdhOTM5OGIwMS5qcGVnP2FsdD1tZWRpYSZ0b2tlbj1hZDI5MDUyZi0xZTY0LTQwNzktOWJiNi1iNzhiOTdlYmJlMjEifQ.YtmvWx4_Vr9Kd18O2F8N2-P9VxFyb3p_6QAPw_AX0og")
        |> Task.perform (Ok >> toMsg)



{-
   {
       "sub": "1234567890",
       "id": "123",
       "isverified": true,
       "email": "dooshanstevanovic@gmail.com",
       "iat": 1516239022,
       "firstname": "Dusan",
       "verificationstring": "1234",
       "profilepicurl": "https://firebasestorage.googleapis.com/v0/b/elmappstorage.appspot.com/o/images%2Fprofile-pic-8b75231e-b18d-469f-863b-c977a9398b01.jpeg?alt=media&token=ad29052f-1e64-4079-9bb6-b78b97ebbe21"
     }
     Http.request
         { method = "PUT"
         , headers = [ Credentials.addHeader token ]
         , url = "/api/verify"
         , expect = Http.expectJson toMsg Credentials.tokenDecoder
         , body = Http.emptyBody
         , timeout = Nothing
         , tracker = Nothing
         }
-}


requestResendEmail :
    String
    -> Credentials.Token
    -> (Result Http.Error () -> msg)
    -> Cmd msg
requestResendEmail email token toMsg =
    Process.sleep 2000
        |> Task.map (\_ -> ())
        |> Task.perform (Ok >> toMsg)



{-
   Http.request
       { method = "PUT"
       , headers = [ Credentials.addHeader token ]
       , url = "/api/resend-email"
       , expect = Http.expectJson toMsg (Json.Decode.succeed ())
       , body = Http.jsonBody (Data.Verification.encodeRequestToResendEmail token email)
       , timeout = Nothing
       , tracker = Nothing
       }
-}
