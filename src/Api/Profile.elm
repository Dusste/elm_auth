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
        |> Task.map (\_ -> Credentials.Token "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwiaWQiOiIxMjMiLCJpc3ZlcmlmaWVkIjpmYWxzZSwiZW1haWwiOiJkb29zaGFuc3RldmFub3ZpY0BnbWFpbC5jb20iLCJpYXQiOjE1MTYyMzkwMjIsImZpcnN0bmFtZSI6IkR1c2FuIiwidmVyaWZpY2F0aW9uc3RyaW5nIjoiMTIzNCIsInByb2ZpbGVwaWN1cmwiOiJodHRwczovL2ZpcmViYXNlc3RvcmFnZS5nb29nbGVhcGlzLmNvbS92MC9iL2VsbWFwcHN0b3JhZ2UuYXBwc3BvdC5jb20vby9pbWFnZXMlMkZwcm9maWxlLXBpYy04Yjc1MjMxZS1iMThkLTQ2OWYtODYzYi1jOTc3YTkzOThiMDEuanBlZz9hbHQ9bWVkaWEmdG9rZW49YWQyOTA1MmYtMWU2NC00MDc5LTliYjYtYjc4Yjk3ZWJiZTIxIn0.e6IdYakMLCLienNxhDHRIOYuCN7-sUuDZ1vYHUVICU4")
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
