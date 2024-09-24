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
        |> Task.map (\_ -> Credentials.Token "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwiaWQiOiIxMjMiLCJpc3ZlcmlmaWVkIjpmYWxzZSwiZW1haWwiOiJkb29zaGFuc3RldmFub3ZpY0BnbWFpbC5jb20iLCJpYXQiOjE1MTYyMzkwMjIsImZpcnN0bmFtZSI6IkR1c2FuIiwidmVyaWZpY2F0aW9uc3RyaW5nIjoiMTIzNCIsInByb2ZpbGVwaWN1cmwiOiJodHRwczovL2ZpcmViYXNlc3RvcmFnZS5nb29nbGVhcGlzLmNvbS92MC9iL2VsbWFwcHN0b3JhZ2UuYXBwc3BvdC5jb20vby9pbWFnZXMlMkZwcm9maWxlLXBpYy04Yjc1MjMxZS1iMThkLTQ2OWYtODYzYi1jOTc3YTkzOThiMDEuanBlZz9hbHQ9bWVkaWEmdG9rZW49YWQyOTA1MmYtMWU2NC00MDc5LTliYjYtYjc4Yjk3ZWJiZTIxIn0.e6IdYakMLCLienNxhDHRIOYuCN7-sUuDZ1vYHUVICU4")
        |> Task.perform (Ok >> toMsg)



{-
   {
     "sub": "1234567890",
     "id": "123",
     "isverified": false,
     "email": "dooshanstevanovic@gmail.com",
     "iat": 1516239022,
     "firstname": "Dusan",
     "verificationstring": "1234",
     "profilepicurl": "https://firebasestorage.googleapis.com/v0/b/elmappstorage.appspot.com/o/images%2Fprofile-pic-8b75231e-b18d-469f-863b-c977a9398b01.jpeg?alt=media&token=ad29052f-1e64-4079-9bb6-b78b97ebbe21"
   }

   Http.post
    { url = "/api/login"
    , body =
        Http.jsonBody <|
            Json.Encode.object
                [ ( "email", Json.Encode.string email )
                , ( "password", Json.Encode.string password )
                ]
    , expect = Http.expectJson toMsg Credentials.tokenDecoder
    }
-}
