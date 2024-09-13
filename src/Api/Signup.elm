module Api.Signup exposing (submitSignup)

import Data.Credentials as Credentials
import Http
import Json.Encode


submitSignup : { email : String, password : String } -> (Result Http.Error Credentials.Token -> msg) -> Cmd msg
submitSignup { email, password } toMsg =
    Http.post
        { url = "/api/signup"
        , body =
            Http.jsonBody <|
                Json.Encode.object
                    [ ( "email", Json.Encode.string email )
                    , ( "password ", Json.Encode.string password )
                    ]
        , expect = Http.expectJson toMsg Credentials.tokenDecoder
        }
