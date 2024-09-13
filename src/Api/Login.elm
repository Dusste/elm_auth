module Api.Login exposing (submitLogin)

import Data.Credentials as Credentials
import Http
import Json.Encode


submitLogin :
    { email : String, password : String }
    -> (Result Http.Error Credentials.Token -> msg)
    -> Cmd msg
submitLogin { email, password } toMsg =
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
