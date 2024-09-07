module Api.ForgotPassword exposing (submitForgotPassword)

import Data.User as User
import Http
import Json.Encode


submitForgotPassword : User.Email -> (Result Http.Error () -> msg) -> Cmd msg
submitForgotPassword email toMsg =
    Http.post
        { url = "/api/forgot-password"
        , body =
            Http.jsonBody <|
                Json.Encode.object
                    [ ( "email", User.emailEncoder email ) ]
        , expect = Http.expectWhatever toMsg
        }
