module Api.ForgotPassword exposing (submitForgotPassword)

import Http
import Json.Encode


submitForgotPassword : String -> (Result Http.Error () -> msg) -> Cmd msg
submitForgotPassword email toMsg =
    Http.post
        { url = "/api/forgot-password"
        , body =
            Http.jsonBody <|
                Json.Encode.string email
        , expect = Http.expectWhatever toMsg
        }
