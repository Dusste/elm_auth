module Api.ResetPassword exposing (submitResetPassword)

import Http
import Json.Encode


submitResetPassword : String -> String -> (Result Http.Error () -> msg) -> Cmd msg
submitResetPassword password resetCodeParam toMsg =
    Http.post
        { url = "/api/reset-password/" ++ resetCodeParam
        , body =
            Http.jsonBody <|
                Json.Encode.string password
        , expect = Http.expectWhatever toMsg
        }
