module Api.ResetPassword exposing (submitResetPassword)

import Data.User as User
import Http
import Json.Encode


submitResetPassword : User.Password -> String -> (Result Http.Error () -> msg) -> Cmd msg
submitResetPassword password resetCodeParam toMsg =
    Http.post
        { url = "/api/reset-password/" ++ resetCodeParam
        , body =
            Http.jsonBody <|
                Json.Encode.object
                    [ ( "password", User.passwordEncoder password ) ]
        , expect = Http.expectWhatever toMsg
        }
