module Data.Verification exposing (encodeRequestToResendEmail)

import Data.Credentials
import Json.Encode


encodeRequestToResendEmail : Data.Credentials.Token -> String -> Json.Encode.Value
encodeRequestToResendEmail token email =
    Json.Encode.object
        [ ( "token", Data.Credentials.encodeToken token )
        , ( "email", Json.Encode.string email )
        ]
