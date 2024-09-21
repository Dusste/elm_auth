module Api.Verification exposing (apiCallToVerify)

import Data.Credentials as Credentials
import Http


apiCallToVerify :
    Credentials.Token
    -> (Result Http.Error Credentials.Token -> msg)
    -> Cmd msg
apiCallToVerify token toMsg =
    Http.request
        { method = "PUT"
        , headers = [ Credentials.addHeader token ]
        , url = "/api/verify"
        , expect = Http.expectJson toMsg Credentials.tokenDecoder
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }
