module Api.Profile exposing (submitProfile)

import Data.Credentials as Credentials
import Data.Profile
import Http


submitProfile :
    Credentials.Token
    -> { profilePic : String, name : String }
    -> (Result Http.Error Credentials.Token -> msg)
    -> Cmd msg
submitProfile token data toMsg =
    Http.request
        { method = "PUT"
        , headers = [ Credentials.addHeader token ]
        , url = "/api/profile"
        , body = Http.jsonBody (Data.Profile.profileSubmitDataEncoder data)
        , expect = Http.expectJson toMsg Credentials.tokenDecoder
        , timeout = Nothing
        , tracker = Nothing
        }
