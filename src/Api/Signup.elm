module Api.Signup exposing (submitSignup)

import Data.Credentials as Credentials
import Data.User as User
import Http


submitSignup : { email : String, password : String } -> (Result Http.Error Credentials.Token -> msg) -> Cmd msg
submitSignup credentials toMsg =
    Http.post
        { url = "/api/signup"
        , body = Http.jsonBody (User.credentialsEncoder credentials)
        , expect = Http.expectJson toMsg Credentials.tokenDecoder
        }
