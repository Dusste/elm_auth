module Api.Login exposing (submitLogin)

import Data.Credentials as Credentials
import Data.User as User
import Http


submitLogin : User.ValidCredentials -> (Result Http.Error Credentials.Token -> msg) -> Cmd msg
submitLogin credentials toMsg =
    Http.post
        { url = "/api/login"
        , body = Http.jsonBody (User.credentialsEncoder credentials)
        , expect = Http.expectJson toMsg Credentials.tokenDecoder
        }
