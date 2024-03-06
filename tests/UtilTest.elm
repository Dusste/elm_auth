module UtilTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import User exposing (Email, parseEmail)


parseEmailTest : Test
parseEmailTest =
    describe "Parse email"
        [ test "should pass" <|
            \_ ->
                parseEmail "dooshanstevanovic@gmail.com"
                    |> Expect.equal
                        (Ok <| Email "dooshanstevanovic@gmail.com")
        , test "should not pass if all fields invalid" <|
            \_ ->
                parseEmail "?^do?os?ha\nsteva!@#novic@gDma#@@#il.cDDS$@$@om"
                    |> Expect.equal (Err "Invalid email")
        , test "should not pass if there is no domain" <|
            \_ ->
                parseEmail "dassda@dsdas"
                    |> Expect.equal (Err "Invalid email")
        , test "should not pass pass if there is no server name" <|
            \_ ->
                parseEmail "dsdas.com"
                    |> Expect.equal (Err "Invalid email")
        ]
