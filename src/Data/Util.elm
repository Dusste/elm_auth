module Data.Util exposing (..)

import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
import Http
import Regex exposing (Regex)
import Tailwind.Theme as Tw
import Tailwind.Utilities as Tw


buildErrorMessage : Http.Error -> String
buildErrorMessage httpError =
    case httpError of
        Http.BadUrl message ->
            message

        Http.Timeout ->
            "Server is taking too long to respond. Please try again later."

        Http.NetworkError ->
            "Unable to reach server."

        Http.BadStatus statusCode ->
            "Request failed with status code: " ++ String.fromInt statusCode

        Http.BadBody message ->
            message



-- todo: try to find something better


validEmail : Regex
validEmail =
    "^[a-zA-Z0-9.!#$%&'*+\\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"
        |> Regex.fromStringWith { caseInsensitive = True, multiline = False }
        |> Maybe.withDefault Regex.never


loadingElement : Html msg
loadingElement =
    Html.div [ Attr.css [ Tw.relative, Tw.h_5, Tw.w_5, Tw.flex ] ]
        [ Html.span
            [ Attr.css [ Tw.animate_ping, Tw.absolute, Tw.inline_flex, Tw.h_full, Tw.w_full, Tw.rounded_full, Tw.bg_color Tw.sky_400, Tw.opacity_75 ] ]
            []
        , Html.span [ Attr.css [ Tw.relative, Tw.inline_flex, Tw.rounded_full, Tw.h_5, Tw.w_5, Tw.bg_color Tw.sky_500 ] ] []
        ]
