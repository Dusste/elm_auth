module Data.Util exposing (loadingElement, validEmail)

import Html exposing (Html)
import Html.Attributes as HA
import Regex exposing (Regex)



-- todo: try to find something better


validEmail : Regex
validEmail =
    "^[a-zA-Z0-9.!#$%&'*+\\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"
        |> Regex.fromStringWith { caseInsensitive = True, multiline = False }
        |> Maybe.withDefault Regex.never


loadingElement : Html msg
loadingElement =
    Html.div
        [--  HA.class [ Tw.relative, Tw.h_5, Tw.w_5, Tw.flex ]
        ]
        [ Html.span
            [-- HA.class [ Tw.animate_ping, Tw.absolute, Tw.inline_flex, Tw.h_full, Tw.w_full, Tw.rounded_full, Tw.bg_color Tw.sky_400, Tw.opacity_75 ]
            ]
            []
        , Html.span
            [-- HA.class [ Tw.relative, Tw.inline_flex, Tw.rounded_full, Tw.h_5, Tw.w_5, Tw.bg_color Tw.sky_500 ]
            ]
            []
        ]
