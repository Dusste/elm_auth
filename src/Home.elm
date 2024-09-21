module Home exposing (..)

import Html exposing (Html)
import Html.Attributes as HA


type alias Model =
    {}


initialModel : Model
initialModel =
    {}


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Cmd.none
    )


type Msg
    = NoOp


view : Html Msg
view =
    Html.div
        [ HA.class "flex flex-col items-center justify-center mt-64"
        ]
        [ Html.h2
            [ HA.class "text-[36px]" ]
            [ Html.text "Hello and welcome to our awesome website !" ]
        , Html.p
            []
            [ Html.text "which is still under construction" ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )
