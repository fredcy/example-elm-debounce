module Main exposing (..)

import Html exposing (Html)
import Html.Events as Html
import Task
import Time
import Process


type alias Model =
    { input : String
    , debounced : String
    , sleepCount : Int
    }


init : Model
init =
    Model "" "" 0


type Action
    = Update String
    | Timeout Int


timeToSettle =
    250 * Time.millisecond


{-| Have model.debounced track the model.input value with "de-bouncing".  User
input causes the Update action. On each Update we start a new sleep task
and associate it with a unique `Timeout` value, keeping track of the most recent
value in model.sleepCount. When the most recent timer fires we consider the
input settled and act on it.
-}
update : Action -> Model -> ( Model, Cmd Action )
update action model =
    case action |> Debug.log "action" of
        Update val ->
            let
                newCount =
                    model.sleepCount + 1
            in
                ( { model | input = val, sleepCount = newCount }
                , Process.sleep timeToSettle |> Task.perform (always (Timeout newCount))
                )

        Timeout count ->
            -- Only act on the most recent sleep call.
            if count == model.sleepCount then
                ( { model | debounced = model.input, sleepCount = 0 }, Cmd.none )
            else
                ( model, Cmd.none )


view : Model -> Html Action
view model =
    Html.div []
        [ Html.h3 [] [ Html.text "Input here" ]
        , Html.input [ Html.onInput Update ] []
        , Html.h3 [] [ Html.text "Debounced value" ]
        , Html.div [] [ Html.text model.debounced ]
        ]


main =
    Html.program
        { init = ( init, Cmd.none )
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
