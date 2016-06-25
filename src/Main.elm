module Main exposing (..)

import Html.App
import SpaceInvaders exposing (init, update, view, subscriptions)


main : Program Never
main =
    Html.App.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
