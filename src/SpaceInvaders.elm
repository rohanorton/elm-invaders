module SpaceInvaders exposing (init, update, view, subscriptions)

import Html exposing (Html, text, div)
import Keyboard.Extra as Kb
import AnimationFrame
import Player
import Board


-- Model


type alias Model =
    { player : Player.Model
    , board : Board.Model
    , keyboard : Kb.Model
    }


board : Board.Model
board =
    Board.init 310


init : ( Model, Cmd Msg )
init =
    let
        ( kbmodel, kbcmd ) =
            Kb.init
    in
        { player = Player.init board
        , board = board
        , keyboard = kbmodel
        }
            ! [ Cmd.map Key kbcmd ]



-- Update


type Msg
    = Tick
    | Key Kb.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ player, keyboard } as model) =
    case msg of
        Tick ->
            let
                player' =
                    if Kb.isPressed Kb.ArrowLeft keyboard then
                        Player.update Player.Left player
                    else if Kb.isPressed Kb.ArrowRight keyboard then
                        Player.update Player.Right player
                    else
                        player
            in
                { model | player = player' } ! []

        Key keyboardMsg ->
            let
                ( keyboardModel, keyboardCmd ) =
                    Kb.update keyboardMsg model.keyboard
            in
                { model | keyboard = keyboardModel } ! [ Cmd.map Key keyboardCmd ]



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs (always Tick)
        , Sub.map Key Kb.subscriptions
        ]



-- View


view : Model -> Html Msg
view model =
    div []
        [ Board.view model.board [ Player.view model.player ]
        ]
