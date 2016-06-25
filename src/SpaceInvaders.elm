module SpaceInvaders exposing (init, update, view, subscriptions)

import Html exposing (Html, text, div)
import Keyboard.Extra as Kb
import AnimationFrame
import Player
import Invader
import Bullet
import Entity
import Board


-- Model


type alias Model =
    { player : Player.Model
    , invaders : List Invader.Model
    , bullets : List Bullet.Model
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
        , invaders = invadersInit
        , bullets = []
        , board = board
        , keyboard = kbmodel
        }
            ! [ Cmd.map Key kbcmd ]


invadersInit : List Invader.Model
invadersInit =
    List.map createInvader [0..24]


createInvader : Int -> Invader.Model
createInvader int =
    Invader.init board
        { x = toFloat <| 30 + int % 8 * 30
        , y = toFloat <| 30 + int % 3 * 30
        }



-- Update


type Msg
    = Tick
    | Key Kb.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ player, invaders, keyboard, bullets } as model) =
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

                bullets' =
                    if Kb.isPressed Kb.Space keyboard then
                        Bullet.init board player'.center { x = 0, y = -6 }
                            :: List.map (Bullet.update Bullet.Tick) bullets
                    else
                        List.map (Bullet.update Bullet.Tick) bullets

                invaders' =
                    List.map (Invader.update Invader.Tick) invaders
                        |> List.filter (isNotHit bullets')
            in
                { model
                    | player = player'
                    , invaders = invaders'
                    , bullets = bullets'
                }
                    ! []

        Key keyboardMsg ->
            let
                ( keyboardModel, keyboardCmd ) =
                    Kb.update keyboardMsg model.keyboard
            in
                { model | keyboard = keyboardModel } ! [ Cmd.map Key keyboardCmd ]


isNotHit : List Bullet.Model -> Invader.Model -> Bool
isNotHit bullets invader =
    List.filter (isColliding invader) bullets
        |> List.isEmpty


isColliding : { a | center : Entity.Vector, size : Entity.Vector } -> { b | center : Entity.Vector, size : Entity.Vector } -> Bool
isColliding b1 b2 =
    not
        ((b1.center == b2.center && b1.size == b2.size)
            || (b1.center.x + b1.size.x / 2 <= b2.center.x - b2.size.x / 2)
            || (b1.center.y + b1.size.y / 2 <= b2.center.y - b2.size.y / 2)
            || (b1.center.x - b1.size.x / 2 >= b2.center.x + b2.size.x / 2)
            || (b1.center.y - b1.size.y / 2 >= b2.center.y + b2.size.y / 2)
        )



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
        [ Board.view model.board
            <| Player.view model.player
            :: List.map Invader.view model.invaders
            ++ List.map Bullet.view model.bullets
        ]
