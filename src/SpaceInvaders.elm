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
    , invaders : List IndexedInvader
    , bullets : List Bullet.Model
    , board : Board.Model
    , keyboard : Kb.Model
    }


type alias IndexedInvader =
    { id : Int
    , model : Invader.Model
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


invadersInit : List IndexedInvader
invadersInit =
    List.map createInvader [0..24]


createInvader : Int -> IndexedInvader
createInvader int =
    { id = int
    , model =
        Invader.init board
            { x = toFloat <| 30 + int % 8 * 30
            , y = toFloat <| 30 + int % 3 * 30
            }
    }



-- Update


type Msg
    = Tick
    | InvaderMsg Int Invader.Msg
    | Key Kb.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ player, invaders, keyboard, bullets } as model) =
    case msg of
        Tick ->
            let
                ( player', playerBullets ) =
                    if Kb.isPressed Kb.ArrowLeft keyboard then
                        Player.update Player.Left player
                    else if Kb.isPressed Kb.ArrowRight keyboard then
                        Player.update Player.Right player
                    else if Kb.isPressed Kb.Space keyboard then
                        Player.update (Player.Shoot board) player
                    else
                        ( player, [] )

                bullets' =
                    List.concat invaderBullets ++ playerBullets ++ List.map (Bullet.update Bullet.Tick) bullets

                ( invaders', invaderCmds, invaderBullets ) =
                    List.map (updateInvaders Invader.Tick) invaders
                        |> unzip3

                invaders'' =
                    List.filter (isNotHit bullets') invaders'
            in
                { model
                    | player = player'
                    , invaders = invaders''
                    , bullets = bullets'
                }
                    ! invaderCmds

        InvaderMsg id subMsg ->
            let
                ( invaders', cmds, invaderBullets ) =
                    List.map (updateInvader id subMsg) invaders
                        |> unzip3

                bullets' =
                    List.concat invaderBullets ++ bullets
            in
                { model
                    | invaders = invaders
                    , bullets = bullets'
                }
                    ! cmds

        Key keyboardMsg ->
            let
                ( keyboardModel, keyboardCmd ) =
                    Kb.update keyboardMsg model.keyboard
            in
                { model | keyboard = keyboardModel } ! [ Cmd.map Key keyboardCmd ]


unzip3 : List ( a, b, c ) -> ( List a, List b, List c )
unzip3 =
    List.foldl (\( x, y, z ) ( xs, ys, zs ) -> ( x :: xs, y :: ys, z :: zs )) ( [], [], [] )


updateInvader : Int -> Invader.Msg -> IndexedInvader -> ( IndexedInvader, Cmd Msg, List Bullet.Model )
updateInvader targetId msg { id, model } =
    let
        ( invader, cmds, bullets ) =
            if targetId == id then
                Invader.update msg model
            else
                ( model, Cmd.none, [] )
    in
        ( IndexedInvader id invader, Cmd.map (InvaderMsg id) cmds, bullets )


updateInvaders : Invader.Msg -> IndexedInvader -> ( IndexedInvader, Cmd Msg, List Bullet.Model )
updateInvaders msg ({ id, model } as invader) =
    updateInvader id msg invader


isNotHit : List Bullet.Model -> IndexedInvader -> Bool
isNotHit bullets { model } =
    List.filter (isColliding model) bullets
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
            :: List.map (Invader.view << .model) model.invaders
            ++ List.map Bullet.view model.bullets
        ]
