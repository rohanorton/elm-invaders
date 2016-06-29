module Invader exposing (Model, init, update, view, Msg(..))

import Svg exposing (Svg, svg, rect)
import Svg.Attributes exposing (x, y, width, height)
import Random
import Entity exposing (Vector, Entity)
import Bullet


-- Model


type alias Model =
    Entity
        { patrolX : Float
        , speedX : Float
        }


init : Entity a -> Vector -> Model
init board center =
    { size = { x = 15, y = 15 }
    , center = center
    , patrolX = 0
    , speedX = 0.3
    }



-- Update


type Msg
    = Tick
    | Shoot Int


board : Entity {}
board =
    { size = { x = 310, y = 310 }
    , center = { x = 310 / 2, y = 310 / 2 }
    }


update : Msg -> Model -> ( Model, Cmd Msg, List Bullet.Model )
update msg ({ center, size, patrolX, speedX } as model) =
    case msg of
        Tick ->
            let
                speedX' =
                    if patrolX < 0 || patrolX > 40 then
                        negate speedX
                    else
                        speedX

                center' =
                    { center | x = center.x + speedX' }

                patrolX' =
                    patrolX + speedX'
            in
                ( { model
                    | center = center'
                    , patrolX = patrolX'
                    , speedX = speedX'
                  }
                , Random.generate Shoot (Random.int 1 1000)
                , []
                )

        Shoot chance ->
            let
                bullets =
                    if chance > 995 then
                        [ Bullet.init board { center | y = center.y + 15 } { x = 0, y = 6 } ]
                    else
                        []
            in
                ( model, Cmd.none, bullets )



-- View


view : Model -> Svg a
view { size, center } =
    rect
        [ width <| toString size.x
        , height <| toString size.y
        , x <| toString <| center.x - size.x / 2
        , y <| toString <| center.y - size.y / 2
        ]
        []
