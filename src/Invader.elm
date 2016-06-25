module Invader exposing (Model, init, update, view, Msg(..))

import Svg exposing (Svg, svg, rect)
import Svg.Attributes exposing (x, y, width, height)
import Entity


-- Model


type alias Model =
    { size : Entity.Vector
    , center : Entity.Vector
    , patrolX : Float
    , speedX : Float
    }


init : Entity.Model -> Entity.Vector -> Model
init board center =
    { size = { x = 15, y = 15 }
    , center = center
    , patrolX = 0
    , speedX = 0.3
    }



-- Update


type Msg
    = Tick


update : Msg -> Model -> Model
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
                { model
                    | center = center'
                    , patrolX = patrolX'
                    , speedX = speedX'
                }



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
