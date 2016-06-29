module Bullet exposing (Model, init, update, view, Msg(..))

import Svg exposing (Svg, svg, rect)
import Svg.Attributes exposing (x, y, width, height)
import Entity exposing (Vector, Entity)


-- Model


type alias Model =
    Entity
        { velocity : Vector
        }


init : Entity a -> Vector -> Vector -> Model
init board center velocity =
    { size = { x = 3, y = 3 }
    , center = center
    , velocity = velocity
    }



-- Update


type Msg
    = Tick


update : Msg -> Model -> Model
update msg ({ center, size, velocity } as model) =
    case msg of
        Tick ->
            let
                center' =
                    { x = center.x + velocity.x
                    , y = center.y + velocity.y
                    }
            in
                { model | center = center' }



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
