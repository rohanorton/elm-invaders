module Player exposing (Model, init, update, view, Msg(..))

import Svg exposing (Svg, svg, rect)
import Svg.Attributes exposing (x, y, width, height)
import Entity


-- Model


type alias Model =
    Entity.Model


init : Entity.Model -> Model
init board =
    { size = { x = 15, y = 15 }
    , center = { x = board.center.x, y = board.size.y - 15 }
    }



-- Update


type Msg
    = Left
    | Right


update : Msg -> Model -> Model
update msg ({ center, size } as model) =
    case msg of
        Left ->
            let
                center' =
                    if center.x < 0 then
                        center
                    else
                        { center | x = center.x - 2 }
            in
                { model | center = center' }

        Right ->
            let
                center' =
                    if center.x > 310 then
                        center
                    else
                        { center | x = center.x + 2 }
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
