module Player exposing (Model, init, update, view, Msg(..))

import Svg exposing (Svg, svg, rect)
import Svg.Attributes exposing (x, y, width, height)
import Entity exposing (Entity)
import Bullet


-- Model


type alias Model =
    Entity {}


init : Entity a -> Model
init board =
    { size = { x = 15, y = 15 }
    , center = { x = board.center.x, y = board.size.y - 15 }
    }



-- Update


type Msg
    = Left
    | Right
    | Shoot (Entity {})


update : Msg -> Model -> ( Model, List Bullet.Model )
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
                ( { model | center = center' }, [] )

        Right ->
            let
                center' =
                    if center.x > 310 then
                        center
                    else
                        { center | x = center.x + 2 }
            in
                ( { model | center = center' }, [] )

        Shoot board ->
            ( model, [ Bullet.init board { center | y = center.y - 15 } { x = 0, y = -6 } ] )



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
