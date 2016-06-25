module Board exposing (..)

import Svg exposing (Svg, svg, rect)
import Svg.Attributes exposing (x, y, width, height)
import Entity


type alias Model =
    Entity.Model


init : Float -> Model
init size =
    { size = { x = size, y = size }
    , center = { x = size / 2, y = size / 2 }
    }


view : Model -> List (Svg a) -> Svg a
view model elems =
    svg
        [ width <| toString model.size.x
        , height <| toString model.size.y
        ]
        elems
