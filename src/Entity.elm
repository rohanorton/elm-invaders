module Entity exposing (Model, Vector)


type alias Vector =
    { x : Float, y : Float }


type alias Model =
    { size : Vector
    , center : Vector
    }
