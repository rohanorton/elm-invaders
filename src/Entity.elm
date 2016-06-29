module Entity exposing (Entity, Vector)


type alias Vector =
    { x : Float, y : Float }


type alias Entity a =
    { a
        | size : Vector
        , center : Vector
    }
