module Object exposing (Object, step, stepV)

{-| Represents a physics object in the game.
-}


{-| Contains values for current x and y positions, as well as
vectors for the x and y axes
-}
type alias Object a =
    { a
        | x : Float
        , y : Float
        , vx : Float
        , vy : Float
    }


{-| Step the vectors of an object given specific collision criteria.

This allows us to detect collisions and have the object reverse itself

-}
stepV : Float -> Bool -> Bool -> Float
stepV v lowerCollision upperCollision =
    if lowerCollision then
        abs v

    else if upperCollision then
        -(abs v)

    else
        v


{-| Step the object's x & y positions based on the x and y deltas along
with the time elapsed
-}
step : Float -> Object a -> Object a
step delta ({ x, y, vx, vy } as obj) =
    { obj
        | x = x + vx * delta
        , y = y + vy * delta
    }
