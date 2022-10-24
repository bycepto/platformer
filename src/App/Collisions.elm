module App.Collisions exposing
    ( BoundingBox
    , CollisionSides
    , collision
    )


type alias BoundingBox =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


type alias CollisionSides =
    { top : Bool
    , bottom : Bool
    , left : Bool
    , right : Bool
    }


{-| Check for collisions between two bounding boxes. If there is a collision,
return which sides the first bounding box hit on the second bound box.

For example:

##############
#.....##.....#
#.BB1.##.BB2.#
#.....##.....#
##############

BB1 hits the LEFT side of BB2, so the result will be Just {left=true, ...}

-}
collision : BoundingBox -> BoundingBox -> Maybe CollisionSides
collision bb1 bb2 =
    if
        (bb1.x < bb2.x + bb2.width)
            && (bb1.x + bb1.width > bb2.x)
            && (bb1.y < bb2.y + bb2.height)
            && (bb1.height + bb1.y > bb2.y)
    then
        Just
            { left = bb1.x < bb2.x
            , right = bb1.x + bb1.width > bb2.x + bb2.width
            , top = bb1.y < bb2.y
            , bottom = bb1.y + bb1.height > bb2.y + bb2.height
            }

    else
        Nothing
