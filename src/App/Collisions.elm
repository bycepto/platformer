module App.Collisions exposing (BoundingBox, CollisionType(..), collision)


type alias BoundingBox =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


type CollisionType
    = Top
    | Bottom
    | Left
    | Right


{-| Check for collisions between two bounding boxes. If there is a collision,
returns a list of sides that the first bounding box hit on the second bound
box.

For example:

##############
#.....##.....#
#.BB1.##.BB2.#
#.....##.....#
##############

This is a LEFT collision, since BB1 collided with the left side of BB2

-}
collision : BoundingBox -> BoundingBox -> List CollisionType
collision bb1 bb2 =
    if
        (bb1.x < bb2.x + bb2.width)
            && (bb1.x + bb1.width > bb2.x)
            && (bb1.y < bb2.y + bb2.height)
            && (bb1.height + bb1.y > bb2.y)
    then
        List.concat
            [ if bb1.x < bb2.x then
                [ Left ]

              else
                []
            , if bb1.x + bb1.width > bb2.x + bb2.width then
                [ Right ]

              else
                []
            , if bb1.y < bb2.y then
                [ Top ]

              else
                []
            , if bb1.y + bb1.height > bb2.y + bb2.height then
                [ Bottom ]

              else
                []
            ]

    else
        []
