module Collision exposing
    ( Circle
    , LineSegment
    , Point
    , Rectangle
    , RectanglesInfo
    , detectCircleRect
    , detectCircleRectInfo
    , detectLineCircle
    , detectLineCircleInfo
    , detectRects
    , detectRectsInfo
    , left
    , right
    , toCircle
    , toLineSegment
    , toPoint
    , top
    )

{-| This module implements collision detection between a variety of shapes.

Implementation are heavily based on <https://www.jeffreythompson.org/collision-detection/>

-}

-- POINT


type alias Point =
    { x : Float
    , y : Float
    }


toPoint : ( Float, Float ) -> Point
toPoint ( x, y ) =
    Point x y


distance : Point -> Point -> Float
distance p1 p2 =
    sqrt <| (p2.x - p1.x) ^ 2 + (p2.y - p1.y) ^ 2



-- LINE SEGMENT


type alias LineSegment =
    { x1 : Float
    , y1 : Float
    , x2 : Float
    , y2 : Float
    }


toLineSegment : ( Float, Float ) -> ( Float, Float ) -> LineSegment
toLineSegment ( x1, y1 ) ( x2, y2 ) =
    LineSegment x1 y1 x2 y2


head : LineSegment -> Point
head { x1, y1 } =
    Point x1 y1


tail : LineSegment -> Point
tail { x2, y2 } =
    Point x2 y2


length : LineSegment -> Float
length line =
    distance (head line) (tail line)



-- CIRCLE


type alias Circle =
    { x : Float
    , y : Float
    , radius : Float
    }


toCircle : ( Float, Float ) -> Float -> Circle
toCircle ( x, y ) radius =
    Circle x y radius


center : Circle -> Point
center { x, y } =
    Point x y



-- RECTANGLE


type alias Rectangle =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


left : Rectangle -> Float
left { x } =
    x


right : Rectangle -> Float
right { x, width } =
    x + width


top : Rectangle -> Float
top { y } =
    y


bottom : Rectangle -> Float
bottom { y, height } =
    y + height



-- squishX : Float -> Rectangle -> Rectangle
-- squishX val rect =
--     { rect | x = rect.x + val, width = rect.width - val }


type alias RectanglesInfo =
    { top : Bool
    , bottom : Bool
    , left : Bool
    , right : Bool
    }



-- COLLISIONS


detectPointLineInfo : Point -> LineSegment -> Maybe ( LineSegment, LineSegment )
detectPointLineInfo p line =
    let
        distFromEnds =
            distance p (head line) + distance p (tail line)

        lineLen =
            length line

        -- TODO: make this configurable
        buffer =
            0.1
    in
    if distFromEnds >= lineLen - buffer && distFromEnds <= lineLen + buffer then
        Just <|
            ( toLineSegment ( line.x1, line.y1 ) ( p.x, p.y )
            , toLineSegment ( p.x, p.y ) ( line.x2, line.y2 )
            )

    else
        Nothing


-- detectPointLine : Point -> LineSegment -> Bool
-- detectPointLine p line =
--     detectPointLineInfo p line /= Nothing


detectPointCircle : Point -> Circle -> Bool
detectPointCircle p circle =
    distance p (center circle) <= circle.radius


{-| Detect the closest point between line and circle if they collide
-}
detectLineCircleInfo : LineSegment -> Circle -> Maybe ( LineSegment, LineSegment )
detectLineCircleInfo line circle =
    if detectPointCircle (head line) circle || detectPointCircle (tail line) circle then
        Nothing

    else
        let
            lineLen =
                length line

            dot =
                (((circle.x - line.x1) * (line.x2 - line.x1)) + ((circle.y - line.y1) * (line.y2 - line.y1))) / lineLen ^ 2

            closestX =
                line.x1 + (dot * (line.x2 - line.x1))

            closestY =
                line.y1 + (dot * (line.y2 - line.y1))

            closestPoint =
                Point closestX closestY
        in
        if detectPointCircle closestPoint circle then
            detectPointLineInfo closestPoint line

        else
            Nothing


detectLineCircle : LineSegment -> Circle -> Bool
detectLineCircle line circle =
    detectLineCircleInfo line circle /= Nothing


{-| Check for collisions between two rectangles. If there is a collision,
return which sides the first rectangle hit on the second rectange.

For example:

##################
#.......##.......#
#.Rect1.##.Rect2.#
#.......##.......#
##################

Rect1 hits the LEFT side of Rect2, so the result will be Just {left=true, ...}

-}
detectRectsInfo : Rectangle -> Rectangle -> Maybe RectanglesInfo
detectRectsInfo rect1 rect2 =
    if detectRects rect1 rect2 then
        Just
            { left = left rect1 < left rect2
            , right = right rect1 > right rect2
            , top = top rect1 < top rect2
            , bottom = bottom rect1 > bottom rect2
            }

    else
        Nothing


detectRects : Rectangle -> Rectangle -> Bool
detectRects rect1 rect2 =
    (left rect1 < right rect2)
        && (right rect1 > left rect2)
        && (top rect1 < bottom rect2)
        && (bottom rect1 > top rect2)



-- Circle / Rectangle


detectCircleRectInfo : Circle -> Rectangle -> Maybe RectanglesInfo
detectCircleRectInfo circle rect =
    if detectCircleRect circle rect then
        Just
            { left = circle.x - circle.radius < left rect
            , right = circle.x + circle.radius > right rect
            , top = circle.y - circle.radius < top rect
            , bottom = circle.y + circle.radius > bottom rect
            }

    else
        Nothing


detectCircleRect : Circle -> Rectangle -> Bool
detectCircleRect circle rect =
    detectPointCircle
        (toPoint ( detectCircleRectX circle rect, detectCircleRectY circle rect ))
        circle


detectCircleRectX : Circle -> Rectangle -> Float
detectCircleRectX circle rect =
    if circle.x < left rect then
        left rect

    else if circle.x > right rect then
        right rect

    else
        circle.x


detectCircleRectY : Circle -> Rectangle -> Float
detectCircleRectY circle rect =
    if circle.y < top rect then
        top rect

    else if circle.y > bottom rect then
        bottom rect

    else
        circle.y
