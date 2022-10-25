module App.Roller exposing
    (  Roller
       -- , applyKeyboardInputs
       -- , applyPhysics
       -- , boundingBox

    , init
    , render
    , update
    )

import App.Block exposing (Block)
import App.Collisions exposing (BoundingBox, CollisionSides)
import Canvas as V
import Canvas.Settings as VS
import Canvas.Settings.Advanced as VA
import Color
import Keyboard as K
import Keyboard.Arrows as KA



-- MODEL


type alias Env a =
    { a
        | pressedKeys : List K.Key
        , blocks : List Block
    }


type alias Roller =
    { x : Float
    , y : Float
    , velX : Float
    , velY : Float
    , groundedAt : Maybe Float
    }


init : Float -> Roller
init height =
    { x = 75

    -- , y = height * 0.75 - radius
    , y = 25
    , velX = 0

    -- bootleg gravity
    , velY = 0
    , groundedAt = Nothing
    }


radius : Float
radius =
    25


rotation : Float -> Float
rotation x =
    degrees (x * degreesPerMove)


degreesPerMove : Float
degreesPerMove =
    3.0


boundingBox : Roller -> BoundingBox
boundingBox { x, y } =
    BoundingBox (x - radius) (y - radius) (radius * 2) (radius * 2)



-- UPDATE


update : Env a -> Roller -> Roller
update env roller =
    roller
        |> applyKeyboardInputs env
        |> collideWithBlocks env
        |> applyGravity
        |> applyPhysics


applyKeyboardInputs : Env a -> Roller -> Roller
applyKeyboardInputs { pressedKeys } roller =
    let
        { x } =
            KA.arrows pressedKeys

        speedModifier =
            if List.member K.Shift pressedKeys then
                0.5

            else
                1
    in
    if x /= 0 then
        { roller | velX = toFloat x * accelerationX * speedModifier }

    else
        { roller | velX = 0 }


applyGravity : Roller -> Roller
applyGravity roller =
    case roller.groundedAt of
        Just groundY ->
            -- "climb" up ledge
            { roller | velY = 0.5 * (groundY - bottomY roller) }

        Nothing ->
            { roller | velY = accelerationY }


bottomY : Roller -> Float
bottomY roller =
    let
        { y, height } =
            boundingBox roller
    in
    y + height


applyPhysics : Roller -> Roller
applyPhysics roller =
    -- TODO: don't wrap, this is just for testing
    { roller
        | x = toFloat <| modBy 640 (round <| roller.x + roller.velX)
        , y = toFloat <| modBy 480 (round <| roller.y + roller.velY)
    }


collideWithBlocks : Env a -> Roller -> Roller
collideWithBlocks { blocks } roller =
    List.foldl collideWithBlock { roller | groundedAt = Nothing } blocks


collideWithBlock : Block -> Roller -> Roller
collideWithBlock block roller =
    case collideWithBlockSides block roller of
        Nothing ->
            roller

        Just { left, right, top, bottom } ->
            if top then
                { roller | groundedAt = Just block.y }

            else
                roller


collideWithBlockSides : Block -> Roller -> Maybe CollisionSides
collideWithBlockSides block roller =
    App.Collisions.collision
        (boundingBox roller)
        (App.Block.boundingBox block)


accelerationX : Float
accelerationX =
    3.0


{-| bootleg gravity
-}
accelerationY : Float
accelerationY =
    1.0



-- RENDER


render : Roller -> V.Renderable
render roller =
    V.shapes
        [ VS.fill Color.white
        , VS.stroke Color.black

        -- rotate
        , VA.transform
            [ VA.translate roller.x roller.y
            , VA.rotate (rotation roller.x)
            , VA.translate -roller.x -roller.y
            ]
        ]
        (ballWithEyes roller)


ballWithDash : Roller -> List V.Shape
ballWithDash roller =
    [ V.circle ( roller.x, roller.y ) radius
    , V.path ( roller.x, roller.y + (0.75 * radius) )
        [ V.lineTo ( roller.x, roller.y + radius )
        ]
    ]


ballWithEyes : Roller -> List V.Shape
ballWithEyes roller =
    [ V.circle ( roller.x, roller.y ) radius
    , V.circle ( roller.x, roller.y + (0.35 * radius) ) 3
    , V.circle ( roller.x + (0.75 * radius), roller.y + (0.35 * radius) ) 3
    ]
