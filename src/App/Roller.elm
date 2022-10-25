module App.Roller exposing
    ( Roller
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


type WalledAt
    = LeftX Float
    | RightX Float
      -- TODO: what does this mean? squished? is this even necessary?
    | BothX Float Float


type alias Roller =
    { x : Float
    , y : Float
    , velX : Float
    , velY : Float
    , groundedAt : Maybe Float
    , walledAt : Maybe WalledAt
    , firingLaser : Bool
    }


init : Roller
init =
    { x = 75
    , y = 25
    , velX = 0
    , velY = 0
    , groundedAt = Nothing
    , walledAt = Nothing
    , firingLaser = False
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
    BoundingBox
        (x - radius)
        (y - radius)
        (radius * 2)
        (radius * 2)



-- UPDATE


update : Env a -> Roller -> Roller
update env roller =
    roller
        |> applyKeyboardInputs env
        |> collideWithBlocks env
        |> applyWall
        |> applyGravityOrFloor
        |> applyPhysics


applyKeyboardInputs : Env a -> Roller -> Roller
applyKeyboardInputs env roller =
    roller
        |> applyKeyboardInputsDirections env
        |> applyKeyboardInputsLaser env


applyKeyboardInputsDirections : Env a -> Roller -> Roller
applyKeyboardInputsDirections { pressedKeys } roller =
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


applyKeyboardInputsLaser : Env a -> Roller -> Roller
applyKeyboardInputsLaser { pressedKeys } roller =
    { roller | firingLaser = List.member K.Spacebar pressedKeys }


applyGravityOrFloor : Roller -> Roller
applyGravityOrFloor roller =
    case roller.groundedAt of
        Just groundY ->
            -- "climb" up ledge
            { roller | velY = 0.5 * (groundY - bottomY roller) }

        Nothing ->
            { roller | velY = accelerationY }


applyWall : Roller -> Roller
applyWall roller =
    case roller.walledAt of
        Just (LeftX wallX) ->
            { roller | velX = 0.5 * (wallX - rightX roller - 0.1) }

        Just (RightX wallX) ->
            { roller | velX = 0.5 * (wallX - leftX roller + 0.1) }

        _ ->
            roller


leftX : Roller -> Float
leftX roller =
    let
        { x } =
            boundingBox roller
    in
    x


rightX : Roller -> Float
rightX roller =
    let
        { x, width } =
            boundingBox roller
    in
    x + width


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
    List.foldl
        collideWithBlock
        (preCollisionRoller roller)
        blocks


preCollisionRoller : Roller -> Roller
preCollisionRoller roller =
    { roller | groundedAt = Nothing, walledAt = Nothing }


collideWithBlock : Block -> Roller -> Roller
collideWithBlock block roller =
    case collideWithBlockSides block roller of
        Nothing ->
            roller

        Just { left, right, top } ->
            if top then
                { roller | groundedAt = Just block.y }

            else if left || right then
                { roller
                    | walledAt =
                        if left && right then
                            Just <| BothX block.x <| block.x + block.width

                        else if left then
                            Just <| LeftX block.x

                        else
                            Just <| RightX <| block.x + block.width
                }

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
    V.group
        [ VS.fill Color.white
        , VS.stroke Color.black

        -- rotate
        , VA.transform
            [ VA.translate roller.x roller.y
            , VA.rotate (rotation roller.x)
            , VA.translate -roller.x -roller.y

            -- , VA.applyMatrix { m11 = 1.6, m12 = 0, m21 = 0, m22 = 1, dx = 0, dy = 0 }
            ]
        ]
        [ renderBallWithEyes roller
        ]



-- ballWithDash : Roller -> List V.Shape
-- ballWithDash roller =
--     [ V.circle ( roller.x, roller.y ) radius
--     , V.path ( roller.x, roller.y + (0.75 * radius) )
--         [ V.lineTo ( roller.x, roller.y + radius )
--         ]
--     ]
-- , VA.shadow { blur = 50, color = Color.red, offset = ( 0, 0 ) }


renderBallWithEyes : Roller -> V.Renderable
renderBallWithEyes roller =
    let
        renderEye =
            if roller.firingLaser then
                renderLaserEye

            else
                renderNormalEye
    in
    V.group
        []
        [ renderBall roller
        , renderEye roller.x (roller.y + 0.35 * radius)
        , renderEye (roller.x + 0.75 * radius) (roller.y + 0.35 * radius)
        ]


renderBall : Roller -> V.Renderable
renderBall { x, y } =
    V.shapes [] [ V.circle ( x, y ) radius ]


renderNormalEye : Float -> Float -> V.Renderable
renderNormalEye x y =
    V.shapes
        []
        [ V.circle ( x, y ) 3
        ]


renderLaserEye : Float -> Float -> V.Renderable
renderLaserEye x y =
    V.shapes
        [ VA.shadow { blur = 5, color = Color.red, offset = ( 0, 0 ) }
        , VS.stroke Color.red
        ]
        [ V.circle ( x, y ) 3
        , V.path ( x, y )
            [ V.lineTo ( x + 1000, y + 500 ) ]
        ]
