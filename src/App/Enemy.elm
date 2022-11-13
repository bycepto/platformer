module App.Enemy exposing
    ( Enemy
    , circle
    , init
    , render
    , update
    )

import App.Block exposing (Block)
import App.Lava exposing (Lava)
import Canvas as V
import Canvas.Settings as VS
import Canvas.Settings.Advanced as VA
import Collision as CL
import Color



-- MODEL


type alias Env a =
    { a
        | blocks : List Block
        , lava : List Lava
        , tick : Float
        , devMode : Bool
    }


type WalledAt
    = LeftX Float
    | RightX Float
      -- TODO: what does this mean? squished? is this even necessary?
    | BothX Float Float


type alias Enemy =
    { x : Float
    , y : Float
    , angle : Float
    , velX : Float
    , velY : Float
    , groundedAt : Maybe Float
    , walledAt : Maybe WalledAt

    -- TODO: consider making these states
    , firingLaser : Bool
    , deadAtTick : Maybe Float
    }


init : Enemy
init =
    { x = 400
    , y = 25
    , angle = 0
    , velX = 0
    , velY = 0
    , groundedAt = Nothing
    , walledAt = Nothing
    , firingLaser = False
    , deadAtTick = Nothing
    }


circle : Enemy -> CL.Circle
circle { x, y } =
    CL.Circle x y radius


radius : Float
radius =
    25


degreesPerMove : Float
degreesPerMove =
    3.0


boundingBox : Enemy -> CL.Rectangle
boundingBox { x, y } =
    {- TODO:
       make this platform bounding box, but use a different bounding box
       or collision methods for enemies.
    -}
    CL.Rectangle
        (x - radius + boundingBoxXPadding)
        (y - radius)
        (radius * 2 - boundingBoxXPadding * 2)
        (radius * 2)


boundingBoxXPadding : Float
boundingBoxXPadding =
    10



-- UPDATE


update : Env a -> Enemy -> Enemy
update env roller =
    if roller.deadAtTick /= Nothing then
        roller

    else
        roller
            |> collideWithBlocks env
            |> applyWall
            |> applyGravityOrFloor
            |> applyPhysics
            |> collideWithLava env


collideWithLava : Env a -> Enemy -> Enemy
collideWithLava { tick, lava } enemy =
    if List.any (dectectLavaCollision enemy) lava then
        { enemy | deadAtTick = Just tick }

    else
        enemy


dectectLavaCollision : Enemy -> Lava -> Bool
dectectLavaCollision enemy lava =
    CL.detectRects (App.Lava.boundingBox lava) (boundingBox enemy)


applyGravityOrFloor : Enemy -> Enemy
applyGravityOrFloor roller =
    case roller.groundedAt of
        Just groundY ->
            { roller | y = groundY - radius }

        Nothing ->
            { roller | velY = accelerationY }


applyWall : Enemy -> Enemy
applyWall roller =
    case roller.walledAt of
        Just (LeftX wallX) ->
            { roller | velX = 0.5 * (wallX - rightX roller - 0.1) }

        Just (RightX wallX) ->
            { roller | velX = 0.5 * (wallX - leftX roller + 0.1) }

        _ ->
            -- TODO: what about BothX?
            roller


leftX : Enemy -> Float
leftX roller =
    let
        { x } =
            boundingBox roller
    in
    x


rightX : Enemy -> Float
rightX roller =
    let
        { x, width } =
            boundingBox roller
    in
    x + width


applyPhysics : Enemy -> Enemy
applyPhysics roller =
    -- TODO: don't wrap, this is just for testing
    { roller
        | x = roller.x + roller.velX
        , y = toFloat <| modBy 480 (round <| roller.y + roller.velY)
    }


collideWithBlocks : Env a -> Enemy -> Enemy
collideWithBlocks { blocks } roller =
    List.foldl
        collideWithBlock
        (preCollisionEnemy roller)
        -- TODO: can we make the logic below work without sorting the blocks
        -- from lowest to highest?
        (List.sortBy (negate << .y) blocks)



-- blocks


preCollisionEnemy : Enemy -> Enemy
preCollisionEnemy roller =
    { roller | groundedAt = Nothing, walledAt = Nothing }


collideWithBlock : Block -> Enemy -> Enemy
collideWithBlock block roller =
    case collideWithBlockSides block roller of
        Nothing ->
            roller

        Just sides ->
            roller
                |> collideWithBlockTop block sides
                |> collideWithBlockLeft block sides
                |> collideWithBlockRight block sides


collideWithBlockTop : Block -> CL.RectanglesInfo -> Enemy -> Enemy
collideWithBlockTop block { top } roller =
    if not top then
        roller

    else if canClimbUp block roller then
        -- TODO: can we make the logic below work without sorting the blocks
        -- from lowest to highest?
        { roller | groundedAt = Just block.y }

    else
        roller


collideWithBlockLeft : Block -> CL.RectanglesInfo -> Enemy -> Enemy
collideWithBlockLeft block { left } roller =
    if not left then
        roller

    else if canClimbUp block roller then
        { roller | groundedAt = Just block.y }

    else
        -- { roller | walledAt = Just (LeftX block.x) }
        updateWalledAtWith (LeftX block.x) roller


collideWithBlockRight : Block -> CL.RectanglesInfo -> Enemy -> Enemy
collideWithBlockRight block { right } roller =
    if not right then
        roller

    else if canClimbUp block roller then
        { roller | groundedAt = Just block.y }

    else
        -- { roller | walledAt = Just (RightX <| block.x + block.width) }
        updateWalledAtWith (RightX <| block.x + block.width) roller


updateWalledAtWith : WalledAt -> Enemy -> Enemy
updateWalledAtWith walledAt roller =
    case ( walledAt, roller.walledAt ) of
        ( LeftX lx, Just (RightX rx) ) ->
            { roller | walledAt = Just <| BothX lx rx }

        ( RightX rx, Just (LeftX lx) ) ->
            { roller | walledAt = Just <| BothX lx rx }

        ( LeftX lx, Just (BothX _ rx) ) ->
            { roller | walledAt = Just <| BothX lx rx }

        ( RightX rx, Just (BothX lx _) ) ->
            { roller | walledAt = Just <| BothX lx rx }

        _ ->
            { roller | walledAt = Just walledAt }


canClimbUp : Block -> Enemy -> Bool
canClimbUp block roller =
    case roller.groundedAt of
        Nothing ->
            roller.y + radius - block.y < canClimbUpThreshold

        Just groundedAt ->
            groundedAt - block.y < canClimbUpThreshold


canClimbUpThreshold : Float
canClimbUpThreshold =
    27


collideWithBlockSides : Block -> Enemy -> Maybe CL.RectanglesInfo
collideWithBlockSides block roller =
    CL.detectRectsInfo
        (boundingBox roller)
        (App.Block.boundingBox block)


accelerationX : Float
accelerationX =
    3.0


{-| bootleg gravity
-}
accelerationY : Float
accelerationY =
    3.0



-- RENDER


render : Env a -> Enemy -> V.Renderable
render env roller =
    if roller.deadAtTick /= Nothing then
        V.group [] [ renderExplosion env roller ]

    else
        V.group
            []
            [ renderRollingBallWithEyes env roller
            , renderBoundingBox env roller
            ]


renderBoundingBox : Env a -> Enemy -> V.Renderable
renderBoundingBox { devMode } roller =
    if devMode then
        let
            { x, y, width, height } =
                boundingBox roller
        in
        V.shapes
            [ VS.stroke Color.red
            , VA.alpha 0.5
            ]
            [ V.rect ( x, y ) width height ]

    else
        V.shapes [] []


renderRollingBallWithEyes : Env a -> Enemy -> V.Renderable
renderRollingBallWithEyes env roller =
    V.group
        [ VS.fill Color.white
        , VS.stroke Color.black

        -- rotate
        , VA.transform
            [ VA.translate roller.x roller.y
            , VA.rotate (degrees roller.angle)
            , VA.translate -roller.x -roller.y

            -- , VA.applyMatrix { m11 = 1.6, m12 = 0, m21 = 0, m22 = 1, dx = 0, dy = 0 }
            ]
        ]
        [ renderBallWithEyes env roller
        ]


renderBallWithEyes : Env a -> Enemy -> V.Renderable
renderBallWithEyes _ roller =
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

        -- , renderMouth env roller
        , renderEye roller.x (roller.y - 0.35 * radius)
        , renderEye (roller.x - 0.75 * radius) (roller.y - 0.35 * radius)
        ]


renderBall : Enemy -> V.Renderable
renderBall { x, y } =
    V.shapes
        [ VS.fill Color.black ]
        [ V.circle ( x, y ) radius ]


renderMouth : Env a -> Enemy -> V.Renderable
renderMouth { tick } { x, y } =
    V.shapes
        -- TODO: fill inside of path?
        [ VS.fill Color.purple ]
        [ V.path ( x - 10, y - 5 )
            [ V.lineTo ( x, y - 10 )
            , V.lineTo ( x + 5, y - 5 )
            , V.lineTo ( x + 10, y - 10 )
            , V.lineTo ( x + 15, y - 5 )
            , V.lineTo ( x + 20, y - 10 )
            , V.lineTo ( x + 25, y - 5 )

            -- , V.lineTo ( x - 10, y - 5 )
            ]
        , if modBy 2 (round tick // 8) == 0 then
            V.path ( x - 10, y - 5 )
                [ V.lineTo ( x, y - 20 )
                , V.lineTo ( x + 5, y - 14 )
                , V.lineTo ( x + 10, y - 20 )
                , V.lineTo ( x + 15, y - 14 )
                , V.lineTo ( x + 20, y - 20 )
                , V.lineTo ( x + 25, y - 5 )

                -- , V.lineTo ( x - 10, y - 5 )
                ]

          else
            V.path ( x - 10, y - 5 )
                [ V.lineTo ( x, y - 18 )
                , V.lineTo ( x + 5, y - 12 )
                , V.lineTo ( x + 10, y - 18 )
                , V.lineTo ( x + 15, y - 12 )
                , V.lineTo ( x + 20, y - 18 )
                , V.lineTo ( x + 25, y - 5 )

                -- , V.lineTo ( x - 10, y - 5 )
                ]
        ]


renderNormalEye : Float -> Float -> V.Renderable
renderNormalEye x y =
    V.shapes
        [ VS.fill Color.lightGreen ]
        [ V.circle ( x, y ) 3
        ]


renderLaserEye : Float -> Float -> V.Renderable
renderLaserEye x y =
    let
        line =
            CL.toLineSegment ( x, y ) ( x - 1000, y )
    in
    V.shapes
        [ VA.shadow { blur = 5, color = Color.red, offset = ( 0, 0 ) }
        , VS.stroke Color.red
        ]
        [ V.circle ( x, y ) 3
        , V.path ( line.x1, line.y1 )
            [ V.lineTo ( line.x2, line.y2 ) ]
        ]


renderExplosion : Env a -> Enemy -> V.Renderable
renderExplosion env enemy =
    case enemy.deadAtTick of
        Nothing ->
            V.group [] []

        Just tick ->
            if env.tick > tick + toFloat explosionDuration then
                V.group [] []

            else
                V.group
                    []
                    [ renderExplosionParticle ( enemy.x, enemy.y ) 1 1 (env.tick - tick)
                    , renderExplosionParticle ( enemy.x, enemy.y ) 1 -1 (env.tick - tick)
                    , renderExplosionParticle ( enemy.x, enemy.y ) -1 -1 (env.tick - tick)
                    , renderExplosionParticle ( enemy.x, enemy.y ) -1 1 (env.tick - tick)
                    , renderExplosionParticle ( enemy.x, enemy.y ) 2 -1 (env.tick - tick)
                    , renderExplosionParticle ( enemy.x, enemy.y ) -1 -2 (env.tick - tick)
                    , renderExplosionParticle ( enemy.x, enemy.y ) -1 2 (env.tick - tick)
                    , renderExplosionParticle ( enemy.x, enemy.y ) -3 2 (env.tick - tick)
                    , renderExplosionParticle ( enemy.x, enemy.y ) -3 10 (env.tick - tick)
                    , renderExplosionParticle ( enemy.x, enemy.y ) -10 2 (env.tick - tick)
                    ]


explosionDuration : Int
explosionDuration =
    50


renderExplosionParticle : V.Point -> Float -> Float -> Float -> V.Renderable
renderExplosionParticle ( x, y ) velX velY tick =
    let
        size =
            10

        rotationPerTick =
            10
    in
    V.shapes
        [ VS.fill Color.white
        , VS.stroke Color.green
        , VA.alpha <| (100 - 1.5 * tick) / 100
        , VA.transform
            [ VA.translate
                (x + size / 2 + velX * tick)
                (y + size / 2 + velY * tick)
            , VA.rotate (degrees <| tick * rotationPerTick)
            , VA.translate
                -(x + size / 2)
                -(y + size / 2)
            ]
        ]
        [ V.rect ( x, y ) 10 10 ]
