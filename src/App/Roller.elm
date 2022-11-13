module App.Roller exposing
    ( Laser
    , Roller
    , eyeLasers
    , init
    , render
    , renderLaser
    , update
    )

import App.Block exposing (Block)
import App.Lava exposing (Lava)
import Canvas as V
import Canvas.Settings as VS
import Canvas.Settings.Advanced as VA
import Collision as CL
import Color
import Keyboard as K
import Keyboard.Arrows as KA



-- MODEL


type alias Env a =
    { a
        | pressedKeys : List K.Key
        , blocks : List Block
        , lava : List Lava
        , tick : Float
        , devMode : Bool
    }


type alias Roller =
    { x : Float
    , y : Float
    , angle : Float
    , velX : Float
    , velY : Float
    , obstaclesToLeft : List CL.Rectangle
    , obstaclesToRight : List CL.Rectangle
    , obstaclesBelow : List CL.Rectangle
    , firingLaser : Bool
    , deadAtTick : Maybe Float
    }


init : Roller
init =
    { x = 75
    , y = 25
    , angle = 0
    , velX = 0
    , velY = 0
    , obstaclesToLeft = []
    , obstaclesToRight = []
    , obstaclesBelow = []
    , firingLaser = False
    , deadAtTick = Nothing
    }


radius : Float
radius =
    25


degreesPerMove : Float
degreesPerMove =
    2.0


circle : Roller -> CL.Circle
circle { x, y } =
    CL.toCircle ( x, y ) radius



-- UPDATE


update : Env a -> Roller -> Roller
update env roller =
    if roller.deadAtTick /= Nothing then
        roller

    else
        roller
            |> applyKeyboardInputs env
            |> detectBlockCollisions env
            |> applyVelX
            |> applyVelY
            |> collideWithLava env


collideWithLava : Env a -> Roller -> Roller
collideWithLava { tick, lava } roller =
    if List.any (dectectLavaCollision roller) lava then
        { roller | deadAtTick = Just tick }

    else
        roller


dectectLavaCollision : Roller -> Lava -> Bool
dectectLavaCollision roller lava =
    CL.detectCircleRect (circle roller) (App.Lava.boundingBox lava)


applyKeyboardInputs : Env a -> Roller -> Roller
applyKeyboardInputs env roller =
    roller
        |> applyKeyboardInputsDirections env
        |> applyKeyboardInputsLaser env


applyKeyboardInputsDirections : Env a -> Roller -> Roller
applyKeyboardInputsDirections { pressedKeys } roller =
    let
        arrows =
            KA.arrows pressedKeys

        wasd =
            KA.wasd pressedKeys

        x =
            clamp -1 1 (arrows.x + wasd.x)

        pressingShift =
            List.member K.Shift pressedKeys

        velModifier =
            if pressingShift then
                0

            else
                1

        spinModifier =
            if pressingShift then
                0.1

            else
                1
    in
    if x /= 0 then
        { roller
            | velX = toFloat x * accelerationX * velModifier
            , angle = roller.angle + degreesPerMove * toFloat x * accelerationX * spinModifier
        }

    else
        { roller | velX = 0 }


applyKeyboardInputsLaser : Env a -> Roller -> Roller
applyKeyboardInputsLaser { pressedKeys } roller =
    { roller | firingLaser = List.member K.Spacebar pressedKeys }


applyVelX : Roller -> Roller
applyVelX roller =
    if roller.velX < 0 then
        case
            roller.obstaclesToLeft
                |> List.filter (not << canClimbUp roller)
                |> List.map CL.right
                |> List.maximum
        of
            Nothing ->
                { roller | x = roller.x + roller.velX }

            Just x ->
                { roller | x = x + radius }

    else if roller.velX > 0 then
        case
            roller.obstaclesToRight
                |> List.filter (not << canClimbUp roller)
                |> List.map CL.left
                |> List.minimum
        of
            Nothing ->
                { roller | x = roller.x + roller.velX }

            Just x ->
                { roller | x = x - radius }

    else
        roller


applyVelY : Roller -> Roller
applyVelY roller =
    case
        roller.obstaclesBelow
            |> List.filterMap (getGroundPoint roller)
            |> List.minimum
    of
        Nothing ->
            fall roller

        Just y ->
            { roller | y = y - radius + 1 }


{-| Assuming that the roller has collided with the top of the given rectangle,
return where the lowest point of the circle should be.
-}
getGroundPoint : Roller -> CL.Rectangle -> Maybe Float
getGroundPoint roller rect =
    if roller.x < CL.left rect then
        if roller.x + radius - hangXThreshold < CL.left rect then
            -- too far off
            Nothing

        else
            -- hang off the left side of the rectangle
            let
                th =
                    atan2 (CL.top rect - roller.y) (CL.left rect - roller.x)
            in
            Just <| CL.top rect + radius * (1 - sin th)

    else if roller.x > CL.right rect then
        if roller.x - radius + hangXThreshold > CL.right rect then
            -- too far off
            Nothing

        else
            -- hang off the right side of the rectangle
            let
                th =
                    atan2 (CL.top rect - roller.y) (CL.right rect - roller.x)
            in
            Just <| CL.top rect + radius * (1 - sin th)

    else
        -- no hanging
        Just <| CL.top rect


hangXThreshold : Float
hangXThreshold =
    3


fall : Roller -> Roller
fall roller =
    { roller | y = toFloat <| modBy 480 <| round <| roller.y + accelerationY }


detectBlockCollisions : Env a -> Roller -> Roller
detectBlockCollisions { blocks } roller =
    List.foldl collideWithBlock (preCollisionRoller roller) blocks



-- blocks


preCollisionRoller : Roller -> Roller
preCollisionRoller roller =
    { roller
        | obstaclesBelow = []
        , obstaclesToLeft = []
        , obstaclesToRight = []
    }


collideWithBlock : Block -> Roller -> Roller
collideWithBlock block roller =
    case collideWithBlockSides block roller of
        Nothing ->
            roller

        Just sides ->
            roller
                |> collideWithBlockLeft block sides
                |> collideWithBlockRight block sides
                |> collideWithBlockTop block sides


collideWithBlockTop : Block -> CL.RectanglesInfo -> Roller -> Roller
collideWithBlockTop block { top } roller =
    if top then
        { roller | obstaclesBelow = App.Block.boundingBox block :: roller.obstaclesBelow }

    else
        roller


collideWithBlockLeft : Block -> CL.RectanglesInfo -> Roller -> Roller
collideWithBlockLeft block { left } roller =
    if left then
        { roller | obstaclesToRight = App.Block.boundingBox block :: roller.obstaclesToRight }

    else
        roller


collideWithBlockRight : Block -> CL.RectanglesInfo -> Roller -> Roller
collideWithBlockRight block { right } roller =
    if right then
        { roller | obstaclesToLeft = App.Block.boundingBox block :: roller.obstaclesToLeft }

    else
        roller


canClimbUp : Roller -> CL.Rectangle -> Bool
canClimbUp roller rect =
    roller.y + radius - rect.y < canClimbUpThreshold


canClimbUpThreshold : Float
canClimbUpThreshold =
    20


collideWithBlockSides : Block -> Roller -> Maybe CL.RectanglesInfo
collideWithBlockSides block roller =
    CL.detectCircleRectInfo
        (circle roller)
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


render : Env a -> Roller -> V.Renderable
render env roller =
    V.group
        []
        [ if env.devMode then
            V.text
                [ VS.stroke Color.red ]
                ( 15, 15 )
            <|
                String.join " "
                    [ "x"
                    , String.fromFloat roller.x
                    , "y"
                    , String.fromFloat roller.y
                    , "ang"
                    , String.fromFloat roller.angle
                    , "velX"
                    , String.fromFloat roller.velX
                    , "velY"
                    , String.fromFloat roller.velY
                    ]

          else
            V.text [] ( 0, 0 ) ""
        , if roller.deadAtTick /= Nothing then
            renderExplosion env roller

          else
            renderRollingBallWithEyes env roller
        ]


renderRollingBallWithEyes : Env a -> Roller -> V.Renderable
renderRollingBallWithEyes env roller =
    V.group
        [ VS.fill Color.white
        , VS.stroke Color.black
        ]
        [ renderBallWithEyes env roller
        ]


renderBallWithEyes : Env a -> Roller -> V.Renderable
renderBallWithEyes _ roller =
    let
        renderEye =
            if roller.firingLaser then
                renderLaserEyeWithEnemy

            else
                renderNormalEye
    in
    V.group
        []
        [ renderBall roller
        , renderEye <| backEyeLocation roller
        , renderEye <| frontEyeLocation roller
        ]


eyeLocation : Float -> Float -> Roller -> V.Point
eyeLocation radiusPct angleOffset roller =
    Tuple.mapBoth
        ((+) roller.x)
        ((+) roller.y)
    <|
        fromPolar ( radiusPct * radius, degrees <| roller.angle + angleOffset )


backEyeLocation : Roller -> V.Point
backEyeLocation roller =
    eyeLocation 0.35 -90 roller


frontEyeLocation : Roller -> V.Point
frontEyeLocation roller =
    eyeLocation 0.8 -25 roller


renderBall : Roller -> V.Renderable
renderBall { x, y } =
    V.shapes [] [ V.circle ( x, y ) radius ]


renderMouth : Env a -> Roller -> V.Renderable
renderMouth { tick } { x, y } =
    V.shapes
        []
        [ V.path ( x - 10, y - 5 )
            [ V.lineTo ( x, y - 10 )
            , V.lineTo ( x + 5, y - 5 )
            , V.lineTo ( x + 10, y - 10 )
            , V.lineTo ( x + 15, y - 5 )
            , V.lineTo ( x + 20, y - 10 )
            , V.lineTo ( x + 25, y - 5 )
            ]
        , if modBy 2 (round tick // 8) == 0 then
            V.path ( x - 10, y - 5 )
                [ V.lineTo ( x, y - 20 )
                , V.lineTo ( x + 5, y - 14 )
                , V.lineTo ( x + 10, y - 20 )
                , V.lineTo ( x + 15, y - 14 )
                , V.lineTo ( x + 20, y - 20 )
                , V.lineTo ( x + 25, y - 5 )
                ]

          else
            V.path ( x - 10, y - 5 )
                [ V.lineTo ( x, y - 18 )
                , V.lineTo ( x + 5, y - 12 )
                , V.lineTo ( x + 10, y - 18 )
                , V.lineTo ( x + 15, y - 12 )
                , V.lineTo ( x + 20, y - 18 )
                , V.lineTo ( x + 25, y - 5 )
                ]
        ]


renderNormalEye : ( Float, Float ) -> V.Renderable
renderNormalEye p =
    V.shapes [] [ V.circle p 3 ]


type alias Laser =
    { source : V.Point
    , target : V.Point
    }


eyeLasers : Roller -> List Laser
eyeLasers roller =
    List.map
        (eyeLaser roller.angle)
        [ backEyeLocation roller
        , frontEyeLocation roller
        ]


eyeLaser : Float -> V.Point -> Laser
eyeLaser angle ( x, y ) =
    let
        ( dx, dy ) =
            fromPolar ( 1000, degrees angle )
    in
    { source = ( x, y )
    , target = ( x + dx, y + dy )
    }


renderLaser : Laser -> V.Renderable
renderLaser { source, target } =
    let
        ( x1, y1 ) =
            source

        ( x2, y2 ) =
            target
    in
    V.shapes
        [ VA.shadow { blur = 5, color = Color.red, offset = ( 0, 0 ) }
        , VS.stroke Color.red
        ]
        [ V.path ( x1, y1 )
            [ V.lineTo ( x2, y2 ) ]
        ]


renderLaserEyeWithEnemy : V.Point -> V.Renderable
renderLaserEyeWithEnemy ( x, y ) =
    V.shapes
        [ VA.shadow { blur = 5, color = Color.red, offset = ( 0, 0 ) }
        , VS.stroke Color.red
        ]
        [ V.circle ( x, y ) 3
        ]


renderExplosion : Env a -> Roller -> V.Renderable
renderExplosion env roller =
    case roller.deadAtTick of
        Nothing ->
            V.group [] []

        Just tick ->
            if env.tick > tick + toFloat explosionDuration then
                V.group [] []

            else
                V.group
                    []
                    [ renderExplosionParticle ( roller.x, roller.y ) 1 1 (env.tick - tick)
                    , renderExplosionParticle ( roller.x, roller.y ) 1 -1 (env.tick - tick)
                    , renderExplosionParticle ( roller.x, roller.y ) -1 -1 (env.tick - tick)
                    , renderExplosionParticle ( roller.x, roller.y ) -1 1 (env.tick - tick)
                    , renderExplosionParticle ( roller.x, roller.y ) 2 -1 (env.tick - tick)
                    , renderExplosionParticle ( roller.x, roller.y ) -1 -2 (env.tick - tick)
                    , renderExplosionParticle ( roller.x, roller.y ) -1 2 (env.tick - tick)
                    , renderExplosionParticle ( roller.x, roller.y ) -3 2 (env.tick - tick)
                    , renderExplosionParticle ( roller.x, roller.y ) -3 10 (env.tick - tick)
                    , renderExplosionParticle ( roller.x, roller.y ) -10 2 (env.tick - tick)
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
