module App.Roller exposing
    ( Roller
    , circle
    , eyes
    , init
    , pushFrom
    , render
    , stopX
    , update
    )

import App.Block exposing (Block, Slope)
import App.Lava exposing (Lava)
import Canvas as V
import Canvas.Settings as VS
import Canvas.Settings.Advanced as VA
import Collision as CL
import Color exposing (Color)



-- MODEL


type alias Env a =
    { a
        | tick : Float
        , devMode : Bool
    }


type alias Room a =
    { a
        | blocks : List Block
        , lava : List Lava
        , slopes : List Slope
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

    -- config
    , facingLeft : Bool
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
    , facingLeft = False
    }


radius : Float
radius =
    25


circle : Roller -> CL.Shape
circle { x, y } =
    CL.Circle <| CL.toCircle ( x, y ) radius



-- UPDATE


update : Env a -> Room b -> Roller -> Roller
update env room roller =
    if roller.deadAtTick /= Nothing then
        roller

    else
        roller
            |> detectBlockCollisions room
            |> applyVelX
            |> applyVelY
            |> collideWithSlopes room.slopes
            |> collideWithLava env room


collideWithLava : Env a -> Room b -> Roller -> Roller
collideWithLava { tick } { lava } roller =
    if List.any (detectLavaCollision roller) lava then
        { roller | deadAtTick = Just tick }

    else
        roller


detectLavaCollision : Roller -> Lava -> Bool
detectLavaCollision roller lava =
    case circle roller of
        CL.Circle c ->
            CL.detectCircleRect c (App.Lava.boundingBox lava)


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
    { roller | y = roller.y + accelerationY }


detectBlockCollisions : Room b -> Roller -> Roller
detectBlockCollisions { blocks } roller =
    List.foldl collideWithBlock (preCollisionRoller roller) blocks



-- lasers


pushFrom : Float -> Float -> Roller -> Roller
pushFrom fromX atY roller =
    { roller
        | velX =
            clamp
                -maxAbsPushX
                maxAbsPushX
                roller.velX
                + (if fromX < roller.x then
                    0.05

                   else
                    -0.05
                  )
        , angle =
            roller.angle
                + (if fromX < roller.x then
                    if atY < roller.y then
                        3

                    else
                        -3

                   else if atY < roller.y then
                    -3

                   else
                    3
                  )
    }


maxAbsPushX : Float
maxAbsPushX =
    2


stopX : Roller -> Roller
stopX roller =
    { roller | velX = 0 }



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
    case circle roller of
        CL.Circle c ->
            CL.detectCircleRectInfo c (App.Block.boundingBox block)


{-| bootleg gravity
-}
accelerationY : Float
accelerationY =
    3.0



-- slopes


collideWithSlopes : List Slope -> Roller -> Roller
collideWithSlopes slopes roller =
    List.foldl
        collideWithSlope
        roller
        slopes


collideWithSlope : Slope -> Roller -> Roller
collideWithSlope slope roller =
    let
        line =
            CL.toLineSegment ( slope.x1, slope.y1 ) ( slope.x2, slope.y2 )
    in
    case circle roller of
        CL.Circle c ->
            case CL.detectLineCircleInfo line c of
                Nothing ->
                    roller

                Just ( leftSegment, rightSegment ) ->
                    { roller
                        | y = leftSegment.y2 - radius
                        , velX =
                            -- if roller.y > rightSegment.y2 && App.Block.grade slope < -0.1 then
                                roller.velX * 1 / (roller.x - rightSegment.x1)
                            --
                            -- else
                            --     roller.velX
                    }



-- else
--     roller
-- canClimbUpSlope : Slope -> Roller -> Bool
-- canClimbUpSlope slope roller =
--     roller.y + radius - canClimbUpSlopeThreshold < segment.y2 && App.Block.grade slope < 3
--


canClimbUpSlopeThreshold : Float
canClimbUpSlopeThreshold =
    10



-- RENDER


type alias Conf =
    { bodyColor : Color
    , eyeColor : Color
    , debug : Bool
    }


render : Env a -> Conf -> Roller -> V.Renderable
render env conf roller =
    V.group
        []
        [ if conf.debug && env.devMode then
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
            renderRollingBallWithEyes conf roller
        ]


renderRollingBallWithEyes : Conf -> Roller -> V.Renderable
renderRollingBallWithEyes conf roller =
    V.group
        [ VS.fill conf.bodyColor
        , VS.stroke Color.black
        ]
        [ renderBallWithEyes conf roller
        ]


renderBallWithEyes : Conf -> Roller -> V.Renderable
renderBallWithEyes conf roller =
    let
        renderEye =
            if roller.firingLaser then
                renderLaserEye

            else
                renderNormalEye conf
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
    if roller.facingLeft then
        eyeLocation -0.35 -90 roller

    else
        eyeLocation 0.35 -90 roller


frontEyeLocation : Roller -> V.Point
frontEyeLocation roller =
    if roller.facingLeft then
        eyeLocation -0.8 -155 roller

    else
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


renderNormalEye : Conf -> ( Float, Float ) -> V.Renderable
renderNormalEye conf p =
    V.shapes
        [ VS.fill conf.eyeColor ]
        [ V.circle p 3 ]


eyes : Roller -> List V.Point
eyes roller =
    [ backEyeLocation roller
    , frontEyeLocation roller
    ]


renderLaserEye : V.Point -> V.Renderable
renderLaserEye ( x, y ) =
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
