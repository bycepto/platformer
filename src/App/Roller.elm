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
import App.Enemy exposing (Enemy)
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
        , tick : Float
        , devMode : Bool

        -- TODO: hack! remove
        , enemy : Enemy
    }


type alias Roller =
    { x : Float
    , y : Float
    , angle : Float
    , velX : Float
    , velY : Float
    , groundedAt : Maybe Float
    , walledAt : Maybe WalledAt
    , firingLaser : Bool
    }


type WalledAt
    = LeftX Float
    | RightX Float
      -- TODO: what does this mean? squished? is this even necessary?
    | BothX Float Float


init : Roller
init =
    { x = 75
    , y = 25
    , angle = 0
    , velX = 0
    , velY = 0
    , groundedAt = Nothing
    , walledAt = Nothing
    , firingLaser = False
    }


radius : Float
radius =
    25


degreesPerMove : Float
degreesPerMove =
    2.0


circle : Roller -> CL.Circle
circle { x, y } =
    {- TODO:
       make this platform bounding box, but use a different bounding box
       or collision methods for enemies.
    -}
    -- CL.Rectangle
    --     (x - radius + boundingBoxXPadding)
    --     (y - radius)
    --     (radius * 2 - boundingBoxXPadding * 2)
    --     (radius * 2)
    CL.toCircle ( x, y ) radius



-- boundingBoxXPadding : Float
-- boundingBoxXPadding =
--     10
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


applyGravityOrFloor : Roller -> Roller
applyGravityOrFloor roller =
    case roller.groundedAt of
        Just groundY ->
            -- let
            --     _ =
            --         Debug.log "ground" "ground"
            -- in
            { roller | y = groundY - radius }

        Nothing ->
            -- let
            --     _ =
            --         Debug.log "gravity" "gravity"
            -- in
            { roller | velY = accelerationY }


applyWall : Roller -> Roller
applyWall roller =
    case roller.walledAt of
        Just (LeftX wallX) ->
            { roller | velX = 0.5 * (wallX - rightX roller - 0.1) }

        Just (RightX wallX) ->
            { roller | velX = 0.5 * (wallX - leftX roller + 0.1) }

        _ ->
            -- TODO: what about BothX?
            roller


leftX : Roller -> Float
leftX { x } =
    x - radius


rightX : Roller -> Float
rightX { x } =
    x + radius


applyPhysics : Roller -> Roller
applyPhysics roller =
    -- TODO: don't wrap, this is just for testing
    { roller
        | x = roller.x + roller.velX
        , y = toFloat <| modBy 480 (round <| roller.y + roller.velY)
    }


collideWithBlocks : Env a -> Roller -> Roller
collideWithBlocks { blocks } roller =
    -- let
    --     result =
    --         List.foldl
    --             collideWithBlock
    --             (preCollisionRoller roller)
    --             -- TODO: can we make the logic below work without sorting the blocks
    --             -- from lowest to highest?
    --             (List.sortBy (negate << .y) blocks)
    --
    --     _ =
    --         Debug.log "GROUNDED AT" result.groundedAt
    -- in
    -- result
    List.foldl
        collideWithBlock
        (preCollisionRoller roller)
        -- TODO: can we make the logic below work without sorting the blocks
        -- from lowest to highest?
        (List.sortBy (negate << .y) blocks)



-- blocks


preCollisionRoller : Roller -> Roller
preCollisionRoller roller =
    { roller | groundedAt = Nothing, walledAt = Nothing }


collideWithBlock : Block -> Roller -> Roller
collideWithBlock block roller =
    case collideWithBlockSides block roller of
        Nothing ->
            roller

        Just sides ->
            roller
                |> collideWithBlockTop block sides
                |> collideWithBlockLeft block sides
                |> collideWithBlockRight block sides


collideWithBlockTop : Block -> CL.RectanglesInfo -> Roller -> Roller
collideWithBlockTop block { top } roller =
    if not top then
        -- let
        --     _ =
        --         Debug.log "NOT TOP" ""
        -- in
        roller

    else if canClimbUp block roller then
        -- TODO: can we make the logic below work without sorting the blocks
        -- from lowest to highest?
        { roller | groundedAt = Just block.y }

    else
        -- let
        --     _ =
        --         Debug.log "CANNOT CLIMB" ""
        -- in
        roller


collideWithBlockLeft : Block -> CL.RectanglesInfo -> Roller -> Roller
collideWithBlockLeft block { left } roller =
    -- collideWithBlockLeft block { left, top } roller =
    if not left then
        roller

    else if canClimbUp block roller then
        -- { roller | groundedAt = groundOffEdge block top roller }
        { roller | groundedAt = Just block.y }

    else
        updateWalledAtWith (LeftX block.x) roller


collideWithBlockRight : Block -> CL.RectanglesInfo -> Roller -> Roller
collideWithBlockRight block { right } roller =
    -- collideWithBlockRight block { right, top } roller =
    if not right then
        roller

    else if canClimbUp block roller then
        -- { roller | groundedAt = groundOffEdge block top roller }
        { roller | groundedAt = Just block.y }

    else
        updateWalledAtWith (RightX <| block.x + block.width) roller


groundOffEdge : Block -> Bool -> Roller -> Maybe Float
groundOffEdge block onTop roller =
    -- TODO: handle case where ball hangs off one side?
    -- combine both steps?
    if not onTop then
        Nothing

    else if roller.x < block.x then
        let
            th =
                -- Debug.log "theta" <|
                atan2 (block.y - roller.y) (block.x - roller.x)
        in
        Just <| block.y + radius * (1 - sin th)

    else if roller.x > block.x + block.width then
        let
            th =
                -- Debug.log "theta" <|
                atan2 (block.y - roller.y) (block.x + block.width - roller.x)
        in
        Just <| block.y + radius * (1 - sin th)

    else
        Just <| block.y


updateWalledAtWith : WalledAt -> Roller -> Roller
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


canClimbUp : Block -> Roller -> Bool
canClimbUp block roller =
    case roller.groundedAt of
        Nothing ->
            roller.y + radius - block.y < canClimbUpThreshold

        Just groundedAt ->
            groundedAt - block.y < canClimbUpThreshold


canClimbUpThreshold : Float
canClimbUpThreshold =
    27


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
        , renderRollingBallWithEyes env roller
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
