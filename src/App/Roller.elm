module App.Roller exposing (Roller, init, render, update)

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
    }


type alias Roller =
    { x : Float
    , y : Float
    , velX : Float
    , velY : Float
    }


init : Float -> Roller
init height =
    { x = 75

    -- , y = height * 0.75 - radius
    , y = 25
    , velX = 0

    -- bootleg gravity
    , velY = 1
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



-- UPDATE


update : Env a -> Roller -> Roller
update env roller =
    roller
        |> applyKeyboardInputs env
        |> applyPhysics


applyKeyboardInputs : Env a -> Roller -> Roller
applyKeyboardInputs env roller =
    let
        { x } =
            KA.arrows env.pressedKeys

        speedModifier =
            if List.member K.Shift env.pressedKeys then
                0.5

            else
                1
    in
    if x /= 0 then
        { roller | velX = toFloat x * accelerationX * speedModifier }

    else
        { roller | velX = 0 }


applyPhysics : Roller -> Roller
applyPhysics roller =
    { roller
        | x = roller.x + roller.velX
        , y = roller.y + roller.velY
    }


accelerationX : Float
accelerationX =
    3.0



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
