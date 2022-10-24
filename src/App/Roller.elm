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
    }


init : Float -> Roller
init height =
    { x = 25
    , y = height * 0.75 - radius
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


applyKeyboardInputs : Env a -> Roller -> Roller
applyKeyboardInputs env roller =
    let
        { x } =
            KA.arrows env.pressedKeys
    in
    if x /= 0 then
        { roller | x = roller.x + toFloat x * accelerationX }

    else
        roller


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
        [ V.circle ( roller.x, roller.y ) radius
        , V.path ( roller.x, roller.y + (0.75 * radius) )
            [ V.lineTo ( roller.x, roller.y + radius )
            ]
        ]
