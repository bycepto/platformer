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
    { position : V.Point
    }


init : Float -> Roller
init height =
    { position = initPosition height
    }


initPosition : Float -> V.Point
initPosition height =
    ( 25, height * 0.75 - radius )


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
        { roller
            | position =
                Tuple.mapFirst
                    (\posX -> posX + toFloat x * accelerationX)
                    roller.position
        }

    else
        roller


accelerationX : Float
accelerationX =
    3.0



-- RENDER


render : Roller -> V.Renderable
render model =
    let
        ( cx, cy ) =
            model.position
    in
    V.shapes
        [ VS.fill Color.white
        , VS.stroke Color.black

        -- rotate
        , VA.transform
            [ VA.translate cx cy
            , VA.rotate (rotation <| Tuple.first model.position)
            , VA.translate -cx -cy
            ]
        ]
        [ V.circle ( cx, cy ) radius
        , V.path ( cx, cy + (0.75 * radius) )
            [ V.lineTo ( cx, cy + radius )
            ]
        ]
