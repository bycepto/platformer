module App.Hero exposing
    ( Hero
    , init
    , render
    , renderLaser
    , update
    )

import App.Block exposing (Block)
import App.Laser exposing (Laser)
import App.Lava exposing (Lava)
import App.Roller exposing (Roller)
import Canvas as V
import Color
import Keyboard as K
import Keyboard.Arrows as KA



-- MODEL


type alias Env a =
    { a
        | pressedKeys : List K.Key
        , tick : Float
        , devMode : Bool
    }


type alias Room a =
    { a
        | blocks : List Block
        , lava : List Lava
    }


type alias Hero =
    { roller : Roller
    }


init : Hero
init =
    { roller = App.Roller.init
    }



-- UPDATE


update : Env a -> Room b -> Hero -> Hero
update env room hero =
    { hero | roller = updateRoller env room hero.roller }


updateRoller : Env a -> Room b -> Roller -> Roller
updateRoller env room roller =
    roller
        |> applyKeyboardInputs env
        |> App.Roller.update env room


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


accelerationX : Float
accelerationX =
    3.0


degreesPerMove : Float
degreesPerMove =
    2.0


applyKeyboardInputsLaser : Env a -> Roller -> Roller
applyKeyboardInputsLaser { pressedKeys } roller =
    { roller | firingLaser = List.member K.Spacebar pressedKeys }



-- RENDER


render : Env a -> Hero -> V.Renderable
render env hero =
    App.Roller.render
        env
        { bodyColor = Color.white
        , eyeColor = Color.white
        , debug = env.devMode
        }
        hero.roller


renderLaser : Laser -> V.Renderable
renderLaser laser =
    App.Laser.render laser
