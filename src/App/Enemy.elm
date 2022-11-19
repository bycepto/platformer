module App.Enemy exposing
    ( Enemy
    , applyLaser
    , init
    , render
    , update
    )

import App.Block exposing (Block)
import App.Laser exposing (Laser)
import App.Lava exposing (Lava)
import App.Roller exposing (Roller)
import Canvas as V
import Collision as CL
import Color



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
    }


type alias Enemy =
    { roller : Roller
    }


init : Enemy
init =
    { roller =
        App.Roller.init
            |> (\r ->
                    { r
                        | x = 400
                        , y = 25
                        , facingLeft = True
                        , angle = 180
                    }
               )
    }



-- UPDATE


update : Env a -> Room b -> Enemy -> Enemy
update env room enemy =
    { enemy | roller = updateRoller env room enemy.roller }


updateRoller : Env a -> Room b -> Roller -> Roller
updateRoller env room roller =
    roller
        |> App.Roller.update env room


applyLaser : Laser -> Enemy -> Enemy
applyLaser laser enemy =
    let
        line =
            CL.toLineSegment laser.source laser.target

        circle =
            case App.Roller.circle enemy.roller of
                CL.Circle c ->
                    c
    in
    case CL.detectLineCircleInfo line circle of
        Nothing ->
            enemy

        Just ( { x1, y2 }, _ ) ->
            { enemy | roller = App.Roller.pushFrom x1 y2 enemy.roller }



-- RENDER


render : Env a -> Enemy -> V.Renderable
render env enemy =
    App.Roller.render
        env
        { bodyColor = Color.black
        , eyeColor = Color.green
        , debug = False
        }
        enemy.roller
