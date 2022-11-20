module App.Enemy exposing
    ( Enemy
    , applyLaser
    , render
    , setX
    , spawn
    , update
    )

import App.Block exposing (Block, Slope)
import App.Laser exposing (Laser)
import App.Lava exposing (Lava)
import App.Roller exposing (Roller)
import Canvas as V
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
        , slopes : List Slope
    }


type alias Enemy =
    { roller : Roller
    }


spawn : Float -> Float -> Enemy
spawn x y =
    { roller =
        App.Roller.init
            |> (\r ->
                    { r
                        | x = x
                        , y = y
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
applyLaser { source, target } enemy =
    let
        ( x1, _ ) =
            source

        ( _, y2 ) =
            target
    in
    { enemy | roller = App.Roller.pushFrom x1 y2 enemy.roller }


setX : Float -> Enemy -> Enemy
setX x enemy =
    let
        roller =
            enemy.roller
    in
    { enemy | roller = { roller | x = x } }



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
