module App.Enemy exposing
    ( Enemy
    , applyLaser
    , init
    , render
    , setX
    , update
    )

import App.Block exposing (Block)
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
    }


type alias Enemy =
    { roller : Roller
    }


init : Float -> Enemy
init x =
    { roller =
        App.Roller.init
            |> (\r ->
                    { r
                        | x = x
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
