module App.Enemy exposing
    ( Enemy
    , init
    , render
    , update
    )

import App.Block exposing (Block)
import App.Lava exposing (Lava)
import App.Roller exposing (Roller)
import Canvas as V
import Color



-- MODEL


type alias Env a =
    { a
        | blocks : List Block
        , lava : List Lava
        , tick : Float
        , devMode : Bool
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


update : Env a -> Enemy -> Enemy
update env enemy =
    { enemy | roller = updateRoller env enemy.roller }


updateRoller : Env a -> Roller -> Roller
updateRoller env roller =
    roller
        |> App.Roller.update env



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
