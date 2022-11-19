module App.Room exposing (Room, init, render, update)

import App.Block exposing (Block)
import App.Enemy exposing (Enemy)
import App.Hero exposing (Hero)
import App.Laser exposing (Laser)
import App.Lava exposing (Lava)
import App.Roller
import Canvas as V
import Keyboard as K



-- MODEL


type alias Env a =
    { a
        | pressedKeys : List K.Key
        , tick : Float
        , devMode : Bool
    }


type Room
    = Room
        { -- entities
          hero : Hero
        , lasers : List Laser
        , enemy : Maybe Enemy

        -- obstacles
        , blocks : List Block
        , lava : List Lava

        -- exits
        , left : Maybe (Hero -> Room)
        , right : Maybe (Hero -> Room)
        }


init : Hero -> Room
init =
    room1


room1 : Hero -> Room
room1 hero =
    Room
        { hero = hero
        , lasers = []
        , enemy = Just App.Enemy.init
        , blocks =
            [ App.Block.init 25 150 100 30
            , App.Block.init 175 300 100 30
            , App.Block.initMoving 275 285 50 30
            , App.Block.init 325 310 50 30
            , App.Block.init 400 300 50 30
            , App.Block.initMoving 475 100 50 100
            , App.Block.init 475 300 50 100
            , App.Block.init -10 0 20 height
            , App.Block.init 525 300 1000 10
            ]
        , lava =
            [ App.Lava.initMoving 0 400 width height
            ]
        , left = Nothing
        , right = Just room2
        }


room2 : Hero -> Room
room2 hero =
    Room
        { hero = hero
        , lasers = []
        , enemy = Nothing
        , blocks =
            [ App.Block.init 0 300 1000 10
            ]
        , lava =
            [ App.Lava.initMoving 0 400 width height
            ]
        , left = Just room1
        , right = Nothing
        }



-- UPDATE


update : Env a -> Room -> Room
update env room =
    handleFrame env room


handleFrame : Env a -> Room -> Room
handleFrame env room =
    room
        |> updateHero env
        |> updateLasers
        |> updateEnemyLaserCollisions
        |> updateEnemy env
        |> updateBlocks
        |> changeRoom


changeRoom : Room -> Room
changeRoom (Room room) =
    let
        hero =
            room.hero

        roller =
            room.hero.roller
    in
    if roller.x > width then
        case room.right of
            Nothing ->
                Room { room | hero = { roller = { roller | x = width } } }

            Just toRoom ->
                case toRoom { hero | roller = { roller | x = 0 } } of
                    Room newRoom ->
                        -- persist the state of the room you are leaving
                        Room
                            { newRoom | left = Just (\h -> Room { room | hero = h }) }

    else if roller.x < 0 then
        case room.left of
            Nothing ->
                Room { room | hero = { roller = { roller | x = 0 } } }

            Just toRoom ->
                case toRoom { hero | roller = { roller | x = width } } of
                    Room newRoom ->
                        -- persist the state of the room you are leaving
                        Room
                            { newRoom | right = Just (\h -> Room { room | hero = h }) }

    else
        Room room


updateHero : Env a -> Room -> Room
updateHero env (Room room) =
    Room { room | hero = App.Hero.update env room room.hero }


updateLasers : Room -> Room
updateLasers (Room room) =
    Room
        { room
            | lasers =
                if room.hero.roller.firingLaser then
                    List.map
                        (App.Laser.init room.hero.roller.angle)
                        (App.Roller.eyes room.hero.roller)

                else
                    []
        }


updateEnemy : Env a -> Room -> Room
updateEnemy env (Room room) =
    Room <|
        case room.enemy of
            Just e ->
                { room
                    | enemy = Just <| App.Enemy.update env room e
                }

            Nothing ->
                room


updateEnemyLaserCollisions : Room -> Room
updateEnemyLaserCollisions (Room room) =
    -- TODO: refactor
    let
        -- V1
        rollers =
            case room.enemy of
                Nothing ->
                    []

                Just enemy ->
                    [ App.Roller.circle enemy.roller ]

        lasers =
            List.map
                (\laser -> Maybe.withDefault laser <| App.Laser.applyFirstCollision rollers laser)
                room.lasers

        -- V2
        enemies =
            room.enemy
                |> Maybe.map List.singleton
                |> Maybe.withDefault []

        collisions =
            List.map
                (\laser -> App.Laser.applyFirstCollisionWithEntity (App.Roller.circle << .roller) enemies laser)
                room.lasers

        newEnemy =
            room.enemy
                |> Maybe.map
                    (\enemy ->
                        List.foldl
                            App.Enemy.applyLaser
                            { enemy | roller = App.Roller.stopX enemy.roller }
                            room.lasers
                    )
    in
    Room { room | lasers = lasers, enemy = newEnemy }


updateBlocks : Room -> Room
updateBlocks (Room room) =
    Room
        { room
            | blocks = List.map App.Block.move room.blocks
            , lava = List.map App.Lava.move room.lava
        }



-- RENDER


render : Env a -> Room -> V.Renderable
render env (Room room) =
    V.group
        []
    <|
        List.concat
            [ List.map App.Block.render room.blocks
            , List.map App.Lava.render room.lava
            , [ App.Hero.render env room.hero ]

            -- TODO: this is a hack - we render enemies after lasers so
            -- the laser appear behind them.
            , List.map App.Hero.renderLaser room.lasers
            , case room.enemy of
                Nothing ->
                    []

                Just enemy ->
                    [ App.Enemy.render env enemy ]
            ]


width : number
width =
    640


height : number
height =
    480
