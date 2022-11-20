module App.Room exposing (Room, init, render, update)

import App.Block exposing (Block, Slope)
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
        , enemies : List Enemy

        -- obstacles
        , blocks : List Block
        , lava : List Lava
        , slopes : List Slope

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
        , enemies = [ App.Enemy.init 300 ]
        , blocks =
            [ App.Block.init 25 150 100 30
            , App.Block.init 175 280 100 30
            , App.Block.initMoving 275 265 50 30
            , App.Block.init 400 300 50 30
            , App.Block.init 475 75 50 100
            , App.Block.initMoving 475 300 50 100
            , App.Block.init 525 300 1000 10
            ]
        , lava =
            [ App.Lava.initMoving 0 400 width height
            ]
        , slopes =
            [ -- HACK: extend the slope slightly into the block to prevent the
              -- roller from briefly dropping between the block and slock
              Slope (125 - 10) (150 + 2) 475 75
            ]
        , left = Nothing
        , right = Just room2
        }


room2 : Hero -> Room
room2 hero =
    Room
        { hero = hero
        , lasers = []
        , enemies = [ App.Enemy.init 300, App.Enemy.init 500 ]
        , blocks =
            [ App.Block.init 0 300 1000 10
            ]
        , lava =
            [ App.Lava.initMoving 0 400 width height
            ]
        , slopes = []
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
        |> updateEnemies env
        |> updateBlocks
        |> changeRoomHero
        |> changeRoomEnemies


changeRoomHero : Room -> Room
changeRoomHero (Room room) =
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


changeRoomEnemies : Room -> Room
changeRoomEnemies (Room room) =
    -- TODO: refactor
    let
        ( goingLeft, notGoingLeft ) =
            List.partition (\enemy -> enemy.roller.x < 0) room.enemies

        ( goingRight, staying ) =
            List.partition (\enemy -> enemy.roller.x > width) notGoingLeft
    in
    Room
        { room
            | enemies =
                List.concat
                    [ staying
                    , case room.left of
                        Nothing ->
                            List.map (App.Enemy.setX 0) goingLeft

                        Just _ ->
                            []
                    , case room.right of
                        Nothing ->
                            List.map (App.Enemy.setX width) goingRight

                        Just _ ->
                            []
                    ]
            , left =
                room.left
                    |> Maybe.map
                        (\toRoom ->
                            \h ->
                                case toRoom h of
                                    Room nextRoom ->
                                        Room
                                            { nextRoom
                                                | enemies = List.map (App.Enemy.setX (width - nextRoomBuffer)) goingLeft ++ nextRoom.enemies
                                            }
                        )
            , right =
                room.right
                    |> Maybe.map
                        (\toRoom ->
                            \h ->
                                case toRoom h of
                                    Room nextRoom ->
                                        Room
                                            { nextRoom
                                                | enemies = List.map (App.Enemy.setX nextRoomBuffer) goingRight ++ nextRoom.enemies
                                            }
                        )
        }


nextRoomBuffer : Float
nextRoomBuffer =
    100


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


updateEnemies : Env a -> Room -> Room
updateEnemies env (Room room) =
    Room <|
        { room
            | enemies = List.map (App.Enemy.update env room) room.enemies
        }


updateEnemyLaserCollisions : Room -> Room
updateEnemyLaserCollisions (Room room) =
    let
        collisions =
            enemyLaserCollisions (Room room)

        newEnemies =
            room.enemies
                |> List.map
                    (\enemy ->
                        collisions
                            |> List.filter (\( _, target ) -> target == Just enemy)
                            |> List.head
                            |> Maybe.map
                                (\( { source, target }, _ ) ->
                                    let
                                        ( x1, _ ) =
                                            source

                                        ( _, y2 ) =
                                            target
                                    in
                                    { enemy | roller = App.Roller.pushFrom x1 y2 enemy.roller }
                                )
                            |> Maybe.withDefault { enemy | roller = App.Roller.stopX enemy.roller }
                    )

        newLasers =
            collisions |> List.unzip |> Tuple.first
    in
    Room { room | lasers = newLasers, enemies = newEnemies }


enemyLaserCollisions : Room -> List ( Laser, Maybe Enemy )
enemyLaserCollisions (Room room) =
    List.map
        (App.Laser.applyFirstCollisionWithEntity (App.Roller.circle << .roller) room.enemies)
        room.lasers


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
            [ List.map App.Block.renderSlope room.slopes
            , List.map App.Block.render room.blocks
            , List.map App.Lava.render room.lava
            , [ App.Hero.render env room.hero ]

            -- TODO: this is a hack - we render enemies after lasers so
            -- the laser appear behind them.
            , List.map App.Hero.renderLaser room.lasers
            , List.map (App.Enemy.render env) room.enemies
            ]


width : number
width =
    640


height : number
height =
    480
