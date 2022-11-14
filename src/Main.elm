module Main exposing (main)

import App.Block exposing (Block)
import App.Enemy exposing (Enemy)
import App.Hero exposing (Hero)
import App.Lava exposing (Lava)
import App.Roller exposing (Laser)
import Browser exposing (Document)
import Browser.Events as BE
import Canvas as V
import Collision as CL
import Html as H
import Html.Attributes as At
import Keyboard as K



-- PROGRAM


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { -- game loop ticker
      -- TODO: can this be an integer instead?
      tick : Float

    -- keyboard
    , pressedKeys : List K.Key

    -- entities
    , hero : Hero
    , lasers : List Laser
    , enemy : Enemy

    -- obstacles
    , blocks : List Block
    , lava : List Lava

    -- flags
    , devMode : Bool
    }


initModel : Flags -> Model
initModel flags =
    { tick = 0
    , pressedKeys = []
    , hero = App.Hero.init
    , lasers = []
    , enemy = App.Enemy.init
    , blocks =
        [ App.Block.init 25 150 100 30
        , App.Block.init 175 300 100 30
        , App.Block.initMoving 275 285 50 30

        -- , App.Block.init 275 285 50 30
        , App.Block.init 325 310 50 30
        , App.Block.init 400 300 50 30
        , App.Block.initMoving 475 100 50 100
        , App.Block.init 475 300 50 100
        , App.Block.init -10 0 20 height
        , App.Block.init (width - 10) 0 20 height
        ]
    , lava =
        [ App.Lava.initMoving 0 400 width height
        ]
    , devMode = flags.devMode
    }



-- INITIALIZE


type alias Flags =
    { devMode : Bool }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( initModel flags, Cmd.none )



-- UPDATE


type Msg
    = Frame Float
    | GotKeyPress K.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Frame _ ->
            ( handleFrame model, Cmd.none )

        GotKeyPress keyMsg ->
            ( { model | pressedKeys = K.update keyMsg model.pressedKeys }, Cmd.none )


handleFrame : Model -> Model
handleFrame model =
    model
        |> updateHero
        |> updateEnemy
        |> updateLasers
        |> updateEnemyLaserCollisions
        |> updateBlocks
        |> incrementTick


updateHero : Model -> Model
updateHero model =
    { model | hero = App.Hero.update model model.hero }


updateLasers : Model -> Model
updateLasers model =
    { model
        | lasers =
            if model.hero.roller.firingLaser then
                App.Roller.eyeLasers model.hero.roller

            else
                []
    }


updateEnemy : Model -> Model
updateEnemy model =
    { model
        | enemy = App.Enemy.update model model.enemy
    }


updateEnemyLaserCollisions : Model -> Model
updateEnemyLaserCollisions model =
    -- TODO: refactor
    let
        lasers =
            List.map
                (\laser ->
                    let
                        line =
                            CL.toLineSegment laser.source laser.target

                        endPoint =
                            Maybe.withDefault (CL.toPoint laser.target) <|
                                CL.detectLineCircleInfo line (App.Roller.circle model.enemy.roller)
                    in
                    App.Roller.Laser ( line.x1, line.y1 ) ( endPoint.x, endPoint.y )
                )
                model.lasers

        newEnemy =
            List.foldl
                (\laser e ->
                    let
                        line =
                            CL.toLineSegment laser.source laser.target

                        roller =
                            e.roller
                    in
                    case CL.detectLineCircleInfo (CL.toLineSegment laser.source laser.target) (App.Roller.circle e.roller) of
                        Nothing ->
                            { e | roller = { roller | velX = 0 } }

                        Just actualTarget ->
                            { e
                                | roller =
                                    { roller
                                        | velX =
                                            e.roller.velX
                                                + (if line.x1 < e.roller.x then
                                                    0.5

                                                   else
                                                    -0.5
                                                  )
                                        , angle =
                                            -- TODO: simplify - maybe just rotate one way per side?
                                            e.roller.angle
                                                + (if line.x1 < e.roller.x then
                                                    if actualTarget.y < e.roller.y then
                                                        3

                                                    else
                                                        -3

                                                   else if actualTarget.y < e.roller.y then
                                                    -3

                                                   else
                                                    3
                                                  )
                                    }
                            }
                )
                (let
                    enemy =
                        model.enemy

                    roller =
                        enemy.roller
                 in
                 { enemy | roller = { roller | velX = 0 } }
                )
                model.lasers
    in
    { model | lasers = lasers, enemy = newEnemy }


updateBlocks : Model -> Model
updateBlocks model =
    { model
        | blocks = List.map App.Block.move model.blocks
        , lava = List.map App.Lava.move model.lava
    }


incrementTick : { a | tick : Float } -> { a | tick : Float }
incrementTick model =
    { model | tick = model.tick + 1 }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ BE.onAnimationFrameDelta Frame
        , Sub.map GotKeyPress K.subscriptions
        ]



-- VIEWS


view : Model -> Document Msg
view model =
    { title = "Platformer"
    , body = [ viewBody model ]
    }


viewBody : Model -> H.Html Msg
viewBody model =
    H.div
        [ At.style "display" "flex"
        , At.style "justify-content" "center"
        , At.style "align-items" "center"

        -- background
        , At.style "background-color" "rgba(0,0,0,0.1)"

        -- fill
        , At.style "height" "100vh"
        ]
        [ viewGame model ]


viewGame : Model -> H.Html Msg
viewGame model =
    V.toHtml
        ( width, height )
        [ At.style "height" (String.fromInt height ++ "px")
        , At.style "background-color" "#FFF"
        ]
        (render model)



-- RENDER


render : Model -> List V.Renderable
render model =
    List.concat
        [ [ V.clear ( 0, 0 ) width height
          ]
        , List.map App.Block.render model.blocks
        , List.map App.Lava.render model.lava
        , [ App.Hero.render model model.hero ]

        -- TODO: this is a hack - we render enemies after lasers so
        -- the laser appear behind them.
        , List.map App.Hero.renderLaser model.lasers
        , [ App.Enemy.render model model.enemy
          ]
        ]


width : number
width =
    640


height : number
height =
    480
