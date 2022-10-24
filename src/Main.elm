module Main exposing (main)

import App.Block exposing (Block)
import App.Collisions
import App.Roller exposing (Roller)
import Browser exposing (Document)
import Browser.Events as BE
import Canvas as V
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
      tick : Float

    -- keyboard
    , pressedKeys : List K.Key

    -- entities
    , roller : Roller
    , block : Block
    }


initModel : Model
initModel =
    { tick = 0
    , pressedKeys = []
    , roller = App.Roller.init height
    , block = App.Block.init
    }



-- INITIALIZE


type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( initModel, Cmd.none )



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
        |> updateRollerInputs
        |> handleCollisions
        |> updateRollerPhysics
        |> incrementTick


handleCollisions : Model -> Model
handleCollisions model =
    let
        roller =
            model.roller

        rollerBB =
            App.Roller.boundingBox roller

        blockBB =
            App.Block.boundingBox model.block
    in
    case App.Collisions.collision rollerBB blockBB of
        Nothing ->
            -- bootleg gravity
            { model | roller = { roller | velY = 1 } }

        Just { left, right, top, bottom } ->
            let
                sides_ =
                    Debug.log "sides" { left = left, right = right, top = top, bottom = bottom }

                newRoller =
                    { roller
                        | velX =
                            if left && not top then
                                min roller.velX 0

                            else if right && not top then
                                max roller.velX 0

                            else
                                roller.velX
                        , velY =
                            if top then
                                min roller.velY 0

                            else if bottom then
                                max roller.velY 0

                            else
                                roller.velY
                    }
            in
            { model | roller = newRoller }


updateRollerInputs : Model -> Model
updateRollerInputs model =
    { model | roller = App.Roller.applyKeyboardInputs model model.roller }


updateRollerPhysics : Model -> Model
updateRollerPhysics model =
    { model | roller = App.Roller.applyPhysics model.roller }


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
    [ V.clear ( 0, 0 ) width height
    , App.Roller.render model.roller
    , App.Block.render model.block
    ]


width : number
width =
    640


height : number
height =
    480
