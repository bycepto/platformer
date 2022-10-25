module Main exposing (main)

import App.Block exposing (Block)
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
    , blocks : List Block
    }


initModel : Model
initModel =
    { tick = 0
    , pressedKeys = []
    , roller = App.Roller.init
    , blocks =
        [ App.Block.init 25 150 100 30
        , App.Block.init 175 300 100 30
        , App.Block.init 275 285 50 30
        , App.Block.init 325 310 50 30
        , App.Block.init 400 300 50 30
        , App.Block.init 475 100 50 300
        ]
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
        |> updateRoller
        |> incrementTick


updateRoller : Model -> Model
updateRoller model =
    { model | roller = App.Roller.update model model.roller }


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
          , App.Roller.render model model.roller
          ]
        , List.map App.Block.render model.blocks
        ]


width : number
width =
    640


height : number
height =
    480
