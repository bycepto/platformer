module Main exposing (main)

-- import Canvas.Settings.Text as VW
-- import Canvas.Texture as VT

import App.Roller exposing (Roller)
import Browser exposing (Document)
import Browser.Events as BE
import Canvas as V
import Canvas.Settings as VS
import Color
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

    -- roller
    , roller : Roller
    }


initModel : Model
initModel =
    { tick = 0
    , pressedKeys = []
    , roller = App.Roller.init height
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
        [ V.clear ( 0, 0 ) width height
        , renderSquare model
        ]


renderSquare : Model -> V.Renderable
renderSquare model =
    V.group
        []
        [ renderFloor
        , App.Roller.render model.roller
        ]


renderFloor : V.Renderable
renderFloor =
    V.shapes
        [ VS.fill Color.grey ]
        [ V.rect ( 0, height * 0.75 ) width 1 ]


width : number
width =
    640


height : number
height =
    480
