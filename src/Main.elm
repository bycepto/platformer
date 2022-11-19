module Main exposing (main)

import App.Hero
import App.Room exposing (Room)
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
      -- TODO: can this be an integer instead?
      tick : Float

    -- keyboard
    , pressedKeys : List K.Key

    -- scene
    , room : Room

    -- flags
    , devMode : Bool
    }


initModel : Flags -> Model
initModel flags =
    { tick = 0
    , pressedKeys = []
    , room = App.Room.init App.Hero.init
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
        |> updateRoom
        |> incrementTick


updateRoom : Model -> Model
updateRoom model =
    { model | room = App.Room.update model model.room }


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
    , App.Room.render model model.room
    ]


width : number
width =
    640


height : number
height =
    480
