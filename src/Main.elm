module Main exposing (main)

-- import Canvas.Settings.Advanced as VA
-- import Canvas.Settings.Text as VW
-- import Canvas.Texture as VT

import Browser exposing (Document)
import Canvas as V
import Canvas.Settings as VS
import Color
import Html as H
import Html.Attributes as At



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
    ()



-- INITIALIZE


type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( (), Cmd.none )



-- UPDATE


type alias Msg =
    ()


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEWS


view : Model -> Document Msg
view model =
    { title = "Platformer"
    , body = [ viewBody model ]
    }


viewBody : Model -> H.Html Msg
viewBody _ =
    H.div
        [ At.style "display" "flex"
        , At.style "justify-content" "center"
        , At.style "align-items" "center"

        -- background
        , At.style "background-color" "rgba(0,0,0,0.1)"

        -- fill
        , At.style "height" "100vh"
        ]
        [ viewGame ]


viewGame : H.Html Msg
viewGame =
    V.toHtml
        ( width, height )
        [ At.style "height" (String.fromInt height ++ "px")
        , At.style "background-color" "#FFF"
        ]
        [ V.clear ( 0, 0 ) width height
        , renderSquare
        ]


renderSquare : V.Renderable
renderSquare =
    V.shapes
        [ VS.fill (Color.rgba 0 0 0 1) ]
        [ V.rect ( 0, 0 ) 100 50 ]


width : number
width =
    640


height : number
height =
    480
