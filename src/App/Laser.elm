module App.Laser exposing
    ( Laser
    , init
    , render
    )

import Canvas as V
import Canvas.Settings as VS
import Canvas.Settings.Advanced as VA
import Color



-- MODEL


type alias Laser =
    { source : V.Point
    , target : V.Point
    }



-- RENDER


init : Float -> V.Point -> Laser
init angle ( x, y ) =
    { source = ( x, y )
    , target =
        Tuple.mapBoth
            ((+) x)
            ((+) y)
        <|
            fromPolar ( defaultLength, degrees angle )
    }


defaultLength : Float
defaultLength =
    1000



-- RENDER


render : Laser -> V.Renderable
render { source, target } =
    let
        ( x1, y1 ) =
            source

        ( x2, y2 ) =
            target
    in
    V.shapes
        [ VA.shadow { blur = 5, color = Color.red, offset = ( 0, 0 ) }
        , VS.stroke Color.red
        ]
        [ V.path ( x1, y1 )
            [ V.lineTo ( x2, y2 ) ]
        ]
