module App.Laser exposing
    ( Laser
    , applyFirstCollision
    , applyFirstCollisionWithEntity
    , init
    , render
    )

import Canvas as V
import Canvas.Settings as VS
import Canvas.Settings.Advanced as VA
import Collision as CL
import Color



-- MODEL


type alias Laser =
    { source : V.Point
    , target : V.Point
    }


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


line : Laser -> CL.LineSegment
line { source, target } =
    CL.toLineSegment source target



-- UPDATE


{-| Fire a laser at a list of entities and find the first entity that the laser
collides with along it's path, and return the resulting laser and that entity.

If there is not collision, return the original laser and nothing.

This function also requires a function that maps entities to shapes to detect
collisions.

-}
applyFirstCollisionWithEntity : (a -> CL.Shape) -> List a -> Laser -> ( Laser, Maybe a )
applyFirstCollisionWithEntity toShape entities laser =
    List.foldl
        (\entity ( laserBeforeHit, target ) ->
            case applyCollision (toShape entity) laserBeforeHit of
                Nothing ->
                    ( laserBeforeHit, target )

                Just laserAfterHit ->
                    ( laserAfterHit, Just entity )
        )
        ( laser, Nothing )
        entities


applyFirstCollision : List CL.Shape -> Laser -> Maybe Laser
applyFirstCollision shapes laser =
    List.foldl
        (Maybe.andThen << applyCollision)
        (Just laser)
        shapes


applyCollision : CL.Shape -> Laser -> Maybe Laser
applyCollision shape laser =
    case shape of
        CL.Circle circle ->
            circle
                |> CL.detectLineCircleInfo (line laser)
                |> Maybe.map Tuple.first
                |> Maybe.map (\{ x1, y1, x2, y2 } -> Laser ( x1, y1 ) ( x2, y2 ))



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
