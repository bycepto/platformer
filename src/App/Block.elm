module App.Block exposing
    ( Block
    , boundingBox
    , init
    , initMoving
    , move
    , render
    )

import Canvas as V
import Canvas.Settings as VS
import Collision as CL
import Color



-- MODEL


type alias Block =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    , movement : Maybe Movement
    }


type Movement
    = Vertical Float Float Bool


init : Float -> Float -> Float -> Float -> Block
init x y width height =
    { x = x
    , y = y
    , width = width
    , height = height
    , movement = Nothing
    }


initMoving : Float -> Float -> Float -> Float -> Block
initMoving x y width height =
    { x = x
    , y = y
    , width = width
    , height = height
    , movement = Just <| Vertical (y - 10) (y + 10) True
    }


boundingBox : Block -> CL.Rectangle
boundingBox { x, y, width, height } =
    { x = x
    , y = y
    , width = width
    , height = height
    }



-- UPDATE


move : Block -> Block
move block =
    case block.movement of
        Nothing ->
            block

        Just (Vertical minY maxY goingDown) ->
            if goingDown && block.y >= maxY then
                { block | movement = Just <| Vertical minY maxY False }

            else if not goingDown && block.y <= minY then
                { block | movement = Just <| Vertical minY maxY True }

            else
                { block
                    | y =
                        block.y
                            + moveStep
                            * (if goingDown then
                                1.0

                               else
                                -1.0
                              )
                }


{-| How fast a block moves
-}
moveStep : Float
moveStep =
    -- TODO: make this a parameter
    0.2



-- RENDER


render : Block -> V.Renderable
render block =
    V.shapes
        [ VS.fill Color.white
        , VS.stroke Color.grey
        ]
        [ V.rect ( block.x, block.y ) block.width block.height
        ]
