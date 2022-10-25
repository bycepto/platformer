module App.Block exposing (Block, boundingBox, init, render)

import App.Collisions exposing (BoundingBox)
import Canvas as V
import Canvas.Settings as VS
import Color



-- MODEL


type alias Block =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


init : Float -> Float -> Float -> Float -> Block
init x y width height =
    { x = x
    , y = y
    , width = width
    , height = height
    }


boundingBox : Block -> BoundingBox
boundingBox =
    identity



-- RENDER


render : Block -> V.Renderable
render block =
    V.shapes
        [ VS.fill Color.white
        , VS.stroke Color.grey
        ]
        [ V.rect ( block.x, block.y ) block.width block.height
        ]
