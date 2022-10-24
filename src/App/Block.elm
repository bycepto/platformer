module App.Block exposing (Block, init, render)

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


init : Block
init =
    { x = 25
    , y = 350
    , width = 100
    , height = 100
    }



-- RENDER


render : Block -> V.Renderable
render block =
    V.shapes
        [ VS.fill Color.white
        , VS.stroke Color.black
        ]
        [ V.rect ( block.x, block.y ) block.height block.width
        ]
