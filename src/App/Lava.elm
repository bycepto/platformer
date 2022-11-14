module App.Lava exposing
    ( Lava
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


type alias Lava =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    , movement : Maybe Movement
    }


type Movement
    = Vertical Float Float Bool


init : Float -> Float -> Float -> Float -> Lava
init x y width height =
    { x = x
    , y = y
    , width = width
    , height = height
    , movement = Nothing
    }


initMoving : Float -> Float -> Float -> Float -> Lava
initMoving x y width height =
    { x = x
    , y = y
    , width = width
    , height = height
    , movement = Just <| Vertical (y - 3) (y + 3) True
    }


boundingBox : Lava -> CL.Rectangle
boundingBox { x, y, width, height } =
    { x = x
    , y = y
    , width = width
    , height = height
    }



-- UPDATE


move : Lava -> Lava
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
    0.1



-- RENDER


render : Lava -> V.Renderable
render block =
    V.group
        []
        [ V.shapes
            [ VS.fill Color.yellow
            , VS.stroke Color.yellow
            ]
            [ V.rect ( block.x, block.y ) block.width block.height
            ]
        , V.shapes
            [ VS.fill Color.lightOrange
            , VS.stroke Color.lightOrange
            ]
            [ V.rect ( block.x, block.y + 5 ) block.width (block.height - 5)
            ]
        , V.shapes
            [ VS.fill Color.orange
            , VS.stroke Color.orange
            ]
            [ V.rect ( block.x, block.y + 15 ) block.width (block.height - 15)
            ]
        , V.shapes
            [ VS.fill Color.darkOrange
            , VS.stroke Color.darkOrange
            ]
            [ V.rect ( block.x, block.y + 30 ) block.width (block.height - 30)
            ]
        , V.shapes
            [ VS.fill Color.orange
            , VS.stroke Color.orange
            ]
          <|
            List.filterMap identity <|
                List.indexedMap
                    (\i x ->
                        if modBy 10 i == 0 then
                            Just <|
                                V.rect
                                    ( block.x + toFloat x
                                    , block.y
                                        + 30
                                        + (if modBy 20 i == 0 then
                                            15

                                           else
                                            5
                                          )
                                    )
                                    1
                                    1

                        else
                            Nothing
                    )
                    (List.range (ceiling block.x) (floor (block.x + block.width)))
        ]
