module GraphicTypes exposing (..)

type alias Point = {x : Int, y : Int}
type alias Dimension = {width : Int, height : Int}
type alias LineConnective shape = { shape | middle : Point }
type alias Circle = LineConnective {cx : Int, cy : Int, radius : Int}
type alias Rectangle = LineConnective {x : Int, y : Int, width : Int, height : Int}
type alias Color = String