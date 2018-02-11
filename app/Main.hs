module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Logic
import Game
import Rendering

window =  InWindow "Functional" (640, 480) (100, 100)

backgroundColor = makeColor 0 0 0 255

-- gameAsPicture _ = Blank


main :: IO ()
main = play window backgroundColor 30 initialGame gameAsPicture transformGame (const id) -- instead (\_ -> id)
