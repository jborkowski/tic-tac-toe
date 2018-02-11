module Logic where

import Game

import Data.Array
import Data.Foldable ( asum )

import Graphics.Gloss.Interface.Pure.Game

isCoordCorrect = inRange((0,0), (n - 1, n - 1))

swithPlayer game =
  case gamePlayer game of
    PlayerX -> game { gamePlayer = PlayerO }
    PlayerO -> game { gamePlayer = PlayerX }

-- playerWon :: Player -> Board -> Bool
-- playerWon player board = any isVictoryProj projs
--   where projs = allRowsCoords
--                ++ allColumnCoords
--                ++ allDiagCoords
--         allRowsCoords   = [[(i,j) | j <- [0 .. n - 1]] | i <- [0 .. n - 1]]
--         allColumnCoords = [[(j,i) | j <- [0 .. n - 1]] | i <- [0 .. n - 1]]
--         allDiagCoords   = [[(i,i) | i <- [0 .. n - 1]]
--                           , [(i, j) | i <- [0 .. n - 1], let j = n-1-i]
--                           ]
--         isVictoryProj proj = (n ==)
--                              $ length
--                              $ filter (\cell -> cell == Full player)
--                              $ map (\coord -> board ! coord) proj

countCells :: Cell -> Board -> Int
countCells cell = length . filter((==) cell) . elems


full :: [Cell] -> Maybe Player
full (cell@(Just player):cells) | all (== cell) cells = Just player
full _                                                = Nothing

winner :: Board -> Maybe Player
winner board = asum $ map full $ rows ++ cols ++ diags
    where rows  = [[board ! (i,j) | i <- [0..n-1]] | j <- [0..n-1]]
          cols  = [[board ! (j,i) | i <- [0..n-1]] | j <- [0..n-1]]
          diags = [[board ! (i,i) | i <- [0..n-1]]
                  ,[board ! (i,j) | i <- [0..n-1], let j = n-1-i ]]
                  
checkGameOver game
    | Just p <- winner board =
        game { gameState = GameOver $ Just p }
    | countCells Nothing board == 0 =
        game { gameState = GameOver Nothing }
    | otherwise = game
    where board = gameBoard game


-- checkGameOver game
--   | playerWon PlayerX board =
--     game { gameState = GameOver $ Just PlayerX }
--   | playerWon PlayerO board =
--     game { gameState = GameOver $ Just PlayerO }
--   | playerWon Empty (board == 0) =
--     game { gameState = GameOver $ Nothing }
--   | otherwise = game
--   where board = gameBoard game

playerTurn :: Game -> (Int, Int) -> Game
playerTurn game cellCoord
  | isCoordCorrect cellCoord && board ! cellCoord == Nothing =
    checkGameOver $ swithPlayer $ game { gameBoard = (board // [(cellCoord, Just player)])}
  | otherwise = game
  where board = gameBoard game
        player = gamePlayer game

mousePosAsCellCoord :: (Float, Float) -> (Int, Int)
mousePosAsCellCoord (x, y) = ( floor ((y + (fromIntegral screenHeight * 0.5)) / fromIntegral cellHeight)
                             , floor ((x + (fromIntegral screenWidth * 0.5 )) / fromIntegral cellWidth)
                             )  

transformGame (EventKey (MouseButton LeftButton) Up _ mousePos) game =
  case gameState game of
    Running -> playerTurn game $ mousePosAsCellCoord mousePos
    GameOver _ -> initialGame
transformGame _  game = game
