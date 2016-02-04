module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Pure.Simulate
import Graphics.Gloss.Interface.Pure.Display
import System.Random
import System.Exit


type CoordinateList	= [Point]
type Direction = Point

data Area   = Area  CoordinateList
data Snake  = Snake CoordinateList Direction
data Apple  = Apple Point

data Game   = Started Area Snake Apple | Ended | Exit


-- initial game state
initGame :: Game
initGame = Started
            (Area [(x,y) | x <- [-260,-240..260], y <- [-280,-260..240]])
            (Snake [(x,20) | x <- take 3 [0,20..]] (-20,0)) -- Snake has three parts and moves to the left
            $ Apple (0,-20) -- the apple is at (0,-20)

		
-- animates the game
doAction :: Float -> (Game -> Game)
doAction _ Ended = Ended
doAction timeStep (Started (Area ga) (Snake (snakehead:snakebody) direction) (Apple appleposition))
    | snakehead `notElem` ga = Ended
    | snakehead `elem` snakebody = Ended
    | snakehead == appleposition = Started (Area ga) (Snake snakeparts' direction) $ Apple appleposition'
    | otherwise = Started (Area ga) (Snake moveSnake direction) $ Apple appleposition
	where
		-- move the snake
		moveSnake :: CoordinateList
		moveSnake = snakehead + direction : init (snakehead : snakebody)
		
		-- grow the snake with three parts when it eats an apple
		snakeparts' :: CoordinateList
		snakeparts' = snakehead : snakebody ++ take 3 (repeat $ last snakebody)
		
		-- when the snake eats an apple, select a new random position for the apple to get a "new" apple
		appleposition' :: Point
		appleposition' = randomPosition $ filter (`notElem` snakeparts') ga
		

-- chooses a random element from a list
randomPosition :: CoordinateList -> Point
randomPosition coordList = coordList !! fst (randomR(0, length coordList - 1) (mkStdGen $ 7 * partSum `mod` 11))
	where
		partSum = round $ sum $ map fst coordList


-- draws the graphics
drawGame :: Game -> Picture
drawGame Ended = pictures [t1, t2]
	where
		t1 = scale 0.3 0.3 . translate (-400) 0 . color red . text $ "Snake died!"
		t2 = scale 0.2 0.2 . translate (-800) (-150) . color red . text $ "press Space to restart"
drawGame (Started ga (Snake parts direction) (Apple (ax,ay))) = pictures [snakelength, borders, apple, snake]
	where
		snakelength = scale 0.2 0.2 . translate (-1370) 1320 . color red . text $ "Snake length: " ++ show (length parts)
		borders = color blue . pictures $ [translate 0 (-20) . rectangleWire 544 $ 544]
		apple = color red . pictures $ [translate ax ay . circle $ 10]
		snake = pictures [translate sx sy . color white . rectangleWire 20 $ 20 | (sx,sy) <- parts]
	

-- keypress handling
handleEvents :: Event -> Game -> Game
handleEvents (EventKey (SpecialKey KeySpace) Down _ _) Ended = initGame
handleEvents (EventKey (SpecialKey KeyEsc) Down _ _) Ended = Exit
handleEvents (EventKey button Down _ _)
	(Started ga (Snake snakeparts direction) apple)
	= Started ga (Snake snakeparts direction') apple
		where
		direction'
			| button == SpecialKey KeyRight && snd direction /= 0 = (20,0)
			| button == SpecialKey KeyLeft && snd direction /= 0 = (-20,0)
			| button == SpecialKey KeyUp && fst direction /= 0 = (0,20)
			| button == SpecialKey KeyDown && fst direction /= 0 = (0,-20)
			| otherwise = direction
handleEvents _ boring = boring -- other than the specfied keypresses do not affect the game


main = 	play
	(InWindow "LambdaSnake" (560, 600) (20, 20))
	black
	24
	initGame
	drawGame
	handleEvents
	doAction