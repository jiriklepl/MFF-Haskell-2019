module Main where

import Control.Monad
import Data.HashMap.Lazy hiding (foldl')
import Data.HashSet
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import System.Environment
import Text.Read (readMaybe)

data Cell
  = Alive
  | Dead
  deriving (Eq)

data SimulationState =
  SimulationState
    { cells :: HashSet (Int, Int)
    , curX :: Int
    , curY :: Int
    , width :: Int
    , height :: Int
    }

initSimulation w h =
  SimulationState
    {cells = Data.HashSet.empty, curX = 0, curY = 0, width = w, height = h}

drawWorld s@SimulationState { curX = x
                            , curY = y
                            , cells = css
                            , width = w
                            , height = h
                            } =
  let draw c (i, j) =
        Translate
          (20 * fromIntegral i - fromIntegral (w - 1) * 10)
          (20 * fromIntegral j - fromIntegral (h - 1) * 10)
          (c (rectangleSolid 18 18))
   in Color black $
      Pictures $
      (draw (color white) <$> liftM2 (,) [0 .. w - 1] [0 .. h - 1]) ++
      (draw (color black) <$> Data.HashSet.toList css) ++
      [draw (color red) (x, y)]

handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) s@SimulationState { curX = x
                                                                       , width = w
                                                                       } =
  s {curX = max 0 $ x - 1}
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) s@SimulationState { curX = x
                                                                        , width = w
                                                                        } =
  s {curX = min (w - 1) $ x + 1}
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) s@SimulationState { curY = y
                                                                       , height = h
                                                                       } =
  s {curY = max 0 $ y - 1}
handleEvent (EventKey (SpecialKey KeyUp) Down _ _) s@SimulationState { curY = y
                                                                     , height = h
                                                                     } =
  s {curY = min (h - 1) $ y + 1}
handleEvent (EventKey (Char 'x') Down _ _) s@SimulationState { curY = y
                                                             , curX = x
                                                             , cells = css
                                                             } =
  s
    { cells =
        if Data.HashSet.member (x, y) css
          then Data.HashSet.delete (x, y) css
          else Data.HashSet.insert (x, y) css
    }
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) s@SimulationState { cells = css
                                                                        , width = w
                                                                        , height = h
                                                                        } =
  let addNeigh (x, y) =
        alter
          (\v ->
             case v of
               Just a -> Just (a + 1)
               Nothing -> Just (1 :: Int))
          (x, y)
      addNeighs m (x, y) =
        Prelude.foldr
          addNeigh
          m
          (Prelude.filter
             (/= (x, y))
             (liftM2 (,) [x - 1, x, x + 1] [y - 1, y, y + 1]))
      neighbors =
        foldl' addNeighs (Data.HashMap.Lazy.empty :: HashMap (Int, Int) Int) css
   in s
        { cells =
            keysSet
              (filterWithKey
                 (\k v ->
                    case (k, v) of
                      ((x, y), 3) -> x >= 0 && x < w && y >= 0 && y < h
                      (loc, 2) -> Data.HashSet.member loc css
                      _ -> False)
                 neighbors)
        }
handleEvent _ n = n

updateWorld _ = id

window :: Int -> Int -> Display
window w h = InWindow "Game of Life" (w * 20, h * 20) (0, 0)

main = do
  args <- getArgs
  let (width, height) =
        case args of
          [arg_w, arg_h] ->
            case (readMaybe arg_w :: Maybe Int, readMaybe arg_h :: Maybe Int) of
              (Just 0, _) -> error "the width cannot be 0"
              (_, Just 0) -> error "the height cannot be 0"
              (Just w, Just h) -> (w, h)
              _ -> error "Invalid format"
          [] -> (20, 20)
          _ -> error "Invalid count of arguments"
  play
    (window width height)
    (greyN 0.5)
    25
    (initSimulation width height)
    drawWorld
    handleEvent
    updateWorld
