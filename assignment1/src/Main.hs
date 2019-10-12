module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import System.Environment
import Text.Read (readMaybe)

data Cell = Alive | Dead deriving (Eq)

data SimulationState = SimulationState {
  cells :: [[Cell]],
  curX :: Int,
  curY :: Int,
  width :: Int,
  height :: Int
}

initSimulation w h = SimulationState {
  cells = replicate h . replicate w $ Dead,
  curX = 0,
  curY = 0,
  width = w,
  height = h
}

drawWorld s@SimulationState{ curX = x, curY = y, cells = css, width = w, height = h } =
  let
    drawLoop i j ((v : vs) : vss) =
      Translate (20 * fromIntegral i - fromIntegral (w - 1) * 10) (20 * fromIntegral j - fromIntegral (h - 1) * 10) (
        color (case (v, i - x, j - y) of
          (Alive, 0, 0) -> dark blue
          (Dead, 0, 0) -> light blue
          (Alive, _, _) -> black
          (Dead, _, _) -> white) $ rectangleSolid 18 18) :
      drawLoop (i + 1) j (vs : vss)
    drawLoop _ j ([] : vss) = drawLoop 0 (j + 1) vss
    drawLoop _ _ [] = []
  in
    Color black $
    Pictures $
    drawLoop 0 0 css

instance Num a => Num [a] where
  a + b = uncurry (+) <$> zip a b
  a * b = uncurry (*) <$> zip a b
  a - b = uncurry (-) <$> zip a b
  abs a = abs <$> a
  signum a = signum <$> a
  fromInteger = repeat . fromInteger



handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) s@SimulationState{ curX = x, width = w } = s{ curX = max 0 $ x - 1 }
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) s@SimulationState{ curX = x, width = w } = s{ curX = min (w - 1) $ x + 1 }
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) s@SimulationState{ curY = y, height = h } = s{ curY = max 0 $ y - 1 }
handleEvent (EventKey (SpecialKey KeyUp) Down _ _) s@SimulationState{ curY = y, height = h } = s{ curY = min (h - 1) $ y + 1 }
handleEvent (EventKey (Char 'x') Down _ _) s@SimulationState{ curY = y, curX = x, cells = css } =
  let
    change :: Int -> Int -> [[Cell]] -> [[Cell]]
    change 0 0 ((v : vs) : vss) = (((if v == Alive then Dead else Alive) : vs) : vss)
    change i 0 ((v : vs) : vss) =
      let (vs' : vss') = change (i - 1) 0 (vs : vss)
      in (v : vs') : vss'
    change i j (vs : vss) = vs : change i (j - 1) vss
  in s{ cells = change x y css }
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) s@SimulationState{ cells = css } =
  let
    rreverse (vs:vss) = rreverse vss ++ [reverse vs]
    rreverse [] = []
    expanded [vs] = (Dead : vs) : [Dead : map (const Dead) vs]
    expanded (vs:vss) = (Dead : vs) : expanded vss
    toNums = (\vs -> (\v -> if v == Alive then 1 else 0) <$> vs) <$> (rreverse . expanded . rreverse . expanded) css
    neigs =
      toNums +
      tail toNums +
      drop 2 toNums +
      
      map tail toNums +
      --tail (map tail toNums) +
      drop 2 (map tail toNums) +

      map (drop 2) toNums +
      tail (map (drop 2) toNums) +
      drop 2 (map (drop 2) toNums)

    toCells ((v : vs) : vss) ((n : ns) : nss) = ((case (v, n)  of 
      (Alive, 2) -> Alive
      (_, 3) -> Alive
      _ -> Dead) : vs') : vss'
      where vs' : vss' = toCells (vs:vss) (ns:nss)
    toCells ([] : vss) ([] : nss) = [] : toCells vss nss
    toCells [] [] = []
  in s{ cells = toCells css neigs}
handleEvent _ n = n

updateWorld _ = id

window :: Int -> Int -> Display
window w h = InWindow "Game of Life" (w * 20, h * 20) (0, 0)

main = do
  args <- getArgs
  let 
    (width, height) = case args of
        [arg_w, arg_h] -> case (readMaybe arg_w :: Maybe Int, readMaybe arg_h :: Maybe Int) of
          (Just 0, _) -> error "the width cannot be 0"
          (_, Just 0) -> error "the height cannot be 0"
          (Just w, Just h) -> (w, h)
          _ -> error "Invalid format"
        [] -> (20, 20)
        _ -> error "Invalid count of arguments"
  play (window width height) (greyN 0.5) 25 (initSimulation width height) drawWorld handleEvent updateWorld