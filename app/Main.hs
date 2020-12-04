module Main where

import qualified Data.Vector as V
import Data.Function

parseGrid :: String -> (V.Vector Char, Int, Int)
parseGrid contents =
  let lines' = lines contents
      width = length $ head lines'
      height = length lines'
   in (V.fromList $ foldr (++) "" $ lines', width, height)

solve :: V.Vector Char -> Int -> Int -> Int
solve grid width height = zip [0, 2..] [0, 1..(height - 1)]
  & map (\(x, y) -> y * width + x)
  & map (\idx -> grid `cyclicAccess` idx)
  & filter (=='#')
  & length

cyclicAccess :: V.Vector a -> Int -> a
cyclicAccess vec idx = vec V.! (idx `mod` V.length vec)

main :: IO ()
main = do
  contents <- getContents
  let (grid, width, height) = parseGrid contents
   in print $ show $ solve grid width height
