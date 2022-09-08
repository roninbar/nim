  module Main
  ( main
  ) where

import           Control.Monad (zipWithM_)
import           Data.Bits     (xor)
import           Data.List     (elemIndex, findIndex)
import           Text.Printf   (printf)

type Board = [Int]

type Move = (Int, Int)

data Player = Alice | Bob deriving Show

opponent :: Player -> Player
opponent Alice = Bob
opponent Bob   = Alice

nimSum :: Board -> Int
nimSum = foldl xor 0

putBoard :: Board -> IO ()
putBoard b = do
  zipWithM_ fmt [1 ..] b
  printf "\x3A3 : %04b\n" (nimSum b) -- '\x3A3' = uppercase sigma

 where
  fmt :: Int -> Int -> IO ()
  fmt k n = printf "%d : %04b %s\n" k n (replicate n '*')

getMove :: IO Move
getMove = do
  putStr "Row? "
  k <- readLn
  putStr "How many? "
  m <- readLn
  return (k - 1, m)

bestMove :: Board -> Move
bestMove b = case count (> 1) b of
  0 -> let Just k = elemIndex 1 b in (k, 1)
  1 ->
    let Just k = findIndex (> 1) b
    in  (k, if odd (count (== 1) b) then b !! k else b !! k - 1)
  _ ->
    let m      = nimSum b
        Just k = findIndex (\n -> n `xor` m < n) b
    in  (k, b !! k - (b !! k `xor` m))
  where count pred xs = sum [ if pred x then 1 else 0 | x <- xs ]

validMove :: Move -> Board -> Bool
validMove (k, m) b = 0 <= k && k < length b && m <= b !! k

applyMove :: Move -> Board -> Board
applyMove (k, m) b = take k b ++ [max 0 (b !! k - m)] ++ drop (k + 1) b
-- applyMove (k, m) s = foldMap ($ s) [take k, \s -> [s !! k - m], drop (k + 1)]

finished :: Board -> Bool
finished = all (== 0)

playComputer :: Player -> Board -> IO ()
playComputer p b = do
  putBoard b
  let (k, m) = bestMove b
  printf "%s removes %d from row %d.\n" (show p) m (k + 1)
  let s' = applyMove (k, m) b
  if finished s'
    then do
      putBoard s'
      printf "** The winner is %s! **\BEL\n" (show (opponent p))
    else playHuman (opponent p) s'

playHuman :: Player -> Board -> IO ()
playHuman p b = do
  putBoard b
  printf "%s, enter your move:\n" (show p)
  (k, m) <- getMove
  if validMove (k, m) b
    then do
      let s' = applyMove (k, m) b
      if finished s'
        then do
          putBoard s'
          printf "** The winner is %s **\BEL\n" (show (opponent p))
        else playComputer (opponent p) s'
    else do
      putStrLn "** Invalid move. **\BEL"
      playHuman p b

main :: IO ()
main = playComputer Alice [5, 4 .. 1]
