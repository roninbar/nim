module Main
  ( main
  ) where

import           Control.Monad                  ( zipWithM_ )
import           Data.Bits                      ( xor )
import           Data.List                      ( findIndex )
import           Text.Printf                    ( printf )

type State = [Int]

type Move = (Int, Int)

data Player = Alice | Bob deriving Show

opponent :: Player -> Player
opponent Alice = Bob
opponent Bob   = Alice

nimSum :: State -> Int
nimSum = foldl xor 0

putState :: State -> IO ()
putState s = do
  zipWithM_ fmt [1 ..] s
  printf "\x3A3 : %04b\n" (nimSum s) -- '\x3A3' = uppercase sigma
 where
  fmt :: Int -> Int -> IO ()
  fmt k n = printf "%d : %04b %s\n" k n (replicate n '*')
  -- fmt = flip (flip (printf "%d : %s") . flip replicate '*')

getMove :: IO Move
getMove = do
  putStr "Row? "
  k <- readLn
  putStr "How many? "
  m <- readLn
  return (k - 1, m)

winningMove :: State -> Move
winningMove s = (k, s !! k - (s !! k `xor` m)) where
  m      = foldl xor 0 s
  Just k = findIndex (\n -> n `xor` m < n) s
  
validMove :: Move -> State -> Bool
validMove (k, m) s = 0 <= k && k < length s -- && m <= s !! k

applyMove :: Move -> State -> State
applyMove (k, m) s = take k s ++ [max 0 (s !! k - m)] ++ drop (k + 1) s
-- applyMove (k, m) s = foldMap ($ s) [take k, \s -> [s !! k - m], drop (k + 1)]

finished :: State -> Bool
finished = all (== 0)

playComputer :: Player -> State -> IO ()
playComputer p s = do
  putState s
  let (k, m) = winningMove s
  printf "%s removes %d from row %d.\n" (show p) m (k + 1)
  let s' = applyMove (k, m) s
  if finished s'
    then do
      putState s'
      printf "** The winner is %s! **\BEL\n" (show p)
    else playHuman (opponent p) s'

playHuman :: Player -> State -> IO ()
playHuman p s = do
  putState s
  printf "%s, enter your move:\n" (show p)
  (k, m) <- getMove
  if validMove (k, m) s
    then do
      let s' = applyMove (k, m) s
      if finished s'
        then do
          putState s'
          printf "** The winner is %s **\BEL\n" (show p)
        else playComputer (opponent p) s'
    else do
      putStrLn "** Invalid move. **\BEL"
      playHuman p s

main :: IO ()
main = playComputer Alice [5, 4 .. 1]
