module Main
  ( main
  ) where

import           Control.Monad                  ( zipWithM_ )
import           Text.Printf                    ( printf )

type State = [Int]

type Move = (Int, Int)

data Player = Alice | Bob deriving Show

opponent :: Player -> Player
opponent Alice = Bob
opponent Bob   = Alice

putState :: State -> IO ()
putState = zipWithM_ fmt [1 ..]
 where
  fmt :: Int -> Int -> IO ()
  fmt k n = printf "%d : %s\n" k (replicate n '*')
  -- fmt = flip (flip (printf "%d : %s") . flip replicate '*')

getMove :: IO Move
getMove = do
  putStr "Row? "
  k <- readLn
  putStr "How many? "
  m <- readLn
  return (k - 1, m)

validMove :: Move -> State -> Bool
validMove (k, m) s = 0 <= k && k < length s -- && m <= s !! k

applyMove :: Move -> State -> State
applyMove (k, m) s = take k s ++ [max 0 (s !! k - m)] ++ drop (k + 1) s
-- applyMove (k, m) s = foldMap ($ s) [take k, \s -> [s !! k - m], drop (k + 1)]

finished :: State -> Bool
finished = all (== 0)

play :: Player -> State -> IO ()
play p s = do
  putState s
  printf "%s, enter your move:\n" (show p)
  (k, m) <- getMove
  if validMove (k, m) s
    then do
      let s' = applyMove (k, m) s
      if finished s'
        then do
          putState s'
          printf "** The winner is %s **\BEL\n" (show (opponent p))
        else play (opponent p) s'
    else do
      putStrLn "** Invalid move. **\BEL"
      play p s

main :: IO ()
main = play Alice [5, 4 .. 1]
