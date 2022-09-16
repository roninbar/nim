  module Main
  ( main
  ) where

import           Control.Applicative (Alternative ((<|>)))
import           Control.Monad       (zipWithM_)
import           Data.Bits           (xor)
import           Data.List           (elemIndex, findIndex)
import           Text.Printf         (printf)

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
  fmt k n = printf "%d : %04b %s\n" k n (concat (replicate n "* "))

getMove :: Board -> IO Move
getMove b = do
  putStr "Row? "
  k <- readLn
  putStr "How many? "
  m <- readLn
  if validMove b (k - 1, m)
    then return (k - 1, m)
    else do
      putStrLn "** Invalid move. **\BEL"
      getMove b

bestMove :: Board -> Move
bestMove b = case count (> 1) b of
  0 -> let Just k = elemIndex 1 b in (k, 1)
  1 ->
    let Just k = findIndex (> 1) b
        n      = b !! k
    in  (k, if odd (count (== 1) b) then n else n - 1)
  _ ->
    let m      = nimSum b
        Just k = findIndex (\n -> n > (n `xor` m)) b <|> findIndex (> 0) b
        n      = b !! k
    in  (k, n - (n `xor` m) |+| 1)
  where count pred xs = sum [ if pred x then 1 else 0 | x <- xs ]

infix 0 |+|
(|+|) :: (Ord a, Num a) => a -> a -> a
a |+| b = if a > 0 then a else b

validMove :: Board -> Move -> Bool
validMove b (k, m) = 0 <= k && k < length b && 0 < m && m <= b !! k

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
  play (k, m) b
 where
  play :: Move -> Board -> IO ()
  play v b = do
    let b' = applyMove v b
    if finished b'
      then do
        putBoard b'
        printf "** The winner is %s! **\BEL\n" (show (opponent p))
      else playHuman (opponent p) b'

playHuman :: Player -> Board -> IO ()
playHuman p b = do
  putBoard b
  printf "%s, enter your move:\n" (show p)
  v <- getMove b
  play v b
 where
  play :: Move -> Board -> IO ()
  play v b = do
    let b' = applyMove v b
    if finished b'
      then do
        putBoard b'
        printf "** The winner is %s! **\BEL\n" (show (opponent p))
      else playComputer (opponent p) b'

main :: IO ()
main = playHuman Bob [5, 4 .. 1]
