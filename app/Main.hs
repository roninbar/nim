{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

{-# HLINT ignore "Redundant pure" #-}
module Main
  ( main
  ) where

import           Control.Applicative (Alternative ((<|>)))
import           Control.Monad       (zipWithM_)
import           Control.Monad.State (MonadIO (liftIO), MonadState (get, put),
                                      MonadTrans (lift), State,
                                      StateT (runStateT), gets, void, zipWithM_)
import           Data.Bits           (xor)
import           Data.List           (elemIndex, findIndex)
import           Text.Printf         (printf)

type Board = [Int]

type Move = (Int, Int)

type S = State Board

type ST = StateT Board

data Player
  = Alice
  | Bob
  deriving (Show)

opponent :: Player -> Player
opponent Alice = Bob
opponent Bob   = Alice

nimSum :: Board -> Int
nimSum = foldl xor 0

putBoard :: (MonadState Board m, MonadIO m) => m ()
putBoard = do
  b <- get
  liftIO $ zipWithM_ fmt [1 ..] b
  s <- gets nimSum
  liftIO $ printf "\x3A3 : %04b\n" s -- '\x3A3' = uppercase sigma
  where
    fmt :: Int -> Int -> IO ()
    fmt k n = printf "%d : %04b %s\n" k n (concat (replicate n "* "))

getMove :: (MonadState Board m, MonadIO m) => m Move
getMove = do
  liftIO $ putStr "Row? "
  k <- liftIO readLn
  liftIO $ putStr "How many? "
  m <- liftIO readLn
  v <- validMove (k - 1, m)
  if v
    then return (k - 1, m)
    else do
      liftIO $ putStrLn "** Invalid move. **\BEL"
      getMove

bestMove :: Board -> Move
bestMove b =
  case count (> 1) b of
    0 ->
      let Just k = elemIndex 1 b
       in (k, 1)
    1 ->
      let Just k = findIndex (> 1) b
          n = b !! k
       in ( k
          , if odd (count (== 1) b)
              then n
              else n - 1)
    _ ->
      let m = nimSum b
          Just k = findIndex (\n -> n > (n `xor` m)) b <|> findIndex (> 0) b
          n = b !! k
       in (k, n - (n `xor` m) |+| 1)
  where
    count pred xs =
      sum
        [ if pred x
          then 1
          else 0
        | x <- xs
        ]

infix 0 |+|

(|+|) :: (Ord a, Num a) => a -> a -> a
a |+| b =
  if a > 0
    then a
    else b

validMove :: MonadState Board m => Move -> m Bool
validMove (k, m) = do
  b <- get
  return $ 0 <= k && k < length b && 0 < m && m <= b !! k

applyMove :: MonadState Board m => Move -> m ()
applyMove (k, m) = do
  b <- get
  put $ take k b ++ [max 0 (b !! k - m)] ++ drop (k + 1) b

-- applyMove (k, m) s = foldMap ($ s) [take k, \s -> [s !! k - m], drop (k + 1)]
finished :: MonadState Board m => m Bool
finished = gets (all (== 0))

play ::
     (MonadIO m, MonadState Board m)
  => (Player -> m ())
  -> Player
  -> Move
  -> m ()
play next p v = do
  applyMove v
  f <- finished
  if f
    then do
      putBoard
      liftIO $ printf "** The winner is %s! **\BEL\n" $ show (opponent p)
    else next (opponent p)

playComputer :: (MonadState Board m, MonadIO m) => Player -> m ()
playComputer p = do
  putBoard
  (k, m) <- gets bestMove
  liftIO $ printf "%s removes %d from row %d.\n" (show p) m (k + 1)
  play playHuman p (k, m)

playHuman :: (MonadState Board m, MonadIO m) => Player -> m ()
playHuman p = do
  putBoard
  liftIO $ printf "%s, enter your move:\n" (show p)
  (k, m) <- getMove
  play playComputer p (k, m)

main :: IO ()
main = void $ runStateT (playHuman Bob) [5,4 .. 1]
