{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

{-# HLINT ignore "Redundant pure" #-}
module Main
  ( main
  ) where

import           Control.Applicative    (Alternative ((<|>)))
import           Control.Monad          (void, zipWithM_)
import           Control.Monad.Accum    (MonadAccum (look, add), looks)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Bits              (xor)
import           Data.List              (elemIndex, findIndex)
import           Main.Trans.Accum       (AccumT (AccumT), execAccumT, runAccumT)
import           Text.Printf            (printf)

newtype Board =
  Board [Int]

instance Semigroup Board where
  (<>) :: Board -> Board -> Board
  (<>) (Board xs) (Board ys) = Board (zipWith (+) xs ys)

instance Monoid Board where
  mempty :: Board
  mempty = Board (repeat 0)

type Move = (Int, Int)

data Player
  = Computer
  | Human
  deriving (Show)

opponent :: Player -> Player
opponent Computer = Human
opponent Human    = Computer

nimSum :: Board -> Int
nimSum (Board rows) = foldl xor 0 rows

putBoard :: (MonadAccum Board m, MonadIO m) => m ()
putBoard = do
  Board rows <- look
  liftIO $ zipWithM_ fmt [1 ..] rows
  s <- looks nimSum
  liftIO $ printf "\x3A3 : %04b\n" s -- '\x3A3' = uppercase sigma
  where
    fmt :: Int -> Int -> IO ()
    fmt k n = printf "%d : %04b %s\n" k n (concat (replicate n "* "))

getMove :: (MonadAccum Board m, MonadIO m) => m Move
getMove = do
  liftIO $ putStr "Row? "
  k <- liftIO readLn
  liftIO $ putStr "How many? "
  m <- liftIO readLn
  valid <- validMove (k - 1, m)
  if valid
    then return (k - 1, m)
    else do
      liftIO $ putStrLn "** Invalid move. **\BEL"
      getMove

bestMove :: Board -> Move
bestMove (Board rows) =
  case count (> 1) rows of
    0 ->
      let Just k = elemIndex 1 rows
       in (k, 1)
    1 ->
      let Just k = findIndex (> 1) rows
          n = rows !! k
       in ( k
          , if odd (count (== 1) rows)
              then n
              else n - 1)
    _ ->
      let m = nimSum (Board rows)
          Just k =
            findIndex (\n -> n > (n `xor` m)) rows <|> findIndex (> 0) rows
          n = rows !! k
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

validMove :: MonadAccum Board m => Move -> m Bool
validMove (k, m) = do
  Board rows <- look
  return $ 0 <= k && k < length rows && 0 < m && m <= rows !! k

applyMove :: MonadAccum Board m => Move -> m ()
applyMove (k, m) = add $ Board $ replicate k 0 ++ [-m] ++ repeat 0

finished :: MonadAccum Board m => m Bool
finished = looks (\(Board rows) -> all (== 0) rows)

play ::
     (MonadIO m, MonadAccum Board m, MonadAccum Board m)
  => Player
  -> Move
  -> (Player -> m ())
  -> m ()
play p mv next = do
  applyMove mv
  f <- finished
  if f
    then do
      putBoard
      liftIO $ printf "** The winner is %s! **\BEL\n" $ show (opponent p)
    else next

playComputer :: (MonadAccum Board m, MonadIO m) => m ()
playComputer = do
  putBoard
  mv@(k, m) <- gets bestMove
  liftIO $ printf "Computer removes %d from row %d.\n" m (k + 1)
  play p mv playHuman

playHuman :: (MonadAccum Board m, MonadIO m) => m ()
playHuman = do
  putBoard
  liftIO $ printf "Human, enter your move:\n"
  mv <- getMove
  play p mv playComputer

main :: IO ()
main = void $ execAccumT playHuman $ Board [5, 4 .. 1]
