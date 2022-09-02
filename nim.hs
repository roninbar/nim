module Main
  ( main
  ) where

import           Text.Printf (printf)

type State = [Int]

type Move = (Int, Int)

putState :: State -> IO ()
putState = mapM_ putStrLn . zipWith fmt [1 ..]
 where
  fmt :: Int -> Int -> String
  fmt k n = printf "%d : %s" k (replicate n '*')

getMove :: IO Move
getMove = do
  putStr "Row? "
  s <- getLine
  let k = read s :: Int
  putStr "How many? "
  s <- getLine
  let m = read s :: Int
  return (k - 1, m)

validMove :: Move -> State -> Bool
validMove (k, m) s = 0 <= k && k < length s && m <= s !! k

applyMove :: Move -> State -> State
applyMove (k, m) s = take k s ++ [s !! k - m] ++ drop (k + 1) s

finished :: State -> Bool
finished = all (== 0)

play :: State -> IO ()
play s = do putState s
            (k, m) <- getMove
            if validMove (k, m) s then do
              let s' = applyMove (k, m) s
              if finished s' then do
                putState s'
                putStrLn "** Game over. **\BEL"
              else play s'
            else do
              putStrLn "** Invalid move. **\BEL"
              play s

main :: IO ()
main = play [5, 4 .. 1]
