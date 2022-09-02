module Main where

type State = [Int]
type Move = (Int, Int)

putState :: State -> IO ()
putState = mapM_ print

getMove :: IO Move
-- TODO: Implement `getMove`
getMove = do
  putStr "Row? "
  s <- getLine
  let r = read s :: Int
  putStr "How many? "
  s <- getLine
  let m = read s :: Int
  return (r, m)

applyMove :: Move -> State -> State
-- TODO: Implement `applyMove`
applyMove (k, m) s = [ if i == k then n - m else n | (i, n) <- zip [1 ..] s ]

finished :: State -> Bool
finished = all (== 0)

play :: State -> IO ()
play s = do
  putState s
  (r, m) <- getMove
  let s' = applyMove (r, m) s in if finished s' then return () else play s'

main :: IO ()
main = play [5, 4 .. 1]

