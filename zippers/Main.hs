module Main (main) where
import Data.List

players = 419
marbles = 7216400

solushen = maximum $ map sum $ transpose $ map (take players . map fst) $ takeWhile (not . null) $ iterate (drop players) $ play marbles

main = print solushen

play marbles = run (Ring [] [0]) [1..marbles] where
  initial = Ring [] [0]
  run ring [] = []
  run ring (m:ms) = (score, ring'):run ring' ms where
    (score, ring') = addMarble ring m

addMarble ring marble = (score + score', ring'') where
  twenty3 = marble `rem` 23 == 0
  score
    | twenty3   = marble
    | otherwise = 0
  ring' = put (right $ right ring) marble
  (score', ring'')
    | twenty3 = get $ left $ left $ left $ left $ left $ left $ left ring
    | otherwise = (0, ring')


data Ring = Ring [Int] [Int]
  deriving Show

left (Ring [] []) = Ring [] []
left (Ring [] rs) = left $ Ring (reverse rs) []
left (Ring (l:ls) rs) = Ring ls (l:rs)

right (Ring [] []) = Ring [] []
right (Ring ls []) = right $ Ring [] (reverse ls)
right (Ring ls (r:rs)) = Ring (r:ls) rs

get (Ring [] []) = error "get"
get (Ring ls []) = get $ Ring [] $ reverse ls
get (Ring ls (r:rs)) = (r, Ring ls rs)

put (Ring ls rs) x = Ring ls (x:rs)
