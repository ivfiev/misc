import Data.List
import qualified Data.IntMap.Strict as Map

players = 419
marbles = 7216400

main = print $ maximum $ Map.elems $ play Map.empty (Ring [] [0]) 1 1

play scores ring player marble
  | player > players = play scores ring 1 marble
  | marble > marbles = scores
  | otherwise = play scores' ring' (player + 1) (marble + 1)
    where
      scores'
        | score > 0 = Map.insertWith (+) player score scores
        | otherwise = scores
      (score, ring') = addMarble ring marble

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
