import System.Directory.Internal.Prelude (getArgs)
import Data.List (partition, find, sort)
import Data.Map (Map, (!))
import Data.Map qualified as Map
import Text.Printf (printf)

main :: IO ()
main = do
  [fileName] <- getArgs
  input <- map words . lines <$> readFile fileName
  let (vars, []:rules) = break null input
  let cache = foldr (\[x,y] -> Map.insert (init x) (read y)) Map.empty vars
  let zs = sort [var | [_,_,_,_, var@('z':_)] <- rules]
  let cache' = foldr ((snd .) . flip (eval rules)) cache zs
  let part1 = foldr ((. (2 *)) . (+)) 0 [cache' ! z | z <- zs]
  printf "Part 1: [%d]\n" part1

eval :: [[String]] -> Map String Int -> String -> (Int, Map String Int)
eval rules cache var 
  | Map.member var cache = (cache ! var, cache)
  | otherwise = (val, cache''') where
    Just [var1, op, var2, "->", _] = find ((== var) . last) rules
    (val1, cache') = eval rules cache var1
    (val2, cache'') = eval rules cache' var2
    val = exec op val1 val2
    cache''' = Map.insert var val cache''

exec "AND" x y = fromEnum $ x == 1 && y == 1
exec "OR"  x y = fromEnum $ x == 1 || y == 1
exec "XOR" x y = fromEnum $ x /= y