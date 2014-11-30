-- Times out in SPOJ: As the algorithm is extremely inefficient.
-- Will revisit this later.
-- Relevant discussion: https://www.haskell.org/pipermail/beginners/2012-January/009369.html

import Control.Monad (replicateM)
import Data.List (intersperse)    

hasFactor :: Integral a => a -> Bool
hasFactor x = any (== 0) $ map (\y -> x `mod` y) num
    where num = [2..x-1]

primeNo :: Integral a => a -> a -> [a]                
primeNo x y = filter (not . hasFactor) num
    where num = [x..y]

splitAtSpace :: String -> (String, String)
splitAtSpace ys = aux ([],[]) ys
    where
      aux acc [] = acc
      aux acc (x:xs) = if (x == ' ')
                       then (fst acc, xs)
                       else aux ((fst acc) ++ [x], []) xs 

toIntPair :: (String, String) -> (Int, Int)
toIntPair (x,y) = (read x, read y)            
                            
getInput :: [String] -> [(Int, Int)]
getInput xs = map (toIntPair . splitAtSpace) xs

getPrimeNo :: [(Int, Int)] -> [[String]]
getPrimeNo xs = map (\(x,y) -> map show $ primeNo x y) xs             
              
main = do
  x <- getLine
  let x' = read x :: Int
  inp <- replicateM x' getLine
  let inp' = getInput inp
  let output = concat $ intersperse [""] $ getPrimeNo inp'
  mapM_ putStrLn output
