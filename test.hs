import System.IO

spoj1 :: Int -> IO ()
spoj1 x = if x /= 42
          then putStrLn (show x) >> main
          else return ()
    
main = do
  x <- getLine
  let x' = read x :: Int
  spoj1 x'
