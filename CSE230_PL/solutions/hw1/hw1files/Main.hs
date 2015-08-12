module Main where 

import XMLTypes
import Play
import Mine

firstDiff :: Eq a => [a] -> [a] -> Maybe ([a],[a])
firstDiff [] [] = Nothing
firstDiff (c:cs) (d:ds) 
     | c==d = firstDiff cs ds 
     | otherwise = Just (c:cs, d:ds)
firstDiff cs ds = Just (cs,ds)

testResults :: String -> String -> IO ()
testResults file1 file2 = do 
  f1 <- readFile file1
  f2 <- readFile file2
  case firstDiff f1 f2 of
    Nothing -> putStr "Success!\n"
    Just (cs,ds) -> do
      putStr "Results differ: '"
      putStr (take 20 cs)
      putStr "' vs '"
      putStr (take 20 ds)
      putStr "'\n"

main :: IO () 
main = 
  do putStr "Converting... "
     writeFile "dream.html" (xml2string (formatPlay play))
     testResults "dream.html" "sample.html"
