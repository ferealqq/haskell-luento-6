{-# LANGUAGE TypeApplications #-}

module Main where

-- forall a. a -> a
mäppää :: forall a b. (a -> b) -> [a] -> [b]
mäppää _ [] = []
mäppää funktio (eka : loput) = [(funktio eka)] ++ (mäppää funktio loput)

suodata :: forall a. (a -> Bool) -> [a] -> [a]
suodata _ [] = []
suodata funktio (eka : loput) = case (funktio eka) of
  True -> [eka] ++ (suodata funktio loput)
  False -> suodata funktio loput

main :: IO ()
main = do
  putStrLn "Hello TIEA341"
