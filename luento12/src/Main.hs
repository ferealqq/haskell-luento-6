module Main where

maksimiA :: forall a. Ord a => [a] -> Maybe a
maksimiA [] = Nothing
maksimiA (eka : loput) = Just (maksimiApu eka loput)

maksimiApu :: forall a. Ord a => a -> [a] -> a
maksimiApu suurinTähänMennessä [] = suurinTähänMennessä
maksimiApu !suurinTähänMennessä (eka : loput) = maksimiApu (max suurinTähänMennessä eka) loput

--maksimiAB :: forall a. Ord a => [a] -> Maybe a

keskiarvo :: [Double] -> Maybe Double
keskiarvo [] = Nothing
keskiarvo (eka : loput) = Just (keskiarvoApu (eka, 1) loput)
  where
    keskiarvoApu :: (Double, Double) -> [Double] -> Double
    keskiarvoApu (x, xs) [] = x / xs
    keskiarvoApu (x, xs) (ekaB : loputB) = keskiarvoApu ((x + ekaB), (xs + 1)) loputB

main :: IO ()
main = do
  putStrLn "Hello TIEA341"
