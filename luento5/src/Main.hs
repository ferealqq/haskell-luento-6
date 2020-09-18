module Main where

import MaybeTeht

--laske :: (a->a) -> [a] -> a
--laske funktio (eka:loput) = (funktio eka)++(laske funktio loput) 

--lausekeTesti :: (Natural -> Natural -> Natural) -> Natural -> [Natural] -> Natural 

lausekeTesti :: (Natural -> Natural -> Natural) -> Natural -> [Natural] -> [Natural] 
lausekeTesti funktio numero lista = map (\x -> funktio x numero) lista

kutsuParia :: forall a b. (a, a -> b) -> b
kutsuParia sePari 
  = case sePari of
    (seA,seFunktio) -> seFunktio seA

molemmille :: forall a t. (a -> t) -> (a, a) -> (t, t)
molemmille seFunktio sePari = (seFunktio (fst sePari), seFunktio (snd sePari))

-- let skaalaa kerroin = molemmille (\x -> x * kerroin :: Int)
-- :t skaalaa
--    skaalaa :: Int -> (Int, Int) -> (Int, Int)

kutsuNKertaa :: forall a. Natural -> (a -> a) -> (a -> a)
kutsuNKertaa n f = case n of 
                    0 -> (\se -> se)
                    m -> let 
                          nmYks = kutsuNKertaa (m-1) f
                         in (\se -> nmYks (f se))

-- let neliöjuuri alku x = kutsuNKertaa 10 (\arvaus -> ((x :: Double) - x/arvaus)/2) alku
-- let neliöjuuri alku x = kutsuNKertaa 10 (\arvaus -> ((x :: Double) - x/arvaus)/2) alku

-- let plussa x y = (kutsuNKertaa x succ) y

-- plussaa 1 (4 :: Natural)
--  5
-- plussaa 1 'a'
--  'f'

-- Int -> Int -> Int -> Int

summa :: Int -> Int -> Int -> Int
summa x y z = x + y + z 

summaB :: Int -> Int -> (Int -> Int)
summaB x y = funktio 
  where
    funktio z = x + y + z

summaC :: Int -> (Int -> Int -> Int)
summaC x = funktio 
  where
    funktio y z = x+y+z

summaD :: Int -> (Int -> (Int -> Int))
summaD x = funktio1
  where 
    funktio1 y = funktio2
      where 
        funktio2 z = x+y+z



main :: IO ()
main = do
  putStrLn "Hello TIEA341"
