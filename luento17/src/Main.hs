module Main where

import Data.Char -- Tämä heti module rivin alle
import Test.QuickCheck

summaFoldilla :: forall a. Num a => [a] -> a
summaFoldilla = foldlPilkku (+) 0

foldlPilkku päivitäVälitulos tyhjä lista = apu tyhjä lista
  where
    apu välitulos [] = välitulos
    apu !välitulos (eka : loput) =
      apu (päivitäVälitulos välitulos eka) loput

summa :: forall a. Num a => [a] -> a
summa = foldr (+) 0

arvoväli :: forall a. (Num a, Ord a) => [a] -> a
arvoväli [] = 0
arvoväli lista =
  let m = foldlPilkku min 0 
      mx = foldlPilkku max 0 
   in mx lista - m lista

yksikäänEiFalse :: [Bool] -> Bool 
yksikäänEiFalse = foldlPilkku (\nyt seuraava -> nyt == True && seuraava == True) True

onListassa :: forall a. Eq a => a -> [a] -> Bool
onListassa arvo lista = foldr (\nyt loput -> if loput == True then True else if nyt == arvo then True else False) False lista

etsi :: forall a. (a -> Bool) -> [a] -> Maybe a 
etsi f l = foldr funktio Nothing l
  where
    funktio nyt loput = case f nyt of
                True -> Just nyt
                False -> loput

{-
testi_onListassa :: Natural -> [Natural] -> Bool
testi_onListassa etsi lista
  = let
    tulos = onListassa etsi lista
    in or (map (\x -> x == etsi))
-}
onko :: forall a. Eq a => a -> a -> Bool
onko a b = a == b
--testi_yksikään :: [Bool] -> Bool
--testi_yksikään lista = yksikäänEiFalse lista

data EkaVaiToka = Eka | Toka

partition :: forall a. Ord a => (a -> EkaVaiToka) -> [a] -> ([a],[a])
partition f l = foldr apuF ([],[]) l
  where
    apuF nyt (ekat,tokat) = case f nyt of
                              Eka -> ([nyt]++ekat,tokat)
                              Toka -> (ekat,[nyt]++tokat)  

sekasin :: [Char] -> (Bool,[Char])
sekasin = foldr f (False,[])
  where 
    f :: Char -> (Bool, [Char]) -> (Bool,[Char])
    f eka (olikoIso,loputSekasin)
      | olikoIso  = (False, toLower eka : loputSekasin)
      | otherwise = (True , toUpper eka : loputSekasin)


poistaKaikkiN :: Eq a => a -> [a] -> [a]
poistaKaikkiN n l = foldr apuF [] l
  where
    apuF nyt loput = case n == nyt of
                        True -> case onListassa n loput of 
                                  True -> l
                                  False -> loput 
                        False -> [nyt]++loput

{-
poistaViimeinen_ :: Eq a => a -> [a] -> (Bool,[a])
poistaViimeinen_ n = foldr apuF (True,[])
  where
    apuF :: a -> (Bool,[a]) -> (Bool,[a])
    apuF nyt (eiPoistettu,loput) 
          | nyt == n && eiPoistettu = (False, loput)
          | otherwise = (eiPoistettu, nyt : loput)
-}

poistaViimeinen :: forall a . Eq a => a -> [a] -> (Bool,[a])
poistaViimeinen x = foldr f (True,[])
  where
    f :: a -> (Bool,[a]) -> (Bool,[a])
    f eka (eiOlePoistettu, loputJostaEhkäPoistettu)
            | eka == x && eiOlePoistettu = (False, loputJostaEhkäPoistettu)
            | otherwise = (eiOlePoistettu, eka : loputJostaEhkäPoistettu)

main :: IO ()
main = do
  putStrLn "Hello TIEA341"