{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main where

import Prelude hiding (foldr, map)

--map :: forall a b . (a -> b) -> [a] -> [b]
alkupMap :: forall a b. (a -> b) -> [a] -> [b]
alkupMap f lista = foldr (\eka loputMäpätty -> f eka : loputMäpätty) [] lista

foldr _laitaYhteen tyhja [] =
  tyhja
foldr laitaYhteen tyhja (eka : loput) =
  let loputMäpätty = foldr laitaYhteen tyhja loput
   in laitaYhteen eka loputMäpätty

alkupSumma :: forall a. Num a => [a] -> a
alkupSumma = foldr (\eka loppujenSumma -> eka + loppujenSumma) 0

{-summa tyhjä _laitaYhteen []
 = tyhjä
summa tyhjä (eka : loput)
 = let
    loppujenSumma = summa tyhjä laitaYhteen loput
   in laitaYhteen eka loppujenSumma-}

alkuMaksimi :: Ord a => [a] -> Maybe a
alkuMaksimi = summaJaMaksimi Nothing apu
  where
    apu suurinTähänMennessä [] =
      suurinTähänMennessä
    apu !suurinTähänMennessä (eka : loput) =
      apu (max suurinTähänMennessä eka) loput

alkuSumma :: Num a => [a] -> Maybe a
alkuSumma = summaJaMaksimi Nothing apu
  where
    apu summaNyt [] =
      summaNyt
    apu !summaNyt (eka : loput) =
      apu (eka + summaNyt) loput

--summaJaMaksimi :: Maybe a -> (t -> [t] -> a) -> [t] -> Maybe a
summaJaMaksimi tyhja _apu [] = tyhja
summaJaMaksimi _tyhja apu (eka : loput) = Just (apu eka loput)

alkupMaksimi = summaJaMaksimiB Nothing max Just

maksimiA tyhja _yhteen [] = tyhja
maksimiA _tyhja yhteen (eka1 : loput1) = Just (maksimiApu eka1 loput1)
  where
    maksimiApu suurinTähänMennessä [] =
      suurinTähänMennessä
    maksimiApu !suurinTähänMennessä (eka : loput) =
      maksimiApu (yhteen suurinTähänMennessä eka) loput

alkupSummaA = summaJaMaksimiB 0 (+) (\x -> x)

summaA tyhja _yhteen [] = tyhja
summaA _tyhja yhteen (eka1 : loput1) = Just (summaApu eka1 loput1)
  where
    summaApu summaNyt [] =
      summaNyt
    summaApu !summaNyt (eka : loput) =
      summaApu (yhteen eka summaNyt) loput

-- summaJaMaksimiB on sama kuin foldl mutta pari funktio muuttujaa on väärässä kohdassa
summaJaMaksimiB tyhja _yhteen _lopuksi [] = tyhja
summaJaMaksimiB _tyhja yhteen lopuksi (eka1 : loput1) = lopuksi (apuFunktio eka1 loput1)
  where
    apuFunktio väliarvo [] =
      väliarvo
    apuFunktio !väliarvo (eka : loput) =
      apuFunktio (yhteen eka väliarvo) loput

foldl_ _yhteen tyhja _lopuksi [] = tyhja
foldl_ yhteen _tyhja lopuksi (eka1 : loput1) = lopuksi (apuFunktio eka1 loput1)
  where
    apuFunktio väliarvo [] =
      väliarvo
    apuFunktio !väliarvo (eka : loput) =
      apuFunktio (yhteen eka väliarvo) loput

summa :: forall a. Num a => [a] -> a
summa = foldr (+) 0

minimi :: forall a. Ord a => [a] -> Maybe a
minimi = foldl_ min Nothing Just

arvoväli :: forall a. (Num a, Ord a) => [a] -> a
arvoväli [] = 0
arvoväli lista =
  let m = foldl_ min 0 (\x -> x)
      mx = foldl_ max 0 (\x -> x)
   in mx lista - m lista

main :: IO ()
main = do
  putStrLn "Hello TIEA341"