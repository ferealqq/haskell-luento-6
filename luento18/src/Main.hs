module Main where

import FreqSort
import FunktioidenYhdistäminen
import Test.QuickCheck

-- nollaaJälkeen 2 [1,2,3,4,5,6]
--    [1,2,0,0,0,0]

{-
nollaaJälkeen :: forall a. (Num a, Ord a) => a -> [a] -> [a]
nollaaJälkeen n lista = apu
  where
    f :: forall a. (Num a,Ord a) => a -> (Bool,[a]) -> (Bool, [a])
    f eka (onkoLöydetty,loput) = case compare eka n of
            EQ -> (True,loput)
            otherwise -> case onkoLöydetty of
                      True -> (onkoLöydetty,[0]++loput)
                      False -> (onkoLöydetty,loput)
    apu = snd (foldr f (False,[]) (False,lista))
-}

data OnkoNähty = OnNähtyN | EiOllaNähtyN deriving (Eq)

type Tulos a = OnkoNähty -> [a]

nollaaJälkeen :: forall a. (Eq a, Num a) => a -> [a] -> [a]
nollaaJälkeen n lista = nollaaJälkeen_ n lista EiOllaNähtyN

nollaaJälkeen_ :: forall a. (Eq a, Num a) => a -> [a] -> OnkoNähty -> [a]
nollaaJälkeen_ n lista = foldr f (\_ -> []) lista
  where
    f :: a -> (OnkoNähty -> [a]) -> (OnkoNähty -> [a])
    f listanEka tulosLopulle
      | listanEka == n =
        ( \nähty ->
            if nähty == OnNähtyN
              then 0 : tulosLopulle OnNähtyN
              else listanEka : tulosLopulle OnNähtyN
        )
      | otherwise =
        ( \nähty ->
            if nähty == OnNähtyN
              then 0 : tulosLopulle OnNähtyN
              else listanEka : tulosLopulle EiOllaNähtyN
        )

{-
ota :: forall a. Natural -> [a] -> [a]
ota n lista = snd (foldr f (0,[]) (1,lista))
  where
      f (index,[]) = []
      f (index,(eka:loput)) = case compare index n of
                                EQ -> (index,[eka]++loput)
                                LT -> (index+1,[eka]++loput)
                                GT -> (index+1,[])
-}

poistaViimeinen :: forall a. Eq a => a -> [a] -> (Bool, [a])
poistaViimeinen n ar = foldr f (False, []) ar
  where
    f eka (poistettu, lista) =
      case poistettu of
        True -> (True, [eka] ++ lista)
        False -> if eka == n then (True, lista) else (False, lista ++ [eka])

nollaaJälkeen2 :: Natural -> [Natural] -> [Natural]
nollaaJälkeen2 n ar = fApu n ar False
  where
    fApu :: Natural -> [Natural] -> Bool -> [Natural]
    fApu p l = foldr fun (\_ -> []) l
      where
        fun eka tulosLopulle
          | p == eka =
            ( \nähty ->
                if nähty
                  then 0 : tulosLopulle True
                  else eka : tulosLopulle True
            )
          | otherwise =
            ( \nähty ->
                if nähty
                  then 0 : tulosLopulle True
                  else eka : tulosLopulle False
            )

yksikäänEiFalse :: [Bool] -> Bool
yksikäänEiFalse = foldl' f True
  where
    f eka seuraava
      | eka && seuraava = True
      | otherwise = False

ota :: forall a. Natural -> [a] -> [a]
ota n lista = ota_ lista n

ota_ lista lkm = foldr f tyhjä lista lkm
  where
    tyhjä :: Natural -> [a]
    tyhjä n = []
    f :: a -> (Natural -> [a]) -> (Natural -> [a])
    f listanEka tulosLopuille =
      ( \kuinkaMontaVielä ->
          case (kuinkaMontaVielä > 0) of
            True -> listanEka : tulosLopuille (kuinkaMontaVielä -1)
            False -> []
      )

ota2 :: forall a. Natural -> [a] -> [a]
ota2 _n [] = []
ota2 n (eka : loput)
  | n == 0 = []
  | otherwise = eka : ota2 (n -1) loput

ota3 :: forall a. Natural -> [a] -> [a]
ota3 n lista =
  foldr
    (\eka floput m -> if m > 0 then eka : floput (m -1) else [])
    (const [])
    lista
    n

main :: IO ()
main = do
  putStrLn "Hello TIEA341"
