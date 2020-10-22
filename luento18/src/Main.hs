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

{- 

data OnkoNähty = OnNähtyN | EiOllaNähtyN deriving Eq

type Tulos a = OnkoNähty -> [a]

nollaaJälkeen :: forall a. (Eq a, Num a ) => a -> [a] -> [a]
nollaaJälkeen n lista = nollaaJälkeen_ n lista EiOllaNähtyN

nollaaJälkeen_ :: forall a. (Eq a, Num a) => a -> [a] -> Tulos a 
nollaaJälkeen_ n lista = foldr f [] lista 
  where
    f :: a -> Tulos a -> Tulos a
    f listanEka tulosLopulle 
      | listanEka == n = (\nähty -> if nähty  == OnNähtyN
                                        then 0 : tulosLopulle OnNähtyN
                                        else listanEka : tulosLopulle OnNähtyN
                          )
      | otherwise      = (\nähty -> if nähty == OnNähtyN 
                                          then 0 : tulosLopulle OnNähtyN
                                          else listanEka : tulosLopulle EiOllaNähtyN
                          )
-}

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

ota :: forall a. Natural -> [a] -> [a]
ota n lista = ota_ lista n 

ota_ lista lkm = foldr f tyhjä lista lkm 
  where
    tyhjä :: Natural -> [a] 
    tyhjä n = []
    f :: a -> (Natural -> [a]) -> (Natural -> [a]) 
    f listanEka tulosLopuille 
        = (\kuinkaMontaVielä -> 
              case (kuinkaMontaVielä > 0) of 
                    True -> listanEka : tulosLopuille (kuinkaMontaVielä-1) 
                    False -> []
          )
ota2 :: forall a. Natural -> [a] -> [a]
ota2 _n [] = []
ota2 n (eka :loput)
  | n == 0 = []
  | otherwise = eka : ota2 (n-1) loput

ota3 :: forall a. Natural -> [a] -> [a]
ota3 n lista = foldr (\eka floput m -> if m> 0 then eka:floput (m-1) else [])
                     (const [])
                     lista
                     n

main :: IO ()
main = do
  putStrLn "Hello TIEA341"
