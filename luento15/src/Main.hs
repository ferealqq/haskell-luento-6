module Main where

import Test.QuickCheck
import Data.Char

testi_1 :: [[Char]] -> Bool
testi_1 lista = words (unwords mjonot) == mjonot
  where
    mjonot = map fromString lista

test_2 :: Char -> Bool
test_2 c 
  | isUpper c = toUpper (toLower c) == c
  | otherwise = True

testi_2b :: Char -> Bool
testi_2b c = toLower (toUpper iso) == iso
  where
     iso = toLower c

testi_3 :: [Int] -> [Int] -> Bool
testi_3 l1 l2 = rev1 == rev2
  where 
    rev1 = reverse (l1++l2)
    rev2 = reverse l1 ++ reverse l2 

testi_4 :: Int -> [Int] -> Bool
testi_4 eka xs = all (\x -> x <= maximum1 xs1) xs1
  where
    xs1 = eka :| xs

data BHP a = Tyhjä
            | Yksi a
            | Haara a
                (BHP a)
                (BHP a)
            deriving (Show,Eq)

esimerkkiPuu :: BHP Int
esimerkkiPuu =
  Haara
    8
    ( Haara
        3
        (Yksi 1)
        ( Haara
            6
            (Yksi 4)
            (Yksi 7)
        )
    )
    ( Haara
        10
        Tyhjä
        ( Haara
            14
            (Yksi 13)
            Tyhjä
        )
    )

lisää :: forall a. Ord a => a -> BHP a -> BHP a
lisää arvo puu = case puu of
  Tyhjä -> Yksi arvo
  (Haara puunArvo vasen oikea) -> case compare puunArvo arvo of
    EQ -> puu
    LT ->
      let uusiOikea = lisää arvo oikea
       in Haara puunArvo vasen uusiOikea
    GT ->
      let uusiVasen = lisää arvo vasen
       in Haara puunArvo uusiVasen oikea
  (Yksi puunArvo) -> lisääKohtaanYksi puunArvo arvo

lisääKohtaanYksi :: forall a. Ord a => a -> a -> BHP a
lisääKohtaanYksi puunArvo arvo =
  case compare puunArvo arvo of
    EQ -> Yksi puunArvo
    LT -> Haara puunArvo Tyhjä (Yksi arvo)
    GT -> Haara puunArvo (Yksi arvo) Tyhjä

onkoPuussa :: forall a. Ord a => a -> BHP a -> Bool
onkoPuussa _etsitty Tyhjä = False
onkoPuussa etsitty (Yksi avain) = if avain == etsitty then True else False 
onkoPuussa etsitty (Haara avain vasen oikea) 
  = let
      onkoVasemmalla onkoPuussa etsitty vasen
      onkoOikealla = onkoPuussa etistty oikea
    in case compare etsitty avain of 
        EQ -> True 
        LT -> onkoVasemmalla
        GT -> onkoOikealla

listaPuuksi :: forall a. Ord a => [a] -> BHP a
listaPuuksi [] = Tyhjä
listaPuuksi (eka:loput) 
  = let
      loputPuuhun :: BHP a
      loputPuuhun = listaPuuksi loput
    in lisää eka loputPuuhun

puuTesti :: [Int] -> Bool
puuTesti (eka:loput) = onkoPuussa eka puu 
    where
        puu = listaPuuksi (eka : loput)

onkoPuussaTesti :: Int -> Bool
onkoPuussaTesti x = onkoPuussa x (lisää x puu)
  where
      puu = listaPuuksi [x]

notOnkoTesti :: Int -> Bool
notOnkoTesti x = onkoPuussa x (poista x puu)
  where
    puu = lisää x Tyhjä

--validi :: forall a. Ord a = BHP -> Bool


main :: IO ()
main = do
  {-
  putStrLn "Testi 1"
  quickCheck testi_1

  putStrLn "Testi 2b"
  quickCheckWith  stdArgs{maxSuccess
  =5000} testi_2b

  putStrLn "Testi 2a"
  quickCheck test_2
  -}
  
  putStrLn "Testi 3"
  quickCheckWith stdArgs{maxSuccess=1000} testi_3

  putStrLn "Testi 4"
  quickCheckWith stdArgs{maxSuccess=1000} testi_4