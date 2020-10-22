module FreqSort where 

import FunktioidenYhdistäminen
import Test.QuickCheck

freqSort :: forall a. Ord a => [a] -> [a]
freqSort xs =
 let sorted = sort xs
     grouped = group sorted
     byLen = sortBy (comparing length) grouped
     result = concat byLen
 in result

freqSort_ :: forall a. Ord a => [a] -> [a]
freqSort_ xs = (sort |> group |> sortBy (comparing length) |> concat) xs 

freqSort_2 :: forall a. Ord a => [a] -> [a]
freqSort_2 xs = (concat <| sortBy (comparing length) <| group <| sort) xs

freqSort_3 :: forall a. Ord a => [a] -> [a]
freqSort_3 xs = (concat . sortBy (comparing length) . group . sort) xs

freq_testi_1 :: [Int] -> Bool
freq_testi_1 lista = freqSort lista == freqSort_ lista

freq_testi_2 :: [[Char]] -> Bool
freq_testi_2 lista = freqSort lista == freqSort_ lista

freq_testi_3 :: [[Int]] -> Bool
freq_testi_3 lista = freqSort lista == freqSort_ lista

freq_testit :: IO ()
freq_testit = do
  putStrLn("Freq Testi 1")
  quickCheck freq_testi_1
  putStrLn("Freq Testi 2")
  quickCheck freq_testi_2
  putStrLn("Freq Testi 3")
  quickCheck freq_testi_3                    

freq2_testi_1 :: [Int] -> Bool
freq2_testi_1 lista = freqSort lista == freqSort_ lista

freq2_testi_2 :: [[Char]] -> Bool
freq2_testi_2 lista = freqSort lista == freqSort_ lista

freq2_testi_3 :: [[Int]] -> Bool
freq2_testi_3 lista = freqSort lista == freqSort_ lista

freq2_testit :: IO ()
freq2_testit = do
  putStrLn("Freq 2 Testi 1")
  quickCheck freq2_testi_1
  putStrLn("Freq 2 Testi 2")
  quickCheck freq2_testi_2
  putStrLn("Freq 2 Testi 3")
  quickCheck freq2_testi_3 