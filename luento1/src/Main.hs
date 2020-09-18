module Main where

-- Määritelmän nimi
-- |  'on tyyppiä'
-- |   |      funktio luonnolliselta luvulta luonnolliselle luvulle
-- ↓   ↓          ↓
tuplaa :: Natural -> Natural
tuplaa seLuku = seLuku + seLuku
--      ↑     ↑        ↑
--  funktion parametri |
--            |        |
--            |   mitä lasketaan
--     'on sama kuin'

-- Testaaminen. Käynnistä `stack repl` komentoriviltä ja kirjoita
-- tuplaa 5

kerroKahdella :: Natural -> Natural
kerroKahdella seLuku = seLuku * 2


-- Harjoitellaan 'let-in' rakennetta. Sillä voi tehdä
-- paikallisia määritelmiä. Eli, let x=5 in x+x luetaan
-- "olkoon x yhtä kuin 5 kun lasketaan x+x" ja tulos on 10.
onkoPalindromi :: Text -> Bool
onkoPalindromi syöte 
 = let                                -- ← let ja in kuuluvat yhteen
     syöteLista = toString syöte      -- ← Tässä välissä on lokaaleja määritelmiä ...
     takaperin  = reverse syöteLista  --   ... jotka pitää sisentää
   in syöteLista == takaperin         -- ← ... joita käytetään täällä
-- ↑   
-- Huom. sisennä in ja let samaan sarakkeeseen!

onkoAnagrammi :: Text -> Text -> Bool
onkoAnagrammi syöte₁ syöte₂  -- ← Unicodea ja ääkkösiä saa käyttää
 = let
    lista₁ = toString syöte₁
    lista₂ = toString syöte₂
   in sort lista₁ == sort lista₂   

benefits :: Double
benefits = 422 
saved :: Double
saved = 82

months :: Integer 
months = ceiling ((1260 - (saved))/benefits)

summa :: [Integer] -> Integer
summa [] = 0
summa (x:xs) = x + summa xs 

kuinkaMontaAta :: Text -> Int
kuinkaMontaAta syöte = length (filter (=='a') (toString syöte))

jotain :: Double -> Double
jotain input = 1 + let x = input+saved in x

listaTestaus :: Text -> [Char]
listaTestaus syöte = let y = sort (toString syöte) in y 

headTest :: [a] -> a
headTest [] = error "No head for empty lists!"
headTest (x:_) = x 

testCase :: [a] -> a
testCase xs = case xs of [] -> error "No head for empty lists!"
                         (x:_) -> x
                        
ifTest :: Text -> Text -> Text
ifTest x xs = if x==xs then "ok" else "bad"

ifTest1 :: Integer -> Integer 
ifTest1 number = number + if number == 2 then 2 else 1

kissaTest :: Int 
kissaTest 
 = let 
      alku = "kissa istuu aidalla"
      pelkatAat = filter (=='a') alku
      tulos = length pelkatAat
   in tulos


funcTest :: Integer -> Integer -> Integer 
funcTest x y = (\xs ys -> min xs ys + 1) x y

mapTest :: [Integer] -> [Integer]
mapTest x = map (\xs -> min xs 3 + 1) x 

filterTest :: [Integer] -> [Integer]
filterTest x = filter (\xs -> even xs && xs>5) x

allTest :: [Integer] -> Bool 
allTest x = all (\xs -> 2 /= xs && xs<10) x

summaEsimerkki :: [Natural] -> Natural
summaEsimerkki lista = let 
                loop !n [] = n
                loop !n (eka : loput) = loop (n + eka) loput
              in loop 0 lista

main :: IO ()
main = do
  putStrLn "Hello TIEA341"
