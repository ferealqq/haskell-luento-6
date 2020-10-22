module Main where

import System.Environment

{-
let x = let x = 5 in x * 2
    y = let x = 4 in x + 1
in x + y + 5
-}

vastaus :: Num a => a
vastaus =
  let kerrottu = let kerrotaan = 5 in kerrotaan * 2
      plussattu = let plussataan = 4 in plussataan + 1
   in kerrottu + plussattu + 5

{-
bwciBo :: Chwyrligwgan -> BwganBrain
oncoFonco ::
  (Chwyrligwgan -> Chwyrligwgan) ->
  BwganBrain ->
  (Chwyrligwgan -> BwganBrain -> Swmpus) ->
  (BwganBrain -> X)
phumoedd :: Chwyrligwgan
meicrodon :: Chwyrligwgan -> Chwyrligwgan
twmffat :: BwganBrain -> Chwyrligwgan -> BwganBrain -> Swmpus
x
  = let
      y = phumoedd
      z = bwciBo y
    in z
-}

laskeMissäEniten :: Int -> Char -> [Text] -> [Text]
laskeMissäEniten n kirjain sanasto =
  case ((length sanasto) < n) of
    True -> error "sanaston listassa pitää olla enemmän elementtejä kun n"
    False ->
      let parit = map (\sana -> laskeKirjaimenEsiintymät kirjain sana) sanasto
          enitenPari = reverse (sort parit)
          nEnitenPareja :: [(Int, Text)]
          nEnitenPareja = take n enitenPari
          missäEniten = map (\(_, x) -> x) nEnitenPareja
       in missäEniten

-- Tähän lauseke joka laskee listaan ne n sanaa sanastosta, jossa
-- on eniten pyydettyä kirjain:ta

-- Tälläinen apufunktio voisi olla hyödyllinen.
-- Toteuta se jos haluat tai poista kokonaan jos et tarvitse.
laskeKirjaimenEsiintymät :: Char -> Text -> (Int, Text)
laskeKirjaimenEsiintymät kirjain sana =
  let kirjaimetListana = toString sana
      esiintymä = filter (\k -> kirjain == k) kirjaimetListana
   in ((length esiintymä), sana)

-- Tässä pääohjelma. Sitä ei tarvitse muuttaa, mutta voit kokeilla
-- ohjelmaasi sen avulla. Älä kuitenkaan hävitä pääohjelmaa,
-- tenttiteknisistä syistä.
--
-- Komentoriviltä:
--   1) `stack build`
--   2) `stack exec <ohjelmasi nimi> e 5 src/Main.hs` (tai joku muu tekstitiedosto)
--
--  Replistä:
--   :main e 5 src/Main.hs
--   (Voit kokeilla myös jollain muulla tekstitiedostolla kuin src/Main.hs)
--
--   Voit myös kokeilla aliohjelmaasi ihan normaalisti replistä.

main :: IO ()
main = do
  argumentit <- getArgs
  let ohje = putTextLn "Anna argumentteina yksi kirjan ja tiedoston nimi"
  case argumentit of
    [[c], n, tiedostonNimi] ->
      case readMaybe n of
        Just numero -> do
          sisältö <- readFileText tiedostonNimi
          print (laskeMissäEniten numero c (words sisältö))
        Nothing -> ohje
    _ -> ohje