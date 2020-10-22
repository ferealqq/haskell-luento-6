module Main where

import Data.Char -- Tämä heti module rivin alle
import Test.QuickCheck

data Osa = Eka | Toka | Kolmas deriving (Show, Eq)

modulo3 :: Int -> Osa
modulo3 n = case mod n 3 of
  1 -> Eka
  2 -> Toka
  _ -> Kolmas

kirjainLuokka :: Char -> Osa
kirjainLuokka c
  | isUpper c = Eka
  | isSpace c = Kolmas
  | otherwise = Toka

jaaKolmeenOsaan :: forall a. (a -> Osa) -> [a] -> ([a], [a], [a])
jaaKolmeenOsaan _f [] = (tyhja, tyhja, tyhja)
  where
    tyhja = []
jaaKolmeenOsaan funktio lista =
  let mäpätty = map (\x -> (funktio x, x)) lista
      ekat = filter (\(y, _ys) -> y == Eka) mäpätty
      tokat = filter (\(y, _ys) -> y == Toka) mäpätty
      kolt = filter (\(y, _ys) -> y == Kolmas) mäpätty
      vastaus l = map (\(_z, zs) -> zs) l
   in (vastaus ekat, vastaus tokat, vastaus kolt)

data BHP avain
  = Tyhjä
  | Yksi avain
  | Haara
      avain
      (BHP avain)
      (BHP avain)
  deriving (Show)

esimerkkipuu :: BHP Int
esimerkkipuu =
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

haePienin :: forall a. BHP a -> Maybe a
haePienin Tyhjä = Nothing
haePienin (Yksi avain) = Just avain
haePienin (Haara avain vasen _) = case vasen of
  Tyhjä -> Just avain
  _otherwise -> haePienin vasen

poista :: forall a. Ord a => a -> BHP a -> BHP a
poista _luku Tyhjä = Tyhjä
poista luku (Yksi alkio) = case alkio == luku of
  True -> Tyhjä
  False -> (Yksi alkio)
poista luku (Haara alkio vasen oikea) =
  case compare luku alkio of
    GT -> Haara alkio vasen (poista luku oikea)
    LT -> Haara alkio (poista luku vasen) oikea
    EQ -> case irroitaPienin oikea of
      Nothing -> vasen
      Just (pienin, puuIlmanPienintä) -> Haara pienin vasen puuIlmanPienintä

irroitaPienin :: forall a. BHP a -> Maybe (a, BHP a)
irroitaPienin Tyhjä = Nothing
irroitaPienin (Yksi a) = Just (a, Tyhjä)
irroitaPienin (Haara alkio vasen oikea) =
  let ehkäUusiVasenJaAlkio = irroitaPienin vasen
   in case ehkäUusiVasenJaAlkio of
        Nothing -> Just (alkio, oikea)
        Just (pieninAlkio, vasenEiPienintä) -> Just (pieninAlkio, Haara alkio vasenEiPienintä oikea)

järjestetyksiListaksi :: forall a. Ord a => BHP a -> [a]
järjestetyksiListaksi Tyhjä = []
järjestetyksiListaksi puu = fst (listaksi puu [])
  where
    listaksi puu2 lista =
      let pieninIrroitettu = case irroitaPienin puu2 of
            Just (pienin, uusiPuu) -> listaksi uusiPuu (lista ++ [pienin])
            Nothing -> (lista, puu2)
       in pieninIrroitettu

-- Eka testi, tämän saat ilmaiseksi!
-- Hypoteesi: unwords (words x) on sama kuin x kaikille x:
testi_1 :: [Char] -> Bool
testi_1 merkkilista = unwords (words (fromString merkkilista)) == fromString merkkilista

-- Tulos: Ei pitänyt paikkaansa! Jos on vain välilyöntejä, ne katoavat!

-- Hypoteesi: Char arrrayn jokainen olio muutetuksi text muotoon jota kutsutaan vaikka x:ksi on sama kuin words ( unwords x ) joka on sama asia kun alkuperäinen muttuja l  
testi_2 :: [[Char]] -> Bool 
testi_2 l = words (unwords (map (\x -> toString x) l) == l

-- Tulos: Pitää paikkansa

-- Hypoteesi: listan kirjaimet ovat samat fromString kutsun jälkeen

-- Hypoteesi: kun käännät saman listan kaksi kertaa pitäisi sen olla alkuperäisessä muodossaan.
testi_5 :: [Int] -> Bool
testi_5 l = reverse (reverse l) == l

-- Tulos piti paikkansa. Kun käännät listan kaksi kertaa niin on vastaus sama kun alkuperäinen lista.

-- Hypoteesi: pituuden pitäisi olla sama kun plussaat jokaisesta listan jäsenestä yhden
testi_6 :: [Int] -> Bool
testi_6 l = length l == pituus l

pituus :: [Int] -> Int
pituus [] = 0
pituus (_x : xs) = 1 + (pituus xs)

main :: IO ()
main = do
  putStrLn "Hello TIEA341"
