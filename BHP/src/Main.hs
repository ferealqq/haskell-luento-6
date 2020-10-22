module Main where
import Test.QuickCheck
import Data.Char

data BHP avain =  Tyhjä
                  | Yksi avain
                  | Haara avain (BHP avain) (BHP avain) deriving Show

esimerkkipuu :: BHP Int
esimerkkipuu = Haara 8 
                          (Haara 3 
                            (Yksi 1) 
                            (Haara 6 
                              (Yksi 4) 
                              (Yksi 7))) 
                            (Haara 10 
                              Tyhjä 
                              (Haara 14 
                                (Yksi 13) 
                                Tyhjä))


lisää :: forall a . Ord a => a -> BHP a -> BHP a
lisää a Tyhjä = Yksi a
lisää a (Yksi alkio) = 
                  let
                    pienempi = min a alkio
                    suurempi = max a alkio -- :D 
                    vastaus = if pienempi == suurempi then Haara suurempi Tyhjä (Yksi pienempi) else Haara suurempi (Yksi pienempi) Tyhjä
                  in vastaus
lisää a (Haara alkio vasen oikea)
  | a < alkio 
    = let
        uusiVasen = lisää a vasen
      in Haara alkio uusiVasen oikea
  | otherwise
    = let
        uusiOikea = lisää a oikea
      in Haara alkio vasen uusiOikea

irroitaPienin :: forall a. BHP a -> Maybe (a, BHP a)
irroitaPienin Tyhjä = Nothing
irroitaPienin (Yksi a) = Just (a, Tyhjä)
irroitaPienin (Haara alkio vasen oikea) 
                = let 
                    ehkäUusiVasenJaAlkio = irroitaPienin vasen
                  in case ehkäUusiVasenJaAlkio of
                            Nothing -> Just (alkio, oikea)
                            Just (pieninAlkio, vasenEiPienintä) -> Just (pieninAlkio, Haara alkio vasenEiPienintä oikea)



poista :: forall a . Ord a => a -> BHP a -> BHP a
poista _luku Tyhjä = Tyhjä
poista luku (Yksi alkio) = case alkio == luku of
                            True -> Tyhjä
                            False -> (Yksi alkio)
poista luku (Haara alkio vasen oikea) 
                                = case compare luku alkio of
                                  GT -> Haara alkio vasen (poista luku oikea)
                                  LT -> Haara alkio (poista luku vasen) oikea
                                  EQ -> case irroitaPienin oikea of 
                                          Nothing -> vasen
                                          Just (pienin, puuIlmanPienintä) -> Haara pienin vasen puuIlmanPienintä




onkoPuussa :: forall a. Ord a => a -> BHP a -> Bool
onkoPuussa _etsitty Tyhjä =  False
onkoPuussa etsitty (Yksi avain) = avain == etsitty
onkoPuussa etsitty (Haara avain vasen oikea)
                          = case compare etsitty avain of
                              EQ -> True
                              LT -> onkoPuussa etsitty vasen
                              GT -> onkoPuussa etsitty oikea

haePienin :: forall a . BHP a -> Maybe a
haePienin Tyhjä = Nothing
haePienin (Yksi avain) = Just avain
haePienin (Haara avain vasen _) = case vasen of 
                                  Tyhjä -> Just avain
                                  _otherwise -> haePienin vasen



listaPuuksi :: forall a. Ord a => [a] -> BHP a
listaPuuksi [] = Tyhjä
listaPuuksi (eka:vikat)
    = let
        puu = Yksi eka
        uusiPuu = lisääRekursiivisesti puu vikat
      in uusiPuu



lisääRekursiivisesti :: forall a . Ord a => BHP a -> [a] -> BHP a
lisääRekursiivisesti rPuu [] = rPuu 
lisääRekursiivisesti rPuu (x:xs) = lisää x (lisääRekursiivisesti rPuu xs)

validi :: forall a. Ord a => BHP a -> Bool
validi Tyhjä = True
validi (Yksi _x) = True
validi (Haara arvo vasen oikea)
  = let
      vasenArvo = haePuunArvo vasen
      oikeaArvo = haePuunArvo oikea
      vasenValidi = validi vasen
      oikeaValidi = validi oikea
    in if apuValidi arvo vasenArvo oikeaArvo then oikeaValidi == True && vasenValidi == True else False

apuValidi :: forall a. Ord a => a -> Maybe a -> Maybe a -> Bool 
apuValidi arvo Nothing Nothing = True
apuValidi arvo Nothing (Just mOikea) = arvo <= mOikea
apuValidi arvo (Just mVasen) Nothing = arvo > mVasen
apuValidi arvo (Just mVasen) (Just mOikea) 
        | mVasen < arvo && arvo <= mOikea = True
        | otherwise = False

haePuunArvo :: forall a. BHP a -> Maybe a
haePuunArvo puu = case puu of
                      Tyhjä -> Nothing
                      Yksi alkio -> Just alkio 
                      Haara alkio _ _ -> Just alkio

validi_testi_1 :: [Int] -> Bool
validi_testi_1 l = validi (listaPuuksi l)
-- Ei pidä paikkaansa testin lopputulos koska listaPuuski toimii väärällä menetelmällä.
-- lisää funktio toimii väärin. lisääminen ei ota huomioon yhtäsuuri tilanteita vaan laitta yhtäsuuren alkion vasemlle puolelle
-- eikä oikealle puolelle. Kun validi funktio vaatii että YHTÄSUURI on oikealla puolella

-- Korjasin lisää funktion toimimaan oikein.

validi_testi_2 :: Int -> [Int] -> Bool 
validi_testi_2 x l = validi (lisää x puu)
  where 
    puu = listaPuuksi l

validi_testi_3 :: Int -> [Int] -> Bool 
validi_testi_3 x l = validi (poista x puu)
  where 
    puu = listaPuuksi l


testi1 :: Int -> [Int] -> Bool
testi1 x xs = onkoPuussa x puu
  where
    puu = listaPuuksi (x:xs)

testi2 :: Int -> [Int] -> Bool
testi2 x (b:xs) = False
testi2 x [] = onkoPuussa x Tyhjä == False

testi3 :: Int -> [Int] -> Bool
testi3 x lista = onkoPuussa x (lisää x puu)
  where
    puu = listaPuuksi lista

testi4 :: Int -> [Int] -> Bool
testi4 x lista = not (onkoPuussa x (poista x puu))
  where
    puu = listaPuuksi (ordNub lista)





main :: IO ()
main = do
  putStrLn "Hello TIEA341"