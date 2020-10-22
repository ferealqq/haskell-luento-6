module BinaariPuu where

data BHP avain
  = Tyhjä
  | Yksi avain
  | Haara
      avain
      (BHP avain)
      (BHP avain)
  deriving (Show)

{-
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
-}
haePienin :: forall a. BHP a -> Maybe a
haePienin Tyhjä = Nothing
haePienin (Yksi avain) = Just avain
haePienin (Haara avain vasen _oikea) = case vasen of
  Tyhjä -> Just avain
  otherwise -> haePienin vasen

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

irroitaPienin :: forall a. Ord a => BHP a -> Maybe (a, BHP a)
irroitaPienin puu =
  let pienin = haePienin puu
      uusiPuu = case puu of
        Tyhjä -> Tyhjä
        Yksi puunArvo -> case compare (Just puunArvo) pienin of
          EQ -> Tyhjä
          otherwise -> puu
        Haara puunArvo vasen oikea ->
          case compare (Just puunArvo) pienin of
            EQ -> Tyhjä
            LT ->
              let maybeOikea = irroitaPienin oikea
                  uusiOikea = case maybeOikea of
                    Just (_, maybePuu) -> maybePuu
                    Nothing -> Tyhjä
               in Haara puunArvo vasen uusiOikea
            GT ->
              let uusiVasen = case irroitaPienin vasen of
                    Just (_, maybePuu) -> maybePuu
                    Nothing -> Tyhjä
               in Haara puunArvo uusiVasen oikea
   in case pienin of
        Just arvo -> Just (arvo, uusiPuu)
        Nothing -> Nothing

lisääLista :: forall a. Ord a => [a] -> BHP a -> BHP a
lisääLista [] puu = puu
lisääLista (eka : loput) puu = lisääLista loput (lisää eka puu)

irroitaKaksiPienintä :: forall a. Ord a => BHP a -> Maybe (a, BHP a)
irroitaKaksiPienintä puu =
  let pieninIrroitettu = irroitaPienin puu
      uusiPuu = case pieninIrroitettu of
        Just (_, mPuu) -> mPuu
        Nothing -> puu
   in irroitaPienin uusiPuu

-- ei toimi
-- viddu
-- testaa jotain muuta
{-
poista :: forall a. Ord a => a -> BHP a -> BHP a
poista poistettava puu =
  let uusiPuu = case puu of
        Tyhjä -> Tyhjä
        Yksi puunArvo -> case compare puunArvo poistettava of
          EQ -> Tyhjä
          otherwise -> puu
        Haara puunArvo vasen oikea ->
          case compare puunArvo poistettava of
            EQ -> Tyhjä
            LT ->
              case oikea of
                Tyhjä -> Tyhjä
                (Yksi oikeaArvo) ->
                  if oikeaArvo == poistettava
                    then Haara puunArvo vasen Tyhjä
                    else error "MurMur ei mahdollista"
                (Haara oikeaArvo lapsiVasen lapsiOikea) ->
                  case compare oikeaArvo poistettava of
                    EQ -> case vasen of
                      Tyhjä -> Yksi puunArvo
                      otherwise -> Haara puunArvo vasen Tyhjä
                    LT ->
                      let uusiLapsiOikea = poista poistettava lapsiOikea
                       in Haara puunArvo vasen (Haara oikeaArvo lapsiVasen uusiLapsiOikea)
                    GT ->
                      let uusiLapsiVasen = poista poistettava lapsiVasen
                       in Haara puunArvo vasen (Haara oikeaArvo uusiLapsiVasen lapsiOikea)
            GT ->
              case vasen of
                Tyhjä -> Tyhjä
                (Yksi vasenArvo) ->
                  if vasenArvo == poistettava
                    then Haara puunArvo vasen Tyhjä
                    else error "MurMur ei mahdollista"
                (Haara vasenArvo lapsiVasen lapsiOikea) ->
                  case compare vasenArvo poistettava of
                    EQ -> case vasen of
                      Tyhjä -> Yksi puunArvo
                      otherwise -> Haara puunArvo vasen Tyhjä
                    LT ->
                      let uusiLapsiOikea = poista poistettava lapsiOikea
                       in Haara puunArvo vasen (Haara vasenArvo lapsiVasen uusiLapsiOikea)
                    GT ->
                      let uusiLapsiVasen = poista poistettava lapsiVasen
                       in Haara puunArvo vasen (Haara vasenArvo uusiLapsiVasen lapsiOikea)
   in uusiPuu
-}

{-
errMsg :: IsString p => p
errMsg = "poistettavaa alkiota ei ole puussa"

poista :: forall a. Ord a => a -> BHP a -> BHP a
poista _ Tyhjä = Tyhjä
poista poistettava (Yksi alkio) = if alkio == poistettava then Tyhjä else error errMsg
poista poistettava (Haara alkio vasen oikea) =
  let uusiPuu = case compare alkio poistettava of
        EQ -> Tyhjä
        LT -> case oikea of
          Tyhjä -> error errMsg
          (Yksi oikeaArvo) -> if oikeaArvo == poistettava then Haara alkio vasen Tyhjä else error errMsg
          (Haara lapsiArvo lapsiVasen lapsiOikea) ->
            case compare lapsiArvo poistettava of
              EQ -> case vasen of
                Tyhjä -> Yksi alkio
                otherwise -> Haara alkio vasen Tyhjä
              LT -> Haara alkio vasen (Haara lapsiArvo lapsiVasen (poista poistettava lapsiOikea))
              GT -> Haara alkio vasen (Haara lapsiArvo (poista poistettava lapsiVasen) lapsiOikea)
        GT -> case vasen of
          Tyhjä -> error errMsg
          (Yksi oikeaArvo) -> if oikeaArvo == poistettava then Haara alkio vasen Tyhjä else error errMsg
          (Haara lapsiArvo lapsiVasen lapsiOikea) ->
            case compare lapsiArvo poistettava of
              EQ -> case oikea of
                Tyhjä -> Yksi alkio
                otherwise -> Haara alkio vasen Tyhjä
              LT -> Haara alkio (Haara lapsiArvo lapsiVasen (poista poistettava lapsiOikea)) oikea
              GT -> Haara alkio (Haara lapsiArvo (poista poistettava lapsiVasen) lapsiOikea) oikea
   in uusiPuu
-}

err :: a
err = error "poistettava alkio ei ole puussa"

poista :: forall a. Ord a => a -> BHP a -> BHP a
poista _ Tyhjä = Tyhjä
poista poistettava (Yksi alkio)
  | poistettava == alkio = Tyhjä
  | otherwise = Yksi alkio
poista poistettava (Haara alkio vasen oikea) =
  let poistaOikea = poista poistettava oikea
      poistaVasen = poista poistettava vasen
   in case compare poistettava alkio of
        EQ -> Tyhjä
        LT -> Haara alkio poistaVasen oikea
        GT -> Haara alkio vasen poistaOikea

irroitaPienin2 :: forall a. BHP a -> Maybe (a, BHP a)
irroitaPienin2 Tyhjä = Nothing
irroitaPienin2 (Yksi a) = Just (a, Tyhjä)
irroitaPienin2 (Haara arvo vasen oikea) =
  let ehkäVasen = irroitaPienin2 vasen
   in case ehkäVasen of
        Nothing -> Just (arvo, oikea)
        Just (pienin, pieninVasen) -> Just (pienin, Haara arvo pieninVasen oikea)

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

main :: IO ()
main = do
  putStrLn "Hello TIEA341"
