-# LANGUAGE TypeApplications #-}

module Main where

filterText :: (Char -> Bool) -> Text -> Text
filterText ehto teksti = fromString (filter ehto (toString teksti))

-- Erikoista forall -tyyppiset alilausekkeet  alla olevassa funktiossa.
-- Tehtävässä on helpotuksena @?? merkinnät, joihin
-- on tarpeen täydentää. Muita kuin merkattuja kohtia ei tarvitse
-- täydentää.
--
filterIT :: Text -> Text
filterIT syöte =
  let ilmanVälimerkkejä =
        filterText (\x -> notElem x [',', '.', ';', ':', '"', '\'']) syöte

      riveinä = lines ilmanVälimerkkejä

      sanalistana = map @Text @([Text]) words riveinä

      huonotPois :: [Text] -> [Text]
      huonotPois rivi = filter (\x -> notElem x ["sekä", "niin"]) rivi

      ilmanSanoja = map @([Text]) @([Text]) huonotPois sanalistana

      rivitKoottuna = map @([Text]) @Text unwords ilmanSanoja

      tekstiYhdessä = unlines rivitKoottuna
   in tekstiYhdessä

data JokoTaiMolemmat a b
  = Tyhjä
  | Vasen a
  | Oikea b
  | Molemmat a b
  deriving (Show)

rakennaVasemmasta :: forall a b. a -> JokoTaiMolemmat a b
rakennaVasemmasta a = Vasen a

rakennaOikeasta :: forall a b. b -> JokoTaiMolemmat a b
rakennaOikeasta b = Oikea b

rakennaMolemmista :: forall a b. a -> b -> JokoTaiMolemmat a b
rakennaMolemmista a b = Molemmat a b

yhteensä :: JokoTaiMolemmat Natural Natural -> Natural
yhteensä jokotai = case jokotai of
  Tyhjä -> 0
  (Oikea b) -> b
  (Vasen a) -> a
  (Molemmat a b) -> a + b

-- Esimerkki yhteensä-funktion toiminnasta:
-- TIEA341> yhteensä (rakennaMolemmista 5 7)
-- 12
--
-- TIEA341> yhteensä (rakennaMolemmista 3 2)
-- 5

main :: IO ()
main = do
  putStrLn "Hello TIEA341"
