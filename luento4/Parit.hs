module Parit where

esimerkkiPari :: (Text, Natural)
esimerkkiPari = ("Ville", 40)

esimerkinEkaAlkio = fst esimerkkiPari
esimerkinTokaAlkio = snd esimerkkiPari

tulostaNimipari :: (Text, Natural) -> Text
tulostaNimipari nimiPari 
    = case nimiPari of 
        (nimi,ikä) -> "Henkilö\n" <> nimi <> " on " <> show ikä <> " vuotta vanha"

-- putTextLn it 
-- it viittaa äsken käytettyyn arvoon

ikä = 20
nimi = "pekka"

esimerkkiPari2 :: (Text, Natural, Bool)
esimerkkiPari2 = (nimi,ikä,False)