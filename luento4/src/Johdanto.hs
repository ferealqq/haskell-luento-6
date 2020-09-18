module Johdanto where

-- Arvo, tietotyypit, literaali
-- (Arvo)14 ,(tietotyyppi) Int, (Literaali)14
-- True, Bool       , true 
-- Suorakaide 4 5 6, Paketti , Suorakaide 4 5 6 
-- Set.signleton 5, Set Natural, -- (Ei literaali esitystä)
-- "Tekstiä tässä",  Text , "tuplahipsuihin literaalit"
-- ['t','e','k','s'], [Char], "teks"

-- Määritelmä: 
teePalindromi :: [Char] -> [Char]
teePalindromi teksti = teksti ++ reverse teksti 

lol :: Text -> Text 
lol teksti = fromString ((toString teksti) ++ (reverse (toString teksti)))

taikaluku :: Int
taikaluku = 15

-- Int on abstrakti mutta taas Paketti ei ole koska me tiedetään mitä siellä on sisällä ja me tiedetään miten se on koodattu 
-- 
