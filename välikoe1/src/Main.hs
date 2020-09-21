module Main where
import System.Environment

-- Tässä apufunktio, jolla saadaan välimerkit kätevästi pois.
-- Sitä ei tarvitse muuttaa
filterText :: (Char -> Bool) -> Text -> Text
filterText ehto = fromString . filter ehto . toString

kielletytSanat :: [Text]
kielletytSanat = ["sekä","että","jos","vaikka","kuin","ja","niin"]
              
filterWords :: (Text -> Bool) -> [Text] -> [Text]
filterWords _ [] = []
filterWords ehto (eka:loput) 
  = let
      ekaM = filter ehto (words eka)
    in [(unwords ekaM)]++(filterWords ehto loput)
-- Täydennä tämä tehtävänannon mukaiseksi.
filterIT :: Text -> Text
filterIT syöte 
  = let
     ilmanVälimerkkejä :: Text
     ilmanVälimerkkejä 
        = filterText (\xs -> notElem xs [',','.',';',':','"','\'']) syöte
     x = lines ilmanVälimerkkejä
     ilmanTiettyjäSanoja = filterWords (\xs -> notElem xs kielletytSanat) x
    -- Tähän loput määritelmät
    in (unlines ilmanTiettyjäSanoja)


-- Tässä pääohjelma. Sitä ei tarvitse muuttaa, mutta voit kokeilla
-- ohjelmaasi sen avulla.
-- sekä 
-- moi
-- että
-- test
-- Komentoriviltä: 
--   1) `stack build` 
--   2) `stack exec <ohjelmasi nimi> src/Main.hs` (tai joku muu tekstitiedosto)
--
--  Replistä:
--   :main src/Main.hs 
--   (tai joku muu tekstitiedosto kuin src/Main.hs)
--
--   Voit myös kokeilla aliohjelmaasi ihan normaalisti replistä.
main :: IO ()
main = do
  argumentit <- getArgs
  case argumentit of
    [tiedostonNimi] -> do
              sisältö <- readFileText tiedostonNimi
              putTextLn (filterIT sisältö)
    _ -> putTextLn "Anna tiedostonime argumenttina"