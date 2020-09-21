module Lista where

mapTest :: [Integer] -> [Integer] 
mapTest lista = map succ lista

-- 1 : 2 : (3 : 4) : [] :: [Int]
-- [1 .. 10 ] :: [Int]
-- ['a' .. 'z'] 

-- [ 2*x | x <- [1 .. 10 :: Int]] = map (\x -> 2*x) [1 .. 10 :: Int]

-- zip [1..10::Int] ['a'..'z']
-- length it (Kannattaa käyttää vähän koska length käy koko listan läpi joka tekee siitä hitaan. Tulee paljon rekursiota)

-- let j = group [1,1,2,2,3,4,4,5,6,6,7,8,8,8]
-- [[1,1],[2,2],[3],[4,4],[5],[6,6],[7],[8,8,8]]
-- length j

-- words "kissa istuu aidalla"
-- ["kissa","istuu","aidalla"]

-- lines "kissa istuu\n aidalla"
-- ["kissa istuu","aidalla"]


-- intersperse "," (words "kissa istuu aidalla")
-- ["kissa",",","istuu",",","aidalla"]

-- intercalate (","::Text) (words "kissa istuu aidalla"::Text)
-- https://hackage.haskell.org/package/relude
-- preludesta löytyy paljon funktioita joita voi käyttää hyväksi

-- filter (\x -> x > 5) [1..10::Int]

laskeRivit :: Text -> Natural
laskeRivit txt = genericLength (lines txt)
{-
laskeRivejä :: [Char] -> Natural
laskeRivejä [] = 0
laskeRivejä lista = case lista of 
                        [] -> 0
                        (eka:loput) -> 1+(laskeRivejä loput)

epätyhjiäRivejä :: Text -> Natural
epätyhjiäRivejä txt 
    = let 
        linesLst = lines txt
        vastaus = laskeRivejä (map toString linesLst) 
      in vastaus
-}
otaEkaAlkio :: forall a. [a] -> Maybe a 
otaEkaAlkio lista 
    = case lista of
        [] -> Nothing
        (eka:_) -> Just eka

otaEkaAlkiob :: forall a. [a] -> Maybe a
otaEkaAlkiob [] = Nothing
otaEkaAlkiob (eka:_) = Just eka

epätyhjiäRivejä :: Text -> Natural
epätyhjiäRivejä teksti = genericLength (filter (\x -> x /= "") (lines teksti))

sanaaRivillä :: Text -> [Natural]
sanaaRivillä teksti 
    = let
        rivit = lines teksti
        sanat = map words rivit
        pituudet = map genericLength sanat
      in map fromIntegral pituudet

summaB :: forall a. Num a => [a] -> a
summaB [] = 0
summaB (eka:loput) 
    = let
        loputSumma = summaB loput 
      in eka+loputSumma

sanaaKeskimäärin :: Text -> Maybe Natural
sanaaKeskimäärin teksti 
    = let
        sanatRivillä = sanaaRivillä teksti
        vastaus = (fromIntegral (summaB sanatRivillä))/(fromIntegral (genericLength sanatRivillä))
      in Just (round vastaus)

mitta :: forall a. [a] -> Int 
mitta [] = 0 
mitta (eka:loput) 
    = let
        loputMitta = mitta loput
      in loputMitta+1

kaikkiOnTrue :: [Bool] -> Bool 
kaikkiOnTrue [] = True
kaikkiOnTrue (eka:loput) 
    = case eka of   
        False -> False
        True -> kaikkiOnTrue loput

mäppää :: forall a b. (a -> b) -> [a] -> [b]
mäppää funktio [] = []
mäppää funktio (eka:loput) = [(funktio eka)]++(mäppää funktio loput)

suodata :: forall a. (a -> Bool) -> [a] -> [a]
suodata funktio [] = []
suodata funktio (eka:loput) = if (funktio eka) then [eka]++(suodata funktio loput) else suodata funktio loput