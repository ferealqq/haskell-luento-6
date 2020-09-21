module Main where

saakoOsallistua :: Int -> Bool -> Bool
saakoOsallistua ikä konsertti 
  | konsertti = ikä > 18
  | otherwise = True

{-
kutsuMaybellä3 :: forall a b c d. (a -> b -> c -> d) -> Maybe a -> Maybe b -> Maybe c -> Maybe d
kutsuMaybellä3 funktio (Just mA) (Just mB) (Just mC) = funktio mA mB mC
kustuMaybellä3 _ _ _ _ = Nothing
-}

ainaTrue :: Bool -> Bool -> Bool
ainaTrue x y = x || y 

laskeEpätyhjätRivit :: Text -> Natural
laskeEpätyhjätRivit txt 
  = let 
      rivit = lines txt
      eiTyhjiä = filter onkoEpätyhjä rivit
      onkoEpätyhjä teksti 
        | teksti == "" = False 
        | otherwise    = True
    in genericLength eiTyhjiä

sanojaKeskimäärin :: Text -> Maybe Double 
sanojaKeskimäärin txt 
    | rivienMäärä > 0 = Just keskiarvo
    | otherwise       =  Nothing
  where 
    rivienMäärä, sanojenMäärä,keskiarvo :: Double
    rivienMäärä = genericLength (lines txt)
    sanojenMäärä = genericLength (words txt)
    keskiarvo = sanojenMäärä / rivienMäärä
  
mitta :: forall a. [a] -> Natural
mitta [] = 0
mitta (_:loput) = 1+(mitta loput)

kaikkiOnTrue :: [Bool] -> Bool
kaikkiOnTrue [] = True
kaikkiOnTrue (eka:loput) = if eka then kaikkiOnTrue loput else False

kaikkiOnTrueB :: [Bool] -> Bool
kaikkiOnTrueB [] = True
kaikkiOnTrueB (eka:loput)
    | eka       = kaikkiOnTrueB loput
    | otherwise = False

mäppää :: forall a b. (a -> b) -> [a] -> [b]
mäppää funktio lista 
  = case lista of
        [] -> []
        (eka:loput) -> [(funktio eka)]++(mäppää funktio loput)

suodata :: forall a. (a -> Bool) -> [a] -> [a]
suodata _ [] = []
suodata funktio (eka:loput)
    | funktio eka = [eka]++(suodata funktio loput)
    | otherwise            = suodata funktio loput

suodataB :: forall a. (a -> Bool) -> [a] -> [a]
suodataB _ [] = []
-- ei toimi oikein jättää ekan jostain syystä pois
suodataB funktio (eka:loput) = vastaus
  where
    vastaus = if (funktio eka == True) then [eka]++sLoput else sLoput
      where
        sLoput = suodataB funktio loput  

suodataC :: forall a. (a -> Bool) -> [a] -> [a]
suodataC funktio lista 
  = case lista of
          [] -> []
          (eka:loput) -> let
                            sLoput = suodata funktio loput
                            vastaus = case funktio eka of 
                                          True -> [eka]++sLoput
                                          False -> sLoput
                         in vastaus


main :: IO ()
main = do
  putStrLn "Hello TIEA341"
