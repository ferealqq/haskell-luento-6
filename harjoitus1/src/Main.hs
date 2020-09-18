module Main where

kerroKahdella :: Natural -> Natural
kerroKahdella luku = luku * 2 

onkoAnagrammi :: Text -> Text -> Bool
onkoAnagrammi sana1 sana2 = let kirjaimet1 = toString sana1 
                                kirjaimet2 = toString sana2
                            in sort kirjaimet1 == sort kirjaimet2

tulo :: [Natural] -> Natural 
tulo lista = case lista of [] -> 1
                           (numero:loput) -> numero * tulo loput 


                           
main :: IO ()
main = do
  putStrLn "Hello TIEA341"
