module Main where

seuraavaKirjain :: Char -> Int -> Char
seuraavaKirjain kirjain mones = let kirjanKoodi = ord kirjain
                                in chr (kirjanKoodi+mones)

ceasar :: Int -> Text -> Text 
ceasar monta sana 
 = let
    lista = toString sana
    salattu = map (\kirjain -> seuraavaKirjain kirjain monta) lista
    salattuText = fromString salattu
   in salattuText

avain :: [Int]
avain = [1,3,2]

vigenere :: Text -> Text
vigenere sana
 = let 
    lista = toString sana
    avainListInf = cycle avain
    salattu = zipWith seuraavaKirjain lista avainListInf 
    salattuText = fromString salattu
  in salattuText


main :: IO ()
main = do
  putStrLn "Hello TIEA341"