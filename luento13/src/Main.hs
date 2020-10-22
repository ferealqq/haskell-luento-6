module Main where

maksimiAcc :: forall a. Ord a => [a] -> Maybe a
maksimiAcc [] = Nothing
maksimiAcc lista = maksimiApu Nothing lista
  where
    maksimiApu :: Maybe a -> [a] -> Maybe a
    maksimiApu välitulos [] = välitulos
    maksimiApu välitulos (ekaVäli : loputVäli) =
      maksimiApu (päivitäVälitulos välitulos ekaVäli) loputVäli

    päivitäVälitulos :: Maybe a -> a -> Maybe a
    päivitäVälitulos välitulos ekaPäivitä = case välitulos of
      (Just arvo) -> case compare arvo ekaPäivitä of
        EQ -> Just ekaPäivitä
        LT -> Just ekaPäivitä
        GT -> Just arvo
      Nothing -> Just ekaPäivitä

main :: IO ()
main = do
  putStrLn "Hello TIEA341"
