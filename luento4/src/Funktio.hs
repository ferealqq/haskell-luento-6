module Funktio where

pariFunk :: (Natural->Bool,Bool)
pariFunk = (even, True)

pariFunk2 :: (Natural->Bool,Bool)
pariFunk2
    = let 
        f x = case x of 
            0 -> True
            _ -> False
      in (f, True)

pariFunk3 :: (Natural -> Bool, Bool)
pariFunk3 = (f, True)
    where
        f x = x > 10

pariFunk4 :: (Natural -> Bool, Bool)
pariFunk4 = ( (\x -> x > 20), False )

-- (\x -> x > 20) (34 :: Int)
-- x :=43
-- 42 > 20

funktioArgumenttina :: (Natural -> Natural) -> Bool 
funktioArgumenttina seFunktio = seFunktio 100 > 1000 


-- funktioArgumenttina (\x -> x * 15)
-- True
-- funktioArugmenttina (\x -> x +1)
-- False 

funktioPaluuarvona :: Natural -> (Natural -> Bool)
funktioPaluuarvona n
    = case n of 
        0 -> even
        1 -> odd 
        2 -> (\x -> x+1 > 10)
        _ -> f
      where 
        f x = x < 10

-- let joku = funktioPaluuarvona 2
-- :t joku
-- joku :: Natural -> Bool 
-- joku 2 
-- False


