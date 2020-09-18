module OmaMaybe where 


-- Just 'x' :: Maybe Char
--


data MunMaybe a = EiOle | OnVaan a 
    deriving (Eq,Ord,Show)

-- etsi :: (Natural -> Bool) -> [Natural] -> Maybe Natural
-- etsi :: forall a. (a -> Bool) -> [a] -> Maybe a 
-- etsi (\x -> x > 5) [1,2,3,7,2] 
--  7

etsi :: forall a. (a -> Bool) -> [a] -> Maybe a
etsi p lista 
    = case lista of 
        [] -> Nothing
        (eka : loput) 
            | p eka -> Just eka
            | otherwise -> etsi p loput 
-- etsi (\x -> x > 5) [1,2,3,7,2] 
--  Just 7
-- etsi (\x -> x > 50) [1,2,3,7,2]
--  Nothing

onko :: forall a. (a -> Bool) -> [a] -> Bool
onko ehto lista = case etsi ehto lista of 
                    Just x -> True
                    Nothing -> False