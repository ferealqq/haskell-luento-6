module MaybeTeht where


teeMaybe :: forall a. a -> Maybe a 
teeMaybe l = Just l
onkoTyhjä :: forall a. Maybe a -> Bool
onkoTyhjä j 
    = case j of 
        Just x -> False
        Nothing -> True

onkoTäysi :: forall a. Maybe a -> Bool
onkoTäysi j = if (onkoTyhjä j) then False else True

oletusarvolla :: forall a. a -> Maybe a -> a 
oletusarvolla arvo j 
    = case j of 
        Just x -> x 
        Nothing -> arvo    

kutsuMaybellä1 :: forall a b. (a -> b) -> Maybe a -> Maybe b
kutsuMaybellä1 f ma = case ma of 
                        Just x -> Just (f x)
                        Nothing -> Nothing

kutsuMaybellä2 :: forall a b c. (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
kutsuMaybellä2 f ma mb 
   = case (ma,mb) of
      (_,Nothing) -> Nothing
      (Nothing,_) -> Nothing
      (Nothing,Nothing) -> Nothing
      (Just a, Just b) -> Just (f a b)

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


kutsuMaybellä3 :: forall a b c d. (a -> b -> c -> d) -> Maybe a -> Maybe b -> Maybe c -> Maybe d 
kutsuMaybellä3 f ma mb mc 
   = case ma of 
      Nothing -> Nothing
      Just a -> case mb of 
                  Nothing -> Nothing
                  Just b -> case mc of 
                              Nothing -> Nothing
                              Just c -> Just (f a b c) 
{-
kutsuMaybellä3 f ma mb mc 
   = let
      onkoTyhjiä = onko onkoTyhjä [ma,mb,mc]
      vastaus 
         = case onkoTyhjiä of
            True -> Nothing
            False -> Just ()
-}

{-
maybeCaseLoop :: forall a b. Int -> [Maybe a] -> (Maybe a -> Maybe b) 
maybeCaseLoop n mList 
   = case n of
      0 -> (\s -> s)
      m -> let
            ma = maybeCaseLoop (n-1) -}