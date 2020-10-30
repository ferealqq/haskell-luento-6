module FunktioidenYhdistäminen where

(<|) :: forall a b c. (b -> c) -> (a -> b) -> (a -> c)
(<|) f1 f2 = funktio
  where
    funktio b = f1 (f2 b)

(|>) :: forall a b c. (a -> b) -> (b -> c) -> (a -> c)
(|>) f1 f2 = f3
  where
    f3 a = f2 (f1 a)

safeDiv :: Double -> Double -> Maybe Double
safeDiv a b
  | b /= 0 = Just (a / b)
  | otherwise = Nothing

vaihda :: forall a b c. (a -> b -> c) -> (b -> a -> c)
vaihda f = \x y -> f y x

(>==>) :: forall a b c. (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
(>==>) f1 f2 = f3
  where
    f3 n = case (f1 n) of
      Nothing -> Nothing
      Just m -> f2 m

(>>==) :: forall a b. Maybe a -> (a -> Maybe b) -> Maybe b
(>>==) ma amb = case ma of
  Nothing -> Nothing
  (Just a) -> amb a

sekvenssoi :: [Maybe Integer] -> Maybe [Integer]
sekvenssoi [] = Nothing
sekvenssoi (e : l) = foldl' fu (fu e (Just [])) l
  where
    --fu j Nothing = j >>== (\x -> Just [x])
    fu j Nothing = Nothing
    fu j (Just loput) = j >>== (\x -> Just (x : loput))