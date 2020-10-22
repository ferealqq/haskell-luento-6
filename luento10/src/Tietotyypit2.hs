{-# LANGUAGE TypeApplications #-}

module Tietotyypit2 where

-- :t id
-- -- id :: forall {a}. a -> a

-- :t 15
-- -- 15 :: forall {p}. Num p -> p

-- :t id 15
-- -- id 15 :: forall {a}. Num a => a

--Esim. snd :: forall a. forall b. (a,b) -> b on 3 parametrin funktio:
-- -- Vähän kuin snd :: (a :: Type) -> (b :: Type) -> (a,b) -> b
-- -- Kutsu menisi näin: snd Int Bool (42, True)

test :: forall a. forall b. (a, b) -> b
test (_, b) = b

--         forall eka toka. (eka, toka) -> toka
--         Sama asia
otaToka :: forall eka. forall toka. (eka, toka) -> toka
otaToka (_, b) = b

otaIntBoolinBool :: (Int, Bool) -> Bool
otaIntBoolinBool = otaToka @Int @Bool

{-
length :: forall t a. Foldable t => t a -> Int
group :: forall a. Eq a => [a] -> [[a]]
words :: Text -> [Text]
comparing :: forall a b. Ord a => (b -> a) -> b -> b -> Ordering
sortBy :: forall a. (a -> a -> Ordering) -> [a] -> [a]
-}
-- Alkuperäinen muokkasin itse.
pisimmät :: Text -> [[String]]
pisimmät syöte =
  let sanat = map toString (words syöte)
      vertailu = comparing (length)
      järjestyksessä = sortBy (vertailu) sanat
      ryhmät = group (reverse järjestyksessä)
   in ryhmät

{-
words :: Text -> [Text]
sort :: forall a. Num a => [a] -> [a]
take :: forall a. Int -> [a] -> [a]
reverse :: forall a. [a] -> [a]
-}
-- Tauon jälkeinen versio
pisimmätB :: Text -> [Text]
pisimmätB syöte =
  let sanat = words syöte
      järjestyksessä = sort @Text sanat
      käänteisessä = reverse @Text järjestyksessä
      ekat = take @Text 5 käänteisessä
   in ekat