module Main where

import Data.Char -- Tämä heti module rivin alle
import Test.QuickCheck

data Osa = Eka | Toka | Kolmas

modulo3 :: Int -> Osa
modulo3 n = case mod n 3 of
  1 -> Eka
  2 -> Toka
  _ -> Kolmas

kirjainLuokka :: Char -> Osa
kirjainLuokka c
  | isUpper c = Eka
  | isSpace c = Kolmas
  | otherwise = Toka

jaaKolmeenOsaan :: forall a. (a -> Osa) -> [a] -> ([a], [a], [a])
jaaKolmeenOsaan p = foldr f tyhjä
  where
    tyhjä = ([], [], [])
    f a (ekat, tokat, kolmannet) =
      case (p a) of
        Eka -> (a : ekat, tokat, kolmannet)
        Toka -> (ekat, a : tokat, kolmannet)
        otherwise -> (ekat, tokat, a : kolmannet)

maksimi :: forall a. (Num a, Ord a) => [a] -> Maybe a
maksimi al = Just (foldl' max 0 al)

mäppää :: forall a b. (a -> b) -> [a] -> [b]
mäppää f a = foldr fun [] a
  where
    fun eka loput = f eka : loput

pituus :: forall a. [a] -> Natural
pituus a = foldr fun 0 a
  where
    fun _n s = 1 + s

monesko :: forall a. (Eq a) => a -> [a] -> Maybe Natural
monesko n ar = eka (foldr f (1, [], False) ar)
  where
    eka k =
      case k of
        (a, _, _) -> Just a
        otherwise -> Nothing

    f e (i, l, ollueq) = case ollueq of
      True -> (i, [], True)
      False -> (i + 1, l, n == e)

main :: IO ()
main = do
  putStrLn "Hello TIEA341"
