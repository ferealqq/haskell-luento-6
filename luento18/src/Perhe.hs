module Perhe where

import FunktioidenYhdistäminen
import Prelude
import qualified Data.Map as Map


data Suhde = Isä | Äiti | Sisarus deriving (Eq,Ord,Show)
type Nimi = Text

type SukuPuu = Map (Nimi,Suhde) Nimi

isä :: Nimi -> SukuPuu -> Maybe Nimi
isä n = Map.lookup (n,Isä)

äiti :: Nimi -> SukuPuu -> Maybe Nimi
äiti n = Map.lookup (n,Äiti)

isoIsä :: Nimi -> SukuPuu -> Maybe Nimi
isoIsä n p = case (isä n p) of       -- Miksei isä (isä n) tai jopa isä |> isä?
                Nothing -> Nothing
                Just m  -> isä m p

isoIsä2 n sukupuu 
  = (vaihda isä sukupuu >==> vaihda isä sukupuu) n

{- 
isoÄiti :: Nimi -> SukuPuu -> Maybe Nimi
isoÄiti n p = ( äiti >==> äiti ) n p -- Miksei äiti (äiti n) tai jopa äiti |> äiti?
-}

ovatkoSerkkuja :: SukuPuu -> Nimi -> Nimi -> Maybe Bool
ovatkoSerkkuja suku n1 n2
    = case isoIsä n1 suku of
        Nothing -> Nothing
        Just iiN1 -> case isoIsä n2 suku of
                      Nothing -> Nothing
                      Just iiN2 -> Just (iiN1 == iiN2)


ovatkoSerkkuja2 :: SukuPuu -> Nimi -> Nimi -> Maybe Bool 
ovatkoSerkkuja2 = sukupuu n1 n2
  = isoIsä n1 sukupuu >>==
    where
      teeEkalleIsoIsälle isoIsäN1 = isoIsä n2 sukupuu >>== teePäätös isoIsäN1
      teePäätös isoIsäN1 isoIsäN2 = Just (isoIsäN1 == isoIsäN2)

      