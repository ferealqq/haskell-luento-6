module Main where

n :: forall a. Num a => a
n = 1

data Väri = Punainen | Keltainen | Oranssi
  deriving (Show)

class Puoliryhmä a where
  (+++) :: a -> a -> a

instance Puoliryhmä Int where
  vasen +++ oikea = vasen + oikea

instance Puoliryhmä Väri where
  (+++) vasen oikea 
    = let 
      vasenVäri = tunnistaVäri vasen
      oikeaVäri = oikeaVäri oikea
      vastaus = case (oikeaVäri+vasenVäri) of
                    2 -> Punainen


tunnistaVäri :: Väri -> Int
tunnistaVäri väri = case väri of
        Punainen -> 1
        Keltainen -> 2
        Orange -> 3
        otherwise -> err "MurrMurrr"
-- Num on tyyppi luokka eikä tyyppi niin kuin Int, Natural tai Double.
-- n :: Num

main :: IO ()
main = do
  putStrLn "Hello TIEA341"
