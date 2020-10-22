module Tietotyypit where

-- palveleSivua :: Server -> IO void
--      Jonkin näkönen päättymätön silmukka

data Stream a = Next a (Stream a) deriving (Show)

luonnollisetLuvut :: Natural -> Stream Natural
luonnollisetLuvut n = Next n (luonnollisetLuvut (n + 1))

summaa :: Stream Natural -> Stream Natural -> Stream Natural
summaa (Next x loput1) (Next y loput2) = Next (x + y) (summaa loput1 loput2)

-- summaa (Next 3 (luonnollisetLuvut 3)) (Next 3 (luonnollisetLuvut 3))