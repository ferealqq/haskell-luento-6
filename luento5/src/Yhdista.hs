module Yhdista where 


yhdistä :: forall a b c. (b -> c) -> (a -> b) -> (a -> c)
yhdistä f1 f2 = f3
    where
        f3 a = f1 (f2 a)


testi :: Natural
testi = 2