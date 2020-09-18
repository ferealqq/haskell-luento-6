module TehtFunktiot where 

--testiFunktio sana = (\x -> reverse (toString x)) sana
--testiSana = "kissa" 

kutsu3Kertaa :: forall a. (a -> a) -> a -> a
kutsu3Kertaa seFunktio seA
    = let
        kutsuttuKerran = seFunktio seA
        kutsuttuToisenKerran = seFunktio kutsuttuKerran
        kutsuttuKolmeKertaa = seFunktio kutsuttuToisenKerran
      in kutsuttuKolmeKertaa

testFunktio x = x > 10 

kutsuParia :: forall a b. (a, a -> b) -> b
kutsuParia (arvo,lauseke) = lauseke arvo

molemmille :: forall a t. (a -> t) -> (a, a) -> (t, t)
molemmille funktio pari 
 = case pari of 
     (x,y) -> (funktio x, funktio y)
     
kutsuNKertaa :: forall a. Natural -> (a -> a) -> (a -> a)
kutsuNKertaa n funktio = case n of 
                            0 -> (\x -> x)
                            n2 -> let
                                    kutsuttuNMiinusYksiKertaa = kutsuNKertaa (n2-1) funktio
                                  in (\x -> kutsuttuNMiinusYksiKertaa (funktio x) )