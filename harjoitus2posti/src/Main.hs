module Main where

särkyvä :: Double -> Double
särkyvä alk = (alk*1.20)+5

kirjeenHinta :: Double
kirjeenHinta = 3.90

laskePaketinPintaAla :: Text -> Double
laskePaketinPintaAla a = 3.9

tulo :: [Double] -> Double
tulo [] = 1 
tulo (x:xs) = x * tulo xs 

posti :: IO ()
posti = do
  putStrLn "Postitatko kirjeen vai paketin?"
  putStrLn "Vastaa k, jos postitat kirjeen ja p jos kyseessä on paketti."
  input <- getLine 
  if input == "k" then kirje else paketti

paketti :: IO ()
paketti = do
  putStrLn "Onko pakettisi isompi kuin 20x20x20cm k/e?"
  input <- getLine 
  if input == "k" then isoPaketti else pieniPaketti


isoPaketti :: IO ()
isoPaketti = do 
  putStrLn "Kuinka iso pakettisi on?"
  putStrLn "Vastaus muodossa [pituus,leveys,korkeus] esim [30,40,50]"
  putStrLn (show x)
  --input <- getLine
  --let x = (reads (toString input) :: [(Double, String)]) 
  --putStrLn (show x)
  --putStrLn (show (take 1 x))
  --putStrLn (show (unzip x))
  --putStrLn (show (laskePaketinPintaAla input))
    
pieniPaketti :: IO()
pieniPaketti = do 
  putStrLn "Onko pakettisi särkyvä?"

kirje :: IO ()
kirje = do
  putStrLn "Onko kirje särkyvä k/e?"
  onkoSär <- getLine
  if onkoSär == "k" then putStrLn ("Se maksaa "++(show (särkyvä kirjeenHinta))++"€") else putStrLn ("Se maksaa "++(show kirjeenHinta)++"€")

-- input/output source -> http://learnyouahaskell.com/input-and-output
-- ja vittu uusiks 
-- YEEEEEEEEEEET!

main :: IO ()
main = do
  putStrLn "Hello TIEA341"
