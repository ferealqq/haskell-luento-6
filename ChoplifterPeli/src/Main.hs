module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Interface.Pure.Game
import Prelude hiding (Down)

main :: IO ()
main =
  play
    (InWindow "Choplifter" (1200, 800) (500, 150))
    (light blue)
    24
    (Peli 0 (10, 0) 0 0)
    piirräPeli
    reagoi
    päivitäPeliä

reagoi :: Event -> Choplifter -> Choplifter
reagoi tapahtuma peli =
  case tapahtuma of
    EventKey (Char 'w') Down _ _ -> muutaTehoa 0.3 peli
    EventKey (Char 's') Down _ _ -> muutaTehoa (-0.3) peli
    EventKey (Char 'd') Down _ _ -> kallista 1.7 peli
    EventKey (Char 'a') Down _ _ -> kallista (-1.7) peli
    EventKey (Char 'e') Down _ _ -> kallista 2.9 peli
    EventKey (Char 'q') Down _ _ -> kallista (-2.9) peli
    _ -> peli

päivitäPeliä :: Float -> Choplifter -> Choplifter
päivitäPeliä aikaEdellisestä edellinenTila =
  case edellinenTila of
    Peli aika (x, y) teho kulma -> Peli (aika + aikaEdellisestä) (x + aika * teho, y + aika * teho) (teho * 0.97) kulma

kulmaJaTehoNopeudeksi :: Float -> Float -> (Float, Float)
kulmaJaTehoNopeudeksi teho kulma =
  rotateV (- degToRad kulma) (0, teho)

piirräPeli :: Choplifter -> Picture
piirräPeli p = case p of
  Peli aika (kopteriX, kopteriY) _teho kulma ->
    let kopteriKuva = rotate kulma (scale 0.2 0.2 (kopteri 15 aika))
        peliKuva = translate kopteriX kopteriY kopteriKuva <> maa
     in translate 0 (-180) peliKuva

muutaTehoa :: Float -> Choplifter -> Choplifter
muutaTehoa paljon peli =
  case peli of
    Peli aika paikka teho kulma -> Peli aika paikka (teho + paljon) kulma

kallista :: Float -> Choplifter -> Choplifter
kallista muutos peli =
  case peli of
    Peli aika paikka nopeus kulma -> Peli aika paikka nopeus (kulma + muutos)

data Choplifter
  = Peli
      Float --aika
      (Float, Float) --missä
      (Float) -- teho
      (Float) -- kuinka vinossa

maa :: Picture
maa = color green (translate 0 (-700) (rectangleSolid 5000 1000))

kopteri :: Float -> Float -> Picture
kopteri teho aika = translate 0 (150) (color white runko)
  where
    runko =
      circleSolid 100
        <> translate (-200) 0 (rectangleSolid 300 30)
        <> translate (-350) 0 (rectangleSolid 50 100)
        <> lapa
        <> translate 0 90 (rectangleSolid 10 120)
        <> translate (-50) (-90) (rectangleSolid 10 120)
        <> translate (50) (-90) (rectangleSolid 10 120)
        <> translate 0 (-150) (rectangleSolid 200 15)
    lapa = translate 0 150 (rectangleSolid (350 * sin (aika * teho)) 10)
