module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Interface.Pure.Game
import Prelude hiding (Down)

alkutilanne :: PeliTilanne
alkutilanne =
  ( GameOn
      ( Peli
          0
          (10, 0)
          (0, 0)
          0
          0
          [Talo 800 500 700]
          [Hemmo (800, 700), Hemmo (900, 800)]
      )
  )

main :: IO ()
main =
  play
    (InWindow "Choplifter" (400, 400) (200, 200))
    (light blue)
    24
    alkutilanne
    piirräPeliTilanne
    reagoiPeliTilanne
    päivitäPelitilanne

reagoiPeliTilanne :: Event -> PeliTilanne -> PeliTilanne
reagoiPeliTilanne tapahtuma pelitilanne =
  case pelitilanne of
    GameOver cl -> GameOver cl
    GameOn cl -> GameOn (reagoi tapahtuma cl)

reagoi :: Event -> Choplifter -> Choplifter
reagoi tapahtuma peli =
  case tapahtuma of
    EventKey (Char 'w') Down _ _ -> muutaTehoa 1.5 peli
    EventKey (Char 's') Down _ _ -> muutaTehoa (-1.5) peli
    EventKey (Char 'a') Down _ _ -> kallista (-8) peli
    EventKey (Char 'd') Down _ _ -> kallista (8) peli
    _ -> peli

onkoHyväLaskeutuminen :: Vector -> Float -> Bool
onkoHyväLaskeutuminen nopeus kulma
  | magV nopeus <= 300 && (abs kulma <= 25 || abs kulma <= 385) = True
  | otherwise = False

päivitäPelitilanne :: Float -> PeliTilanne -> PeliTilanne
päivitäPelitilanne aikaEdellisestä pelitilanne =
  case pelitilanne of
    GameOver cl -> GameOver cl
    GameOn cl -> case törmääköTaloon (cl_paikka cl) (cl_kulma cl) (cl_talot cl) of
      Nothing -> GameOn (päivitäPeliä aikaEdellisestä cl)
      Just Roottori -> GameOver cl
      Just Laskuteline
        | onkoHyväLaskeutuminen (cl_nopeus cl) (cl_kulma cl) ->
          GameOn (päivitäPeliä aikaEdellisestä cl {cl_kulma = 0, cl_nopeus = pysäytäPystyssä (cl_nopeus cl)})
        | otherwise -> GameOver cl

pysäytäPystyssä :: Vector -> Vector
pysäytäPystyssä (vx, vy) = (vx, max 0 vy)

päivitäPeliä :: Float -> Choplifter -> Choplifter
päivitäPeliä aikaEdellisestä edellinenTila =
  case edellinenTila of
    Peli
      aika
      (kopteriX, kopteriY)
      (vX, vY)
      teho
      kulma
      talot
      hemmot ->
        let (dX, dY) = kulmaJaTehoKiihtyvyydeksi teho kulma
         in Peli
              (aika + aikaEdellisestä)
              ( kopteriX + aikaEdellisestä * vX,
                max 0 (kopteriY + aikaEdellisestä * vY)
              )
              ((vX + dX) * 0.97, (vY + dY - 5) * 0.97)
              teho
              kulma
              talot
              (map (päivitäHemmoa edellinenTila) hemmot)

kulmaJaTehoKiihtyvyydeksi :: Float -> Float -> (Float, Float)
kulmaJaTehoKiihtyvyydeksi teho kulma =
  rotateV (- degToRad kulma) (0, teho)

kopteriTörmäysviivat :: Point -> Float -> ((Point, Point), (Point, Point))
kopteriTörmäysviivat paikka kulma =
  let vasen = -120
      oikea = 50
      kääntö = rotateV (- degToRad kulma)
   in ( ( kääntö (vasen, 0) #+ paikka,
          kääntö (oikea, 0) #+ paikka
        ),
        ( kääntö (vasen, 120) #+ paikka,
          kääntö (oikea, 120) #+ paikka
        )
      )

data TörmäysKohta = Laskuteline | Roottori
  deriving (Eq, Ord, Show)

törmääköTaloon :: Point -> Float -> [Talo] -> Maybe TörmäysKohta
törmääköTaloon paikka kulma talot = fmap maximum1 (nonEmpty (mapMaybe törmääköYhteen talot))
  where
    -- case (nonEmpty (mapMaybe törmääköYhteen talot)) of
    --  Nothing -> Nothing
    --  Just kohdat -> Just (maximum1 kohdat)

    törmääköYhteen talo =
      let ((ala1, ala2), (ylä1, ylä2)) = kopteriTörmäysviivat paikka kulma
          (va, oy) = nurkkaPisteet talo
       in case (not (segClearsBox ala1 ala2 va oy), not (segClearsBox ylä1 ylä2 va oy)) of
            (True, False) -> Just Laskuteline
            (False, False) -> Nothing
            _ -> Just Roottori

(#+) :: Point -> Vector -> Point
(a, b) #+ (x, y) = (a + x, b + y)

(#-) :: Point -> Point -> Vector
(a, b) #- (x, y) = (a - x, b + y)

piirräPeliTilanne :: PeliTilanne -> Picture
piirräPeliTilanne pelitilanne =
  case pelitilanne of
    GameOver cl -> piirräPeli cl <> translate (-300) 0 (color yellow (text "GAME OVER"))
    GameOn cl -> piirräPeli cl

piirräPeli :: Choplifter -> Picture
piirräPeli peli =
  let kulma = cl_kulma peli
      aika = cl_aika peli
      (kopteriX, kopteriY) = cl_paikka peli
      teho = cl_teho peli
      talot = cl_talot peli

      ((va, oa), (va1, oa1)) = kopteriTörmäysviivat (kopteriX, kopteriY) kulma
      apuviivaAla = color red (line [va, oa])
      apuviivaYlä = color red (line [va1, oa1])

      kopterikuva = rotate kulma (scale 0.4 0.4 (kopteri teho aika))

      piirretytTalot = map piirräTalo talot

      hemmot = map (piirräHemmo aika) (cl_hemmot peli)

      debugViesti =
        scale
          0.8
          0.8
          (text (show (magV (cl_nopeus peli)) <> "  " <> show kulma))

      peliKuva =
        translate kopteriX kopteriY kopterikuva
          <> maa
          <> pictures piirretytTalot
          <> pictures hemmot
          <> apuviivaAla
          <> apuviivaYlä
          <> debugViesti
   in scale 0.25 0.25 (translate 0 (-180) peliKuva)

kallista :: Float -> Choplifter -> Choplifter
kallista muutos peli = peli {cl_kulma = muutos + cl_kulma peli}

muutaTehoa :: Float -> Choplifter -> Choplifter
muutaTehoa muutos peli = peli {cl_teho = muutos + cl_teho peli}

---       ↑
--     cl_teho :: Choplifter -> Float

data PeliTilanne = GameOver Choplifter | GameOn Choplifter

data Choplifter = Peli
  { -- | Aika pelin alusta
    cl_aika :: Float,
    -- | Missä kopteri?
    cl_kopteri :: Kopteri,
    cl_talot :: [Talo], -- Esteet pelissä
    cl_hemmot :: [Hemmo]
  }

data Kopteri = Kopteri
  { kop_paikka :: (Float, Float),
    -- | Kuinka nopeasti menee?
    kop_nopeus :: (Float, Float),
    -- | Teho
    kop_teho :: Float,
    -- | Kuinka vinossa
    kop_kulma :: Float
  }

cl_paikka :: Choplifter -> Point
cl_paikka peli = kop_paikka . cl_kopteri

cl_nopeus :: Choplifter -> Point
cl_nopeus peli = kop_paikkka . cl_kopteri

cl_teho :: Choplifter -> Float
cl_teho peli = kop_teho . cl_kopteri

cl_kulma :: Choplifter -> Float
cl_kulma peli = kop_kulma . cl_kopteri

korkeusKohdassa :: Float -> Choplifter -> Float
korkeusKohdassa kohta peli =
  maybe 0 maximum1 . nonEmpty . map osuukoTaloon . cl_talot $ peli
  where
    osuukoTaloon :: Talo -> Float
    osuukoTaloon talo
      | abs (talo_sijainti talo - kohta) < (talo_leveys talo / 2) =
        talo_korkeus
          talo
      | otherwise = 0

data Talo = Talo
  { talo_korkeus :: Float,
    talo_leveys :: Float,
    talo_sijainti :: Float
  }

data Hemmo = Hemmo {hemmo_sijainti :: Point}

päivitäHemmoa :: Choplifter -> Hemmo -> Hemmo
päivitäHemmoa peli hemmo
  | liikkuu = hemmo {hemmo_sijainti = hemmo_sijainti hemmo #+ (suunta, 0)}
  | otherwise = hemmo
  where
    kopterinPaikka = cl_paikka peli
    liikkuu = haluaaLiikkua && not putoaako
    korkeusEdessä = korkeusKohdassa (fst (hemmo_sijainti hemmo) + suunta * 5) peli
    putoaako = abs (korkeusEdessä - snd (hemmo_sijainti hemmo)) < 10
    haluaaLiikkua = magV (kopterinPaikka #- hemmo_sijainti hemmo) < 60
    suunta
      | fst kopterinPaikka < fst (hemmo_sijainti hemmo) = -5
      | otherwise = 5

piirräHemmo :: Float -> Hemmo -> Picture
piirräHemmo aika hemmo =
  let (x, y) = hemmo_sijainti hemmo
      lantio = (-10, 40)
      vasenJalka = 15 + sin (12 * aika) * 7
      oikeaJalka = 15 + cos (12 * aika) * 7
      hemmoKuva =
        color
          white
          ( translate 0 120 (circleSolid 20)
              <> line [(0, 120), lantio] -- selkä
              <> line
                [ (-40, 90 + cos (8 * aika + 0.3) * 40),
                  (-30, 90),
                  (30, 90),
                  (40, 90 + cos (8 * aika) * 40) -- kädet
                ]
              <> line
                [ (-25, vasenJalka),
                  (-20, vasenJalka),
                  lantio,
                  (30, oikeaJalka),
                  (35, oikeaJalka) --jalat
                ]
          )
   in translate x y hemmoKuva

--- Tästä alaspäin piirtofunktioita

piirräTalo :: Talo -> Picture
piirräTalo talo =
  let paikoillaan = translate (talo_sijainti talo) (talo_korkeus talo / 2) talonKuva
      talonKuva =
        color
          (greyN 0.5)
          (rectangleSolid (talo_leveys talo) (talo_korkeus talo))

      ((vax, vay), (oyx, oyy)) = nurkkaPisteet talo
      apupisteet =
        translate vax vay (color red (circleSolid 10))
          <> translate oyx oyy (color red (circleSolid 10))
   in paikoillaan <> apupisteet

-- type Point = (Float,Float)
nurkkaPisteet :: Talo -> (Point, Point)
nurkkaPisteet talo =
  let vasenAla = (talo_sijainti talo - (talo_leveys talo / 2), 0)
      oikeaYlä = (talo_sijainti talo + (talo_leveys talo / 2), talo_korkeus talo)
   in (vasenAla, oikeaYlä)

maa :: Picture
maa = color green (translate 0 (-500) (rectangleSolid 5000 1000))

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
