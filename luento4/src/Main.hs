module Main where

import Johdanto
import Parit 
import Funktio
import TehtParit

main :: IO ()
main = do
  putStrLn (toString (tulostaNimipari esimerkkiPari))

