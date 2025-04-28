module Lib () where

import Text.Show.Functions ()

data Personaje = unPersonaje {
    nombre :: String,
    poderBasico :: String,
    superPoder :: String,
    superPoderActivo :: Bool,
    cantidadVida :: Int,
    grupo :: String
} 

doble :: Int -> Int
doble x = x * 2

bolaEspinosa :: Personaje -> Personaje 
bolaEspinosa unPersonaje
    | cantidadVida unPersonaje <= 1000 = unPersonaje {
                            nombre = nombre unPersonaje, 
                            poderBasico = poderBasico unPersonaje,
                            superPoder = superPoder unPersonaje,
                            superPoderActivo = superPoderActivo unPersonaje,
                            cantidadVida = 0,
                            grupo = grupo unPersonaje
                            }
    | otherwise = unPersonaje {
                            nombre = nombre unPersonaje, 
                            poderBasico = poderBasico unPersonaje,
                            superPoder = superPoder unPersonaje,
                            superPoderActivo = superPoderActivo unPersonaje,
                            cantidadVida = cantidadVida unPersonaje - 1000,
                            grupo = grupo unPersonaje
                            }
