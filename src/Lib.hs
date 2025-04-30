module Lib () where

import Text.Show.Functions ()

data Personaje = UnPersonaje {
    nombre :: String,
    poderBasico :: String,
    superPoder :: String,
    superPoderActivo :: Bool,
    cantidadVida :: Int,
    grupo :: String
} deriving (Show)

restarVida :: Int -> Int -> Int
restarVida vidaContrincante danioGenerado
    | danioGenerado >= vidaContrincante = 0
    | otherwise = vidaContrincante - danioGenerado

hacerDanio :: Personaje -> Int -> Personaje
hacerDanio unContrincante danio = UnPersonaje {
    nombre = nombre unContrincante,
    poderBasico = poderBasico unContrincante,
    superPoder = superPoder unContrincante,
    superPoderActivo = superPoderActivo unContrincante,
    cantidadVida = restarVida (cantidadVida unContrincante) danio,
    grupo = grupo unContrincante
}

bolaEspinosa :: Personaje -> Personaje 
bolaEspinosa unContrincante = hacerDanio unContrincante 1000

espina :: Personaje
espina = UnPersonaje "Espina" "bolaEspinosa" "granadaDeEspinas 5" True 4800 "Grupo 1"