module Lib () where

import Text.Show.Functions ()

data Personaje = UnPersonaje {
    nombre :: String,
    poderBasico :: String,
    superPoder :: String,
    superPoderActivo :: Bool,
    cantidadVida :: Int,
    grupo :: String
} deriving (Show, Eq)

restarVida :: Int -> Int -> Int
restarVida vidaContrincante danioGenerado
    | danioGenerado >= vidaContrincante = 0
    | otherwise = vidaContrincante - danioGenerado

sumarVida :: Int -> Int -> Int
sumarVida vidaColega sanacionGenerada = vidaColega + sanacionGenerada

hacerDanio :: Personaje -> Int -> Personaje
hacerDanio unContrincante danio = UnPersonaje {
    nombre = nombre unContrincante,
    poderBasico = poderBasico unContrincante,
    superPoder = superPoder unContrincante,
    superPoderActivo = superPoderActivo unContrincante,
    cantidadVida = restarVida (cantidadVida unContrincante) danio,
    grupo = grupo unContrincante
}

sanarColega :: Personaje -> Int -> Personaje
sanarColega unColega sanacion = UnPersonaje {
    nombre = nombre unColega,
    poderBasico = poderBasico unColega,
    superPoder = superPoder unColega,
    superPoderActivo = superPoderActivo unColega,
    cantidadVida = sumarVida (cantidadVida unColega) sanacion,
    grupo = grupo unColega
}

mismoGrupo :: Personaje -> Personaje -> Bool
mismoGrupo unPersonaje otroPersonaje = grupo unPersonaje == grupo otroPersonaje

tuercasSanadoras :: Personaje -> Bool
tuercasSanadoras unPersonaje = superPoder unPersonaje == "lluviaDeTuercas Sanadoras" || poderBasico unPersonaje == "lluviaDeTuercas Sanadoras"

tuercasDaninas :: Personaje -> Bool
tuercasDaninas unPersonaje = superPoder unPersonaje == "lluviaDeTuercas Daninas" || poderBasico unPersonaje == "lluviaDeTuercas Daninas"

bolaEspinosa :: Personaje -> Personaje 
bolaEspinosa unContrincante = hacerDanio unContrincante 1000

lluviaDeTuercas :: Personaje -> Personaje -> Personaje
lluviaDeTuercas unPersonaje otroPersonaje
    | tuercasSanadoras unPersonaje && mismoGrupo unPersonaje otroPersonaje = sanarColega otroPersonaje 800
    | tuercasDaninas unPersonaje && not (mismoGrupo unPersonaje otroPersonaje) = hacerDanio otroPersonaje (cantidadVida otroPersonaje `div` 2)
    | otherwise = unPersonaje

espina :: Personaje
espina = UnPersonaje "Espina" "bolaEspinosa" "granadaDeEspinas 5" True 4800 "LosSapos"

pamela :: Personaje
pamela = UnPersonaje "Pamela" "lluviaDeTuercas Sanadoras" "torretaCurativa" False 9600 "LasRanas"