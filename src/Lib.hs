module Lib () where

import Text.Show.Functions ()

data Personaje = UnPersonaje {
    nombre :: String,
    poderBasico :: Personaje -> Personaje,
    superPoder :: Personaje -> Personaje,
    superPoderActivo :: Bool,
    cantidadVida :: Int
} deriving (Show)
-- si los poderes se definen como :: Personaje -> Personaje, puedo utilizar la data
-- guardada en el personaje como si fuera la funcion misma definida en mi codigo

espina :: Personaje
espina = UnPersonaje "Espina" bolaEspinosa (granadaDeEspinas 5) True 4800

pamela :: Personaje
pamela = UnPersonaje "Pamela" (lluviaDeTuercas "Sanadoras") torretaCurativa False 9600

restarVida :: Int -> Int -> Int
restarVida vidaContrincante danioGenerado
    | danioGenerado >= vidaContrincante = 0
    | otherwise = vidaContrincante - danioGenerado

sumarVida :: Int -> Int -> Int
sumarVida vidaColega sanacionGenerada = vidaColega + sanacionGenerada

hacerDanio :: Int -> Personaje -> Personaje
hacerDanio danio unContrincante = UnPersonaje {
    nombre = nombre unContrincante,
    poderBasico = poderBasico unContrincante,
    superPoder = superPoder unContrincante,
    superPoderActivo = superPoderActivo unContrincante,
    cantidadVida = restarVida (cantidadVida unContrincante) danio
}

sanarColega :: Int -> Personaje -> Personaje
sanarColega sanacion unColega = UnPersonaje {
    nombre = nombre unColega,
    poderBasico = poderBasico unColega,
    superPoder = superPoder unColega,
    superPoderActivo = superPoderActivo unColega,
    cantidadVida = sumarVida (cantidadVida unColega) sanacion
}

cambiarSuperActivo :: Personaje -> Personaje
cambiarSuperActivo unColega = UnPersonaje {
    nombre = nombre unColega,
    poderBasico = poderBasico unColega,
    superPoder = superPoder unColega,
    superPoderActivo = not (superPoderActivo unColega),
    cantidadVida = cantidadVida unColega
}

cambiarNombre :: String -> Personaje -> Personaje
cambiarNombre agregarEnNombre unContrincante = UnPersonaje {
    nombre = nombre unContrincante ++ agregarEnNombre,
    poderBasico = poderBasico unContrincante,
    superPoder = superPoder unContrincante,
    superPoderActivo = superPoderActivo unContrincante,
    cantidadVida = cantidadVida unContrincante
}

bolaEspinosa :: Personaje -> Personaje 
bolaEspinosa unContrincante = hacerDanio 1000 unContrincante

torretaCurativa :: Personaje -> Personaje
torretaCurativa unColega = (cambiarSuperActivo . sanarColega (cantidadVida unColega)) unColega

lluviaDeTuercas :: String -> Personaje -> Personaje
lluviaDeTuercas unTipoTuercas unPersonaje
    | unTipoTuercas == "Sanadoras" = sanarColega 800 unPersonaje
    | unTipoTuercas == "Daninas" = hacerDanio (cantidadVida unPersonaje `div` 2) unPersonaje
    | otherwise = unPersonaje

granadaDeEspinas :: Int -> Personaje -> Personaje
granadaDeEspinas radioExplosion unContrincante
    | radioExplosion > 3 && cantidadVida unContrincante < 800 = (cambiarNombre " Espina estuvo aqui" . hacerDanio (cantidadVida unContrincante)) (cambiarSuperActivo unContrincante)
    | radioExplosion > 3 = (bolaEspinosa . cambiarNombre " Espina estuvo aqui") unContrincante
    | otherwise = bolaEspinosa unContrincante

listaPersonajes :: [Personaje]
listaPersonajes = [espina, pamela]

nombrePersonaje :: Personaje -> String
nombrePersonaje unPersonaje = nombre unPersonaje

vidaMenor800 :: Personaje -> Bool
vidaMenor800 unPersonaje = cantidadVida unPersonaje < 800

personajesEnLasUltimas :: [Personaje] -> [String]
personajesEnLasUltimas listaPersonajes = (map nombrePersonaje . filter vidaMenor800) listaPersonajes

ataqueConPoderEspecial :: Personaje -> Personaje -> Personaje
ataqueConPoderEspecial unPersonaje unContrincante
    | superPoderActivo unPersonaje = (superPoder unPersonaje . poderBasico unPersonaje) unContrincante
    | otherwise = unContrincante