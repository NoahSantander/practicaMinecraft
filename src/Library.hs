module Library where
import PdePreludat
import Data.List (intersect)

--Defino mis tipos-Comienzo---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
type Material = String
type Tiempo = Number
type Receta = ([Material], Tiempo)
type Recetas = [(Material, Receta)]
type Puntaje = Number
type Requisito = String
type Bioma = ([Material], Requisito)
type Precision = Number
type Herramienta = [(Material, Precision)]

data Personaje = UnPersonaje{
    nombre :: String, 
    puntaje :: Number,
    inventario :: [Material]
} deriving Show
--Defino mis tipos-Fin---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Craft-Comienzo-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Punto 1-Comienzo---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
esElObjeto :: [Material] -> Material -> Bool
esElObjeto receta material = elem material receta

ponerMaterial :: Bool -> Material -> [Material]
ponerMaterial bool material
    |bool = [material]
    |otherwise = []

esLaReceta :: Receta -> (Material, Receta) -> Bool
esLaReceta receta (nombre, recetaRecetas) = receta == recetaRecetas

ponerObjetoCrafteado :: Receta -> Material
ponerObjetoCrafteado receta = (fst.head)(filter (esLaReceta receta) recetas)

eliminarObjetos :: Receta -> [Material]-> [Material]
eliminarObjetos receta [] = []
eliminarObjetos receta (material:restoInventario) = ponerMaterial (not(esElObjeto (fst receta) material)) material++eliminarObjetos receta restoInventario

calcularPuntajeReceta :: Receta -> Puntaje
calcularPuntajeReceta (materiales, tiempo) = tiempo * 10

puedeCraftear :: [Material] -> Receta -> Bool
puedeCraftear inventario receta = intersect inventario (fst receta) == fst receta

eliminarObjetosInventario :: Receta -> [Material] -> [Material]
eliminarObjetosInventario = eliminarObjetos 

craftearObjeto :: Personaje -> Receta -> Personaje
craftearObjeto (UnPersonaje nombre puntaje inventario) receta 
    |puedeCraftear inventario receta = UnPersonaje nombre (puntaje + calcularPuntajeReceta receta) (ponerObjetoCrafteado receta:eliminarObjetosInventario receta inventario)
    |otherwise = UnPersonaje nombre (puntaje - 100) inventario
--Punto 1-Fin---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Punto 2-Comienzo---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
crafterSucesivamente :: Personaje -> [Receta] -> Personaje
crafterSucesivamente personaje [] = personaje
crafterSucesivamente personaje (x:xs) = crafterSucesivamente (craftearObjeto personaje x) xs

compararNivel :: Personaje -> Personaje -> Bool
compararNivel (UnPersonaje nombre1 puntaje1 inventario1) (UnPersonaje nombre2 puntaje2 inventario2) = puntaje1 > puntaje2

masPuntosAlReves :: Personaje -> [Receta] -> Bool
masPuntosAlReves personaje lasRecetas = compararNivel (crafterSucesivamente personaje lasRecetas) (crafterSucesivamente personaje (reverse lasRecetas))

dobleXP :: Number -> Receta -> Bool
dobleXP puntaje receta = puntaje <= calcularPuntajeReceta receta

objetosDobleXP :: Personaje -> [Receta] -> [Receta]
objetosDobleXP (UnPersonaje nombre puntaje inventario) = filter (dobleXP (2*puntaje)) 
--Punto 2-Fin---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Craft-Fin-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Mine-Comienzo-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
obtenerPrecisionObjeto :: String -> Number
obtenerPrecisionObjeto herramienta = (snd.head)(filter (\h -> fst h == herramienta) herramientas)

obtenerObjetoBioma :: String -> Bioma -> Material
obtenerObjetoBioma "Hacha" bioma= (head.fst) bioma
obtenerObjetoBioma "Espada" bioma= (last.fst) bioma
obtenerObjetoBioma herramienta bioma 
    |obtenerPrecisionObjeto herramienta <= (length.fst) bioma - 1 = (!!) (fst bioma) (obtenerPrecisionObjeto herramienta)
    |otherwise = (!!) (fst bioma) 0

minar :: String -> Personaje -> Bioma -> Personaje
minar herramienta (UnPersonaje nombre puntaje inventario) bioma 
    |elem (snd bioma) inventario && elem herramienta inventario = UnPersonaje nombre (puntaje + 50) (obtenerObjetoBioma herramienta bioma:inventario)
    |otherwise = UnPersonaje nombre puntaje inventario
--Mine-Fin-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Ejemplos-Comienzo--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
steve :: Personaje
steve = UnPersonaje "Steve" 3000 ["Madera", "Fosforo", "Pollo","Lana", "Agujas", "Tintura", "Taladro"]

fogata :: Receta
fogata = (["Madera", "Fosforo"], 10)
polloAsado :: Receta
polloAsado = (["Fogata", "Pollo"], 300)
sweatter :: Receta
sweatter = (["Lana", "Agujas", "Tintura"], 600)

artico :: Bioma
artico = (["Hielo", "Iglu", "Lobo"], "Sweatter")

herramientas :: Herramienta
herramientas = [("Hacha", 0), ("Espada", 0), ("Pico", 1), ("Taladro", 3), ("Pala", 2)]

listaRecetas = [fogata, polloAsado, sweatter]

recetas :: Recetas
recetas = [("Fogata", fogata), ("Pollo Asado", polloAsado), ("Sweatter", sweatter)]
--Ejemplos-Fin--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


