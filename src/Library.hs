module Library where
import PdePreludat

type Material = String
type Tiempo = Number
type Receta = ([Material], Tiempo)
type Recetas = [Receta]
type Puntaje = Number

data Personaje = UnPersonaje{
    nombre :: String, 
    puntaje :: Number,
    inventario :: [Material]
} deriving Show

esElObjeto :: [Material] -> Material -> Bool
esElObjeto receta material = elem material receta

ponerMaterial :: Bool -> Material -> [Material]
ponerMaterial bool material
    |bool = [material]
    |otherwise = []

eliminarObjetos :: Receta -> [Material]-> [Material]
eliminarObjetos receta [] = []
eliminarObjetos receta (material:restoInventario) = [ponerMaterial (not(esElObjeto material)) material]++eliminarObjetos receta restoInventario

calcularPuntajeReceta :: [Material] -> Receta -> Puntaje
calcularPuntajeReceta inventario (materiales, tiempo) 
    |not(head(filter (/= True) (map (esElObjeto materiales) inventario))) = -100
    |otherwise = tiempo * 10

eliminarObjetosInventario :: Receta -> [Material] -> [Material]
eliminarObjetosInventario receta inventario 
    |not(head(filter (/= True) (map (esElObjeto materiales) inventario))) = inventario
    |otherwise = eliminarObjetos receta inventario

craftearObjeto :: Personaje -> Receta -> Material
craftearObjeto (UnPersonaje nombre puntaje inventario) receta = UnPersonaje nombre (puntaje + (calcularPuntajeReceta inventario receta)) (eliminarObjetosInventario receta inventario)




