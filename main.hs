{-# LANGUAGE InstanceSigs #-} -- Para poder poner los tipos dentro de instance
{-# LANGUAGE ScopedTypeVariables #-} -- Para poder poner los tipos manteniendo la letra dentro de la función

-- sortBy toma una función de comparación y una lista y devuelve la lista ordenada con dicho criterio
-- partition es exactamente igual a filter pero devuelve la tupla de ([CUMPLEN], [NO CUMPLEN])
import Data.List (sortBy, partition)

data NdTree p = 
    Node (NdTree p) -- subárbol izquierdo
    p -- punto
    (NdTree p) -- subárbol derecho
    Int -- eje
    | Empty deriving (Eq, Ord, Show)

-- 1. Para representar puntos de un espacio métrico n-dimensional, se utilizará la siguiente clase:

class Punto p
    where
        dimension :: p -> Int -- devuelve el número de coordenadas de un punto
        coord :: Int -> p -> Double -- devuelve la coordenada k-ésima de un punto (comenzando de 0)
        dist :: p -> p -> Double -- calcula la distancia entre dos puntos
        dist p1 p2 = sum [(coord k p1 - coord k p2) ^ 2 | k <- [0 .. (n-1)]] -- agregar «sqrt $» antes de sum si hiciera falta
            where
                n = dimension p1

-- a) Definir la función dist :: Punto p => p -> p -> Double que calcula la distancia entre dos puntos p =
-- (x0, ..., xn) y q = (y0, ..., yn) usando la siguiente fórmula:
-- sum de i=0 a n de (yi - xi)^2

-- b) Se utilizarán los siguientes tipos de datos para representar puntos en el plano y en el espacio tridimensional:

newtype Punto2d = P2d (Double, Double) deriving Show
newtype Punto3d = P3d (Double, Double, Double) deriving Show

-- Dar las instancias de Punto para Punto2d y Punto3d.

instance Punto Punto2d where
    dimension :: Punto2d -> Int
    dimension _ = 2

    coord :: Int -> Punto2d -> Double
    coord 0 (P2d (x0, _)) = x0
    coord 1 (P2d (_, x1)) = x1
    coord _ _ = error "Fuera de rango o punto inválido"

instance Punto Punto3d where
    dimension :: Punto3d -> Int
    dimension _ = 3

    coord :: Int -> Punto3d -> Double
    coord 0 (P3d (x0, _, _)) = x0
    coord 1 (P3d (_, x1, _)) = x1
    coord 2 (P3d (_, _, x2)) = x2
    coord _ _ = error "Fuera de rango o punto inválido"

{-
2. Para generar un árbol de puntos a partir de una lista de puntos de un espacio n-dimensional, se utilizará el
siguiente método:

i- Seleccionar el eje sobre el cual se alineará el hiperplano. Para seleccionar un eje distinto en cada paso,
elegir el eje de coordenadas level %n (donde % es el operador módulo), donde level es el nivel del árbol
que se está construyendo.

ii- Calcular la mediana de la lista de puntos, según el eje seleccionado.

iii- Crear un árbol l, aplicando este método, con los puntos de la lista que tienen como valor en el eje
seleccionado un valor menor o igual al valor en el eje de la mediana.

iv- Hacer lo mismo para crear un árbol r, con los puntos que tienen el valor correspondiente al eje seleccio-
nado mayor al valor correspondiente al eje de la mediana.

v- Crear un nodo del árbol que tenga como raíz la mediana, como eje el eje seleccionado, como subárbol
izquierdo el árbol l y como subárbol derecho el árbol r .

Utilizando este método de obtiene un árbol que cumple con el invariante dado en la introducción, y es lo más
balanceado que podría ser con el orden dado para las coordenadas.
Definir una función fromList :: Punto p ⇒ [p] → NdTree p, que construya un árbol a partir de una lista de
puntos, utilizando el método dado.
En los siguientes ejercicios se definirán funciones para insertar, eliminar o buscar puntos en un árbol con
estas características. Definir estas funciones de manera eficiente, utilizando la información sobre los ejes de
los nodos.
-}

{-
Sobre esta implementación de fromList:
    NO TOMA los menores o iguales a la mediana del lado izquierdo, sino que devuelve un árbol balanceado.
    Es decir, de la lista [1, 2, 2, 2, 2, 2, 2, 2, 2, 3] de obtiene algo del estilo
    «Node [1, 2, 2, 2, 2] 2 [2, 2, 2, 3]» en lugar de lo que efectivamente pide el enunciado que es
    «Node [1, 2, 2, 2, 2, 2, 2, 2] 2 [3]»
    Creo que esta implementación es mejor.
-}
fromList :: forall p. (Punto p) => [p] -> NdTree p
fromList xs = fromList2 0 (sortBy (comparaCoords 0) xs)
    where
        -- Toma un eje y una lista ordenada por dicho eje, y devuelve el NdTree de dichos puntos
        fromList2 :: Int -> [p] -> NdTree p
        fromList2 _ [] = Empty
        fromList2 eje [p1] = Node Empty p1 Empty eje
        fromList2 eje xs =
            let
                largo :: Int -- Largo lista de puntos
                largo = length xs

                -- Parto la lista en los menores o iguales y mayores o iguales a la mediana (en la mitad),
                -- separando el menor de los mayores
                (men, p1:may) = splitAt (largo `div` 2) xs :: ([p], [p])
                
                newEje :: Int -- El nuevo eje a tomar para ordenar
                newEje = (eje + 1) `mod` dimension p1
                
                ordenar :: [p] -> [p] -- Toma una lista y la ordena según el nuevo eje
                ordenar = sortBy (comparaCoords newEje)
            in
                Node (fromList2 newEje (ordenar men)) p1 (fromList2 newEje (ordenar may)) eje

        -- Toma un eje y dos puntos y devuelve el Ordering según dicho eje
        comparaCoords :: Int -> p -> p -> Ordering
        comparaCoords eje p1 p2 = compare (coord eje p1) (coord eje p2)

{-
Sobre esta implementacion de fromList':
    Hace exactamente lo que pide el enunciado, pero al no balancear el arbol, no hace realmente
    falta ordenar nunca la lista. Podría calcularse y extrarse la mediana sin ordenarla, y con la
    funcion partition directamente podrían separarse los valores menores y mayores de la lista
    desordenada. Sin embargo, calcular la mediana sin ordenarla (con Quickselect) es innecesariamente 
    complejo, por eso simplemente la ordena y trabaja sobre la lista siempre ordenada. Además, como
    las listas en Haskell funcionan como listas enlazadas, no es posible acceder al cualquier índice
    con costo O(1), sino con costo O(índice), por lo que tampoco sería demasiado más eficiente.
-}
fromList' :: forall p. (Punto p) => [p] -> NdTree p
fromList' xs = fromList2 0 (sortBy (comparaCoords 0) xs)
    where
        -- Toma un eje y una lista ordenada por dicho eje, y devuelve el NdTree de dichos puntos
        fromList2 :: Int -> [p] -> NdTree p
        fromList2 _ [] = Empty
        fromList2 eje [p1] = Node Empty p1 Empty eje
        fromList2 eje xs =
            let
                largo :: Int -- Largo lista de puntos
                largo = length xs

                indexMediana :: Int -- Índice de la mediana
                indexMediana = largo `div` 2
                
                p1 :: p -- x es la mediana
                p1 = xs !! indexMediana

                resto :: [p] -- El resto de la lista sin la mediana
                resto = take indexMediana xs ++ drop (indexMediana + 1) xs

                -- Parto la lista en los menores o iguales y mayores a la mediana
                (men, may) = partition (\p -> (coord eje p <= coord eje p1)) resto :: ([p], [p])
                
                newEje :: Int -- El nuevo eje a tomar para ordenar
                newEje = (eje + 1) `mod` dimension p1
                
                ordenar :: [p] -> [p] -- Toma una lista y la ordena según el nuevo eje
                ordenar = sortBy (comparaCoords newEje)
            in
                Node (fromList2 newEje (ordenar men)) p1 (fromList2 newEje (ordenar may)) eje

        -- Toma un eje y dos puntos y devuelve el Ordering según dicho eje
        comparaCoords :: Int -> p -> p -> Ordering
        comparaCoords eje p1 p2 = compare (coord eje p1) (coord eje p2)
