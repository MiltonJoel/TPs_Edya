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

newtype Punto2d = P2d (Double, Double) deriving (Show, Eq)
newtype Punto3d = P3d (Double, Double, Double) deriving (Show, Eq)

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
-- Ejercicio 3

insertar :: Punto p => p -> NdTree p -> NdTree p
insertar p Empty = Node Empty p Empty 0
insertar p (Node izq raiz der e)
    | (<=) (coord e p) (coord e raiz) = Node (insertarConEje siguienteEje p izq) raiz der e
    | otherwise = Node izq raiz (insertarConEje siguienteEje p der) e
    where
        siguienteEje = (\eje -> mod ((+) eje 1) (dimension raiz)) e

insertarConEje :: Punto p => Int -> p -> NdTree p -> NdTree p
insertarConEje e p Empty = Node Empty p Empty e
insertarConEje _ p (Node izq raiz der e)
    | (<=) (coord e p) (coord e raiz) = Node (insertarConEje siguienteEje p izq) raiz der e
    | otherwise = Node izq raiz (insertarConEje siguienteEje p der) e
    where
        siguienteEje = (\eje -> mod (eje+1) (dimension raiz)) e


-- Ejercicio 4

--minEnje: Busca el punto con menor valor en el eje e dentro de todo el árbol.
minEnEje :: Punto p => Int -> NdTree p -> p
minEnEje _ Empty = error "árbol vacío"
minEnEje _ (Node Empty p Empty _) = p
minEnEje e (Node Empty p der _) = minimoEntre e p (minEnEje e der)
minEnEje e (Node izq p Empty _) = minimoEntre e p (minEnEje e izq)
minEnEje e (Node izq p der _) = minimoEntre e p (minimoEntre e (minEnEje e izq) (minEnEje e der))

--maxEnje: Busca el punto con mayor valor en el eje e dentro de todo el árbol.
maxEnEje :: Punto p => Int -> NdTree p -> p 
maxEnEje _ Empty = error "árbol vacío"
maxEnEje _ (Node Empty p Empty _) = p 
maxEnEje e (Node Empty p der _) =
    maximoEntre e p (maxEnEje e der)
maxEnEje e (Node izq p Empty _) =
    maximoEntre e p (maxEnEje e izq)
maxEnEje e (Node izq p der _) =
    maximoEntre e p (maximoEntre e (maxEnEje e izq) (maxEnEje e der))

--minimoEntre: compara dos puntos en el eje e y devuelve el menor.
--Ejemplo: minimoEntre 1 (2,3) (4,7) -> (2,3)
minimoEntre :: Punto p => Int -> p -> p -> p 
minimoEntre e p1 p2 = if coord e p1 <= coord e p2 then p1 else p2

maximoEntre :: Punto p => Int -> p -> p -> p
maximoEntre e p1 p2 = if coord e p1 >= coord e p2 then p1 else p2

-- eliminar: 
eliminar :: (Eq p, Punto p) => p -> NdTree p -> NdTree p 
eliminar _ Empty = Empty 
eliminar p (Node izq raiz der e)
    | coord e p < coord e raiz = Node (eliminar p izq) raiz der e 
    | coord e p > coord e raiz = Node izq raiz (eliminar p der) e 
    | p == raiz = reemplazar izq der e
    |otherwise = Node izq raiz (eliminar p der) e 

reemplazar :: (Eq p, Punto p) => NdTree p -> NdTree p -> Int -> NdTree p 
reemplazar Empty Empty _ = Empty 
reemplazar izq der e =
    let reemplazante = if der /= Empty 
                        then minEnEje e der 
                        else maxEnEje e izq 
        nuevoIzq = if der /= Empty then izq else eliminar reemplazante izq 
        nuevoDer = if der /= Empty then eliminar reemplazante der else Empty 
    in Node nuevoIzq reemplazante nuevoDer e 

-- ejercicio 5

type Rect = (Punto2d, Punto2d)

inRegion :: Punto2d -> Rect -> Bool
inRegion (P2d (x, y)) (P2d (x1, y1), P2d (x2, y2)) =
    and [(>=) x xMin, (<=) x xMax, (>=) y yMin, (<=) y yMax]
    where
        xMin = min x1 x2
        xMax = max x1 x2
        yMin = min y1 y2
        yMax = max y1 y2



-- test cases 

p2d1 :: Punto2d
p2d1 = P2d (1, 1)

p2d2 :: Punto2d
p2d2 = P2d (3, 4)

p2d3 :: Punto2d
p2d3 = P2d (5, 2)

p2d4 :: Punto2d
p2d4 = P2d (2, 6)

p2d5 :: Punto2d
p2d5 = P2d (4, 0)

arbol2dVacio :: NdTree Punto2d
arbol2dVacio = Empty

arbol2dManual :: NdTree Punto2d
arbol2dManual =
    Node
        (Node Empty p2d1 Empty 1)
        p2d2
        (Node Empty p2d3 Empty 1)
        0

arbol2dLista :: NdTree Punto2d
arbol2dLista = fromList [p2d1, p2d2, p2d3, p2d4, p2d5]

arbol2dConInsertar :: NdTree Punto2d
arbol2dConInsertar = insertar p2d4 arbol2dManual

arbol2dConEliminar :: NdTree Punto2d
arbol2dConEliminar = eliminar p2d2 arbol2dLista

rect2dGrande :: Rect
rect2dGrande = (P2d (0, 0), P2d (5, 5))

rect2dChico :: Rect
rect2dChico = (P2d (2, 2), P2d (4, 4))

rect2dInvertido :: Rect
rect2dInvertido = (P2d (5, 5), P2d (0, 0))

p2dDentroRectGrande :: Bool
p2dDentroRectGrande = inRegion p2d2 rect2dGrande

p2dFueraRectChico :: Bool
p2dFueraRectChico = inRegion p2d4 rect2dChico

p3d1 :: Punto3d
p3d1 = P3d (1, 2, 3)

p3d2 :: Punto3d
p3d2 = P3d (4, 1, 0)

p3d3 :: Punto3d
p3d3 = P3d (2, 5, 6)

arbol3dLista :: NdTree Punto3d
arbol3dLista = fromList [p3d1, p3d2, p3d3]
