module TestCases where

import Main
  ( NdTree (..),
    Punto2d (..),
    Punto3d (..),
    Rect,
    eliminar,
    fromList,
    inRegion,
    insertar,
    ortogonalSearch,
  )

import System.Exit (exitFailure)

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

tests :: [(String, Bool)]
tests =
  [ ("p2d2 esta dentro de rect2dGrande", p2dDentroRectGrande),
    ("p2d4 esta fuera de rect2dChico", not p2dFueraRectChico),
    ("p2d2 esta dentro de rect2dInvertido", inRegion p2d2 rect2dInvertido),
    ("ortogonalSearch rect grande contiene p2d1", p2d1 `elem` ortogonalSearch arbol2dLista rect2dGrande),
    ("ortogonalSearch rect grande contiene p2d2", p2d2 `elem` ortogonalSearch arbol2dLista rect2dGrande),
    ("ortogonalSearch rect grande contiene p2d3", p2d3 `elem` ortogonalSearch arbol2dLista rect2dGrande),
    ("ortogonalSearch rect vacio da lista vacia", ortogonalSearch arbol2dLista (P2d (10,10), P2d (20,20)) == []),
    ("ortogonalSearch arbol vacio da lista vacia", ortogonalSearch arbol2dVacio rect2dGrande == []),
    ("ortogonalSearch rect invertido contiene p2d1", p2d1 `elem` ortogonalSearch arbol2dLista rect2dInvertido)
  ]

main :: IO ()
main =
  if and (map snd tests)
    then mapM_ printTest tests
    else do
      mapM_ printTest tests
      exitFailure
  where
    printTest (nombre, resultado) =
      putStrLn (nombre ++ ": " ++ if resultado then "OK" else "FALLO")