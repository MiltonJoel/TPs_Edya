-- Para poder poner los tipos dentro de instance
{-# LANGUAGE InstanceSigs #-}

module ListSeq where

import Par
import Seq
import Data.List (foldl')
import GHC.Real (reduce)

instance Seq [] where
  -- O(1)
  emptyS :: [a]
  emptyS = []

  -- O(1)
  singletonS :: a -> [a]
  singletonS x = [x]

  {-
    lengthS no es paralelizable en lo más mínimo porque la mera división de
    la lista es O(n), y la suma es O(1), por lo que no lo vale.
    Si la spliteo, la única manera es splitAt. Luego, el propio uso splitAt
    requiere intrínsecamente el uso de length, que es de O(n), por lo que 
    no tiene sentido. 
  -}
  -- O(n)
  lengthS :: [a] -> Int
  lengthS = length

  {-
  -- O(n)
  lengthS :: [a] -> Int
  lengthS [] = 0
  lengthS [x] = 1
  lengthS (x:xs) = 1 + lengthS xs 
  -}

  -- O(n)
  nthS :: [a] -> Int -> a
  nthS xs i = xs !! i

  {-
  -- O(n)
  nthS :: [a] -> Int
  nthS [] _ = error "el índice excede el length de la lista"
  nthS (x:xs) 0 = x
  nthS (x:xs) i = nthS xs (i - 1) 
  -}

  -- O(n) -> si f es O(1) y usa mapS secuencial
  -- O(?) -> si f es O(1) y usa mapS con Treeview?
  tabulateS :: (Int -> a) -> Int -> [a]
  tabulateS f n = mapS f [0 .. n - 1]

  {-
  mapS2 :: (a -> b) -> [a] -> [b]
  mapS2 f xs = case showtS xs of
    EMPTY -> emptyS
    ELT x -> singletonS (f x)
    NODE l r ->
      let (a, b) = mapS2 f l ||| mapS2 f r
       in appendS a b
  -}

  -- O(W(f) * n)
  mapS :: (a -> b) -> [a] -> [b]
  mapS = map

  {-
  -- O(max W(f) + O(n) «por cons» + W(paralelizar))
  mapS :: (a -> b) -> [a] -> [b]
  mapS _ [] = []
  mapS f (x:xs) =
    let (x', xs') = f x ||| mapS f xs
     in x' : xs'
  -}

  {-
  mapS1 :: (a -> b) -> [a] -> [b]
  mapS1 _ [] = []
  mapS1 f [x] = [f x]
  mapS1 f xs =
    let mitad = lengthS xs `div` 2
        (l, r) = splitAt mitad xs
        (a, b) = mapS1 f l ||| mapS1 f r
     in a ++ b
  -}

  {-
  filterS2 :: (a -> Bool) -> [a] -> [a]
  filterS2 f xs = case showtS xs of
    EMPTY -> emptyS
    ELT x -> if f x then singletonS x else emptyS
    NODE l r ->
      let (a, b) = filterS2 f l ||| filterS2 f r
       in appendS a b
  -}

  -- O(W(f) * n)
  filterS :: (a -> Bool) -> [a] -> [a]
  filterS = filter

  {-
  -- O(max W(f) + O(n) «por cons» + W(paralelizar))
  filterS :: (a -> Bool) -> [a] -> [a]
  filterS _ [] = []
  filterS f (x:xs) = 
    let (cond, xs') = f x ||| filterS f xs
     in (if cond then x : xs' else xs')
  -}

  {-
  filterS1 :: (a -> Bool) -> [a] -> [a]
  filterS1 _ [] = []
  filterS1 f [x] = [x | f x]
  filterS1 f xs =
    let mitad = lengthS xs `div` 2
        (l, r) = splitAt mitad xs
        (a, b) = filterS1 f l ||| filterS1 f r
     in a ++ b
  -}

  -- O(n)
  appendS :: [a] -> [a] -> [a]
  appendS xs ys = xs ++ ys

  {-
  -- O(n) -> siendo n el length de la primera
  appendS :: [a] -> [a] -> [a]
  appendS [] ys = ys
  appendS (x:xs) ys = x : appendS xs ys
  -}

  -- O(k)
  takeS :: [a] -> Int -> [a]
  takeS xs k = take k xs

  {-
  -- O(k)
  takeS :: [a] -> Int -> [a]
  takeS [] _ = []
  takeS xs 0 = []
  takeS (x:xs) k = x : takeS xs (k - 1)
  -}

  -- O(n)
  dropS :: [a] -> Int -> [a]
  dropS xs k = drop k xs

  {-
  -- O(k)
  dropS :: [a] -> Int -> [a]
  dropS [] _ = []
  dropS xs 0 = xs
  dropS (x:xs) k = dropS xs (k - 1)
  -}

  -- O(n) -> W(len) = n, W(split) = n
  showtS :: [a] -> TreeView a ([a])
  showtS [] = EMPTY
  showtS [x] = ELT x
  showtS xs =
    let mitad = lengthS xs `div` 2
        (l, r) = splitAt' mitad xs
     in NODE l r
      where
        splitAt' = splitAt
        {-
        splitAt' _ [] = ([], [])
        splitAt' k (x : xs) 
          | n <= 0 = ([], x : xs)
          | otherwise = (x : xs', ys)
            where
              (xs', ys) = splitAt' (n - 1) xs
        -}

  -- O(1)
  showlS :: [a] -> ListView a ([a])
  showlS [] = NIL
  showlS (x : xs) = CONS x xs

  -- O(n)
  joinS :: [[a]] -> [a]
  joinS = concat

  {-
  -- O(n) -> siendo n el largo de todas las listas sumadas
  joinS :: [[a]] -> [a]
  joinS [] = []
  joinS [xs] = xs  
  joinS (xs:xss) = appendS xs (joinS xss)
  -}

  {-
  joinS :: [[a]] -> [a]
  joinS = reduceS appendS []
  -}

  reduceS :: (a -> a -> a) -> a -> [a] -> a
  -- O(n * W(f))
  --reduceS = foldl'

  -- O(lg n *  max S(f) + O(n) + W(paralelizar))
  reduceS _ e [] = e
  reduceS _ _ [x] = x 
  reduceS f e xs = reduceS f e (contraer xs)
      where
        contraer [] = [e]
        contraer [x] = [x]
        contraer (x : y : rest) = 
          let (a, b) = f x y ||| contraer rest
           in a : b

  {-
  reduceS2 :: (a -> a -> a) -> a -> [a] -> a
  reduceS2 _ e [] = e -- Caso base necesario si la lista viene vacía desde el inicio
  reduceS2 f e xs = case showtS xs of
    EMPTY -> e
    ELT x -> x
    NODE l r ->
      let (a, b) = reduceS2 f e l ||| reduceS2 f e r
       in f a b
  -}

  {-
  reduceS1 :: (a -> a -> a) -> a -> [a] -> a
  reduceS1 _ e [] = e
  reduceS1 _ _ [x] = x
  reduceS1 f e xs =
    let mitad = lengthS xs `div` 2
        (l, r) = splitAt mitad xs
        (a, b) = reduceS1 f e l ||| reduceS1 f e r
     in f a b
  -}
  -- O(n)
  scanS :: (a -> a -> a) -> a -> [a] -> ([a], a)
  scanS f e xs = (init scanned, last scanned)
    where
      scanned = scanl f e xs

  {-
  scanS2 :: (a -> a -> a) -> a -> [a] -> ([a], a)
  scanS2 f e xs = case showtS xs of
    EMPTY -> (emptyS, e)
    ELT x -> (singletonS e, f e x)
    NODE l r ->
      let ((scanL, totL), (scanR, totR)) = scanS2 f e l ||| scanS2 f e r
          scanR2 = mapS (f totL) scanR
       in (appendS scanL scanR2, f totL totR)
  -}
  -- O(1)
  fromList :: [a] -> [a]
  fromList = id