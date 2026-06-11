-- Para poder poner los tipos dentro de instance
{-# LANGUAGE InstanceSigs #-}

module ListSeq where

import Par
import Seq
import Data.List (foldl')

instance Seq [] where
  -- O(1)
  emptyS :: [a]
  emptyS = []

  -- O(1)
  singletonS :: a -> [a]
  singletonS x = [x]

  -- O(n)
  lengthS :: [a] -> Int
  lengthS = length

  -- O(n)
  nthS :: [a] -> Int -> a
  nthS xs i = xs !! i

  -- O(n) si usa mapS3
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

  -- O(n)
  mapS :: (a -> b) -> [a] -> [b]
  mapS = map

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

  -- O(n)
  filterS :: (a -> Bool) -> [a] -> [a]
  filterS = filter

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

  -- O(n)
  takeS :: [a] -> Int -> [a]
  takeS xs n = take n xs

  -- O(n)
  dropS :: [a] -> Int -> [a]
  dropS xs n = drop n xs

  -- O(n)
  showtS :: [a] -> TreeView a ([a])
  showtS [] = EMPTY
  showtS [x] = ELT x
  showtS xs =
    let mitad = lengthS xs `div` 2
        (l, r) = splitAt mitad xs
     in NODE l r

  -- O(1)
  showlS :: [a] -> ListView a ([a])
  showlS [] = NIL
  showlS (x : xs) = CONS x xs

  -- O(n)
  joinS :: [[a]] -> [a]
  joinS = concat

  {-
  joinS1 :: [[a]] -> [a]
  joinS1 = reduceS appendS []
  -}

  -- O(n)
  reduceS :: (a -> a -> a) -> a -> [a] -> a
  reduceS = foldl'

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