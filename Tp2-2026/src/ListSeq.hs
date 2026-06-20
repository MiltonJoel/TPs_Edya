-- Para poder poner los tipos dentro de instance
{-# LANGUAGE InstanceSigs #-}

module ListSeq where

import Par
import Seq

instance Seq [] where
  -- O(1)
  emptyS :: [a]
  emptyS = []

  -- O(1)
  singletonS :: a -> [a]
  singletonS x = [x]

  -- O(n)
  lengthS :: [a] -> Int
  lengthS [] = 0
  lengthS (x:xs) = 1 + lengthS xs 

  {-
  -- O(n)
  lengthS :: [a] -> Int
  lengthS = length
  -}

  -- O(n) --> Work = i
  nthS :: [a] -> Int -> a
  nthS [] _ = error "el índice excede el tamaño de la lista"
  nthS (x:xs) 0 = x
  nthS (x:xs) i = nthS xs (i - 1)   

  {-
  -- O(n)
  nthS :: [a] -> Int -> a
  nthS xs i = xs !! i
  -}

  -- Work -> O(n * W(f))
  -- Span -> O(max n S(f) + W(paralelizar) * n)
  tabulateS :: (Int -> a) -> Int -> [a]
  tabulateS f n = mapS f [0 .. n - 1]

  -- Work -> O(n * W(f))
  -- Span -> O(max n S(f) + W(paralelizar) * n)
  mapS :: (a -> b) -> [a] -> [b]
  mapS _ [] = []
  mapS f (x:xs) =
    let (x', xs') = f x ||| mapS f xs
     in x' : xs'
  
  {-
  -- O(W(f) * n)
  mapS :: (a -> b) -> [a] -> [b]
  mapS = map
  -}

  -- Work -> O(n * W(f))
  -- Span -> O(max n S(f) + W(paralelizar) * n)
  filterS :: (a -> Bool) -> [a] -> [a]
  filterS _ [] = []
  filterS f (x:xs) = 
    let (cond, xs') = f x ||| filterS f xs
     in (if cond then x : xs' else xs')
  
  {-
  -- O(W(f) * n)
  filterS :: (a -> Bool) -> [a] -> [a]
  filterS = filter
  -}

  -- O(n) -> siendo n el length de la primera lista
  appendS :: [a] -> [a] -> [a]
  appendS [] ys = ys
  appendS (x:xs) ys = x : appendS xs ys
  
  {-
  -- O(n)
  appendS :: [a] -> [a] -> [a]
  appendS xs ys = xs ++ ys
  -}

  -- O(n) --> Work = k
  takeS :: [a] -> Int -> [a]
  takeS [] _ = []
  takeS xs 0 = []
  takeS (x:xs) k = x : takeS xs (k - 1)

  {-
  -- O(k)
  takeS :: [a] -> Int -> [a]
  takeS xs k = take k xs
  -}
  
  
  -- O(n) --> Work = k
  dropS :: [a] -> Int -> [a]
  dropS [] _ = []
  dropS xs 0 = xs
  dropS (x:xs) k = dropS xs (k - 1)

  {-
  -- O(n)
  dropS :: [a] -> Int -> [a]
  dropS xs k = drop k xs
  -}

  -- O(n) -> W(lenghtS) = n, W(splitAt) = n
  showtS :: [a] -> TreeView a [a]
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
  showlS :: [a] -> ListView a [a]
  showlS [] = NIL
  showlS (x : xs) = CONS x xs
  
  -- O(n) -> siendo n el largo de todas las listas sumadas
  joinS :: [[a]] -> [a]
  joinS [] = []
  joinS [xs] = xs  
  joinS (xs:xss) = appendS xs (joinS xss)

  {-
  -- O(n)
  joinS :: [[a]] -> [a]
  joinS = concat
  -}

  {-
  -- El costo incrementa porque recorre varias veces las listas izquierdas
  joinS :: [[a]] -> [a]
  joinS = reduceS appendS []
  -}

  {-
  -- O(n * W(f))
  reduceS :: (a -> a -> a) -> a -> [a] -> a
  reduceS = foldl'
  -}

  -- Work -> O(n * W(f))
  -- Span -> O(lg n * S(f) + O(n) + W(paralelizar))
  reduceS :: (a -> a -> a) -> a -> [a] -> a
  reduceS _ e [] = e
  reduceS _ _ [x] = x 
  reduceS f e xs = reduceS f e (contraer xs)
      where
        contraer [] = []
        contraer [x] = [x]
        contraer (x : y : rest) = 
          let (a, b) = f x y ||| contraer rest
           in a : b

  -- Work -> O(n * W(f))
  -- Span -> O(lg n * S(f) + O(n) + W(paralelizar))
  -- Sea (n = 2k), o (n = 2k + 1)
  scanS :: (a -> a -> a) -> a -> [a] -> ([a], a)
  scanS _ e [] = ([], e)
  scanS _ e [x] = ([e], x)
  scanS f e xs = -- scanS (+) 0 [1, 2, 3, 4, 5]
    let 
        -- <x0 + x1, x2 + x3, ..., x2k-1 + x2k>
        -- [1 + 2, 3 + 4]
        apareados = contraerPares xs
        
        -- (<e, x0 + x1, x0 + x1 + x2 + x3, ...>, x0 + ... + x2k)
        -- [[0, 1 + 2], 1 + 2 + 3 + 4]
        (pares, totalPares) = scanS f e apareados

        -- <x0, x2, x4, ..., x2k-2, x2k>
        -- [1, 3, 5]
        paresOriginales = extraerPares xs

        -- <e, x0, x0 + x1, x0 + x1 + x2, ..., x0 + ... + x2k>
        -- [0, 1, 1 + 2, 1 + 2 + 3, 1 + 2 + 3 + 4]
        prefijos = combinarS pares paresOriginales totalPares

        -- x0 + ... + x2k + x2k+1
        -- 1 + 2 + 3 + 4 + 5
        totalFinal = calcularTotal pares paresOriginales totalPares
    in (prefijos, totalFinal)
      where
        -- Work -> O(n * W(f))
        -- Span -> O(n + S(f))
        -- Descarta el útlimo elemento si es impar
        contraerPares [] = []
        contraerPares [x] = []
        contraerPares (x:y:rest) = 
          let (par, resto) = f x y ||| contraerPares rest
           in par : resto

        -- O(n)
        -- Obtiene los elementos pares originales
        extraerPares [] = []
        extraerPares [x] = [x]
        extraerPares (x:_:rest) = x : extraerPares rest

        -- Work -> O(n * W(f))
        -- Span -> O(n + S(f))
        -- Intercala los resultados pares con los impares
        combinarS [] [] _ = []
        combinarS [] [_] t = [t] -- Si sobra uno, el prefijo es el totalPares
        combinarS (ev:evs) (p:ps) t = 
          let (oddVal, resto) = f ev p ||| combinarS evs ps t
           in ev : oddVal : resto
        combinarS _ _ _ = [] 

        -- O(n)
        -- Define si el total requiere sumar el último elemento impar
        calcularTotal [] [] t = t
        calcularTotal [] [x] t = f t x
        calcularTotal (_:es) (_:ps) t = calcularTotal es ps t
        calcularTotal _ _ t = t

  {-
  -- O(n)
  scanS :: (a -> a -> a) -> a -> [a] -> ([a], a)
  scanS f e xs = (init scanned, last scanned)
    where
      scanned = scanl f e xs
  -}

  -- O(1)
  fromList :: [a] -> [a]
  fromList xs = xs

  {-
  -- O(1)
  fromList :: [a] -> [a]
  fromList = id
  -}