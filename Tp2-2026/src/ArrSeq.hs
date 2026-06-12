module ArrSeq where
import qualified Arr as A
import Arr ((!))
import Par
import Seq

-- O(1)
emptyA :: A.Arr a
emptyA = A.empty

-- O(1)
singletonA :: a-> A.Arr a
singletonA x = A.fromList [x]

-- O(1)
lengthA :: A.Arr a -> Int
lengthA = A.length

-- O(1)
nthA :: A.Arr a -> Int -> a
nthA = (!)

-- O(1)
takeA :: A.Arr a -> Int -> A.Arr a
takeA arr i | i<=0 = A.empty
            | i<= lengthA arr = A.subArray 0 i arr
            | otherwise = arr

-- O(1)
dropA :: A.Arr a -> Int -> A.Arr a
dropA arr i | i<=0 = arr
            | i< n = A.subArray i (n-i) arr
            | otherwise = A.empty
            where n = lengthA arr

-- W = O(sum W(f i)), S = O(max S(f i))
tabulateA :: (Int -> a) -> Int -> A.Arr a
tabulateA = A.tabulate

-- W = O(sum W(f)), S = O(max S(f))
mapA :: (a -> b) -> A.Arr a -> A.Arr b 
mapA f arr = tabulateA (\i -> f (nthA arr i)) (lengthA arr)

-- O(|s| + |t|)
appendA :: A.Arr a -> A.Arr a -> A.Arr a 
appendA arr1 arr2 = tabulateA (\i -> if i < l1 then arr1 ! i else arr2 ! (i-l1)) (l1+l2)
                 where l1 = lengthA arr1 
                       l2 = lengthA arr2

-- W = O(sum W(f si)), S = O(lg|s| + max S(f si))
filterA ::(a -> Bool) -> A.Arr a -> A.Arr a 
filterA p arr = case (lengthA arr) of
                0 -> emptyA
                1 -> if p (nthA arr 0) then takeA arr 1 else emptyA
                n -> let (l,r) = filterA p (takeA arr (n `div` 2)) ||| filterA p (dropA arr (n `div` 2))
                     in appendA l r 

-- O(1)
showtA :: A.Arr a -> TreeView a (A.Arr a)
showtA arr = case (lengthA arr) of
             0 -> EMPTY
             1 -> ELT (nthA arr 0)
             n -> NODE (takeA arr (n `div` 2)) (dropA arr (n `div` 2))

-- O(1)
showlA :: A.Arr a -> ListView a (A.Arr a)
showlA arr = case (lengthA arr) of
             0 -> NIL
             n -> CONS (nthA arr 0) (dropA arr 1)

-- O(|s|) + sum O(|si|), profundidad O(lg|s|)
joinA ::  A.Arr (A.Arr a) -> A.Arr a
joinA = A.flatten

-- O(|xs|)
fromListA :: [a] -> A.Arr a
fromListA = A.fromList

-- W = O(sum_{i=0}^{ceil(n/2)-1} W(f arr_2i arr_2i+1))
-- S = O(max_{i} S(f arr_2i arr_2i+1))
contrA :: (a -> a -> a) -> A.Arr a -> A.Arr a
contrA f   arr = tabulateA faux m
                 where  faux i = if i==(mid) then nthA arr (n-1) else faux' i
                        faux' i = f (nthA arr (i*2)) (nthA arr (i*2+1))
                        mid = n `div` 2
                        n = lengthA arr
                        m = if n `mod` 2 ==0 then mid else mid+1

-- W = O(|arr| + sum W(x op y)), S = O(lg|arr| * max S(x op y))
reduceA :: (a -> a -> a) -> a -> A.Arr a -> a
reduceA f e arr = case (lengthA arr) of
                  0 -> e
                  1 -> f e (nthA arr 0)
                  n -> reduceA f e (contrA f arr)

-- W = O(|v|), S = O(max S(f))
expandA :: (a -> a -> a) -> a -> A.Arr a -> A.Arr a -> A.Arr a
expandA f b v v' = tabulateA aux (lengthA v) where 
  aux i = case even i of
    True  -> nthA v' (div i 2)
    False -> f (nthA v' (div i 2)) (nthA v (i-1))

-- W = O(|v| + sum W(x op y)), S = O(lg|v| * max S(x op y))
scanA :: (a -> a -> a) -> a -> A.Arr a -> (A.Arr a, a)
scanA f b v = case lengthA v of
  0 -> (emptyA, b)
  1 -> (singletonA b, f b (nthA v 0))
  otherwise -> let (v', r) = scanA f b (contrA f v)
               in (expandA f b v v', r) 

instance Seq A.Arr where
  emptyS     = emptyA
  singletonS = singletonA
  lengthS    = lengthA
  nthS       = nthA
  tabulateS  = tabulateA
  mapS       = mapA
  filterS    = filterA
  appendS    = appendA
  takeS      = takeA
  dropS      = dropA
  showtS     = showtA
  showlS     = showlA
  joinS      = joinA
  reduceS    = reduceA
  scanS      = scanA
  fromList   = fromListA

