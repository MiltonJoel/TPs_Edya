{-
 Este módulo requiere la librería parallel. 
 
 Si no la tiene instalada, se puede instalarse utilizando Cabal, ejecutando el siguiente código  
 en un intérprete de comandos: 
 
 $ cabal update
 $ cabal install parallel
 
-}

module Par ((|||)) where

import Control.Parallel
-- GHC.Conc en vez de Control.Parallel, es lo mismo pero no me da error
--import GHC.Conc

infix 1 |||

(|||)   ::   a -> b -> (a,b)
a ||| b = a `par` b `par` (a,b)
