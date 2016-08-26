module Exploradores (Explorador, AB(Nil,Bin), RoseTree(Rose), foldNat, foldRT, foldAB, expNulo, expId, expHijosRT, expHijosAB, expTail, ifExp, singletons, sufijos, inorder, preorder, postorder, dfsRT, ramasRT, hojasRT, listasQueSuman, listasDeLongitud, (<.>), (<^>), (<++>), (<*>)) where

import Prelude hiding ((<*>))

--Definiciones de tipos

type Explorador a b = a -> [b]

data AB a = Nil | Bin (AB a) a (AB a) deriving Eq

data RoseTree a = Rose a [RoseTree a] deriving Eq

-- Definiciones de Show

instance Show a => Show (RoseTree a) where
    show x = concatMap (++"\n") (padTree 0 x)

padTree :: Show a => Int -> RoseTree a -> [String]
padTree i (Rose x ys) =  ((pad i) ++  (show x) ) : (concatMap (padTree (i + 4)) ys)

pad :: Int -> String
pad i = replicate i ' '


instance Show a => Show (AB a) where
  show = padAB 0 0
  
padAB _ _ Nil = ""
padAB n base (Bin i x d) = pad n ++ show x ++ padAB 4 (base+l) i ++ "\n" ++ padAB (n+4+base+l) base d where l = length $ show x


--Ejercicio 1
expNulo :: Explorador a b
expNulo = (\x -> [])

expId :: Explorador a a
expId = (\x -> [x])

expHijosRT :: Explorador (RoseTree a) (RoseTree a)
expHijosRT (Rose x ys) = ys

expHijosAB :: Explorador(AB a) (AB a)
expHijosAB Nil = []
expHijosAB (Bin x y z) = [x,z]

expTail :: Explorador [a] a
expTail [] = []
expTail (x:xs) = xs

--Ejercicio 2
foldNat :: (b->b) -> b -> Integer -> b
foldNat s z 0 = z
foldNat s z n = if n<0 then error (show n ++ " no es un natural") 
                else (foldNat s z (n-1))

--foldRT :: undefined
foldRT :: (a->b->b) -> RoseTree a -> b -> b
foldRT fRose (Rose r []) z = fRose r z
foldRT fRose (Rose r rts) z = foldr (foldRT fRose) (fRose r z) rts

--foldAB :: undefined
foldAB = undefined

--Ejercicio 3
singletons :: Explorador [a] [a]
singletons = undefined

sufijos :: Explorador [a] [a]
sufijos = undefined

--Ejercicio 4
--listasQueSuman :: Explorador Integer ?
listasQueSuman = undefined

--Ejercicio 5
--preorder :: undefined
preorder = undefined

--inorder :: undefined
inorder = undefined

--postorder :: undefined
postorder = undefined

--Ejercicio 6
dfsRT :: Explorador (RoseTree a) a
dfsRT = undefined

hojasRT :: Explorador (RoseTree a) a
hojasRT = undefined

ramasRT :: Explorador (RoseTree a) [a]
ramasRT = undefined

--Ejercicio 7
ifExp :: (a->Bool) -> Explorador a b -> Explorador a b -> Explorador a b
ifExp = undefined

--Ejercicio 8
(<++>) :: Explorador a b -> Explorador a b -> Explorador a b
(<++>) = undefined

--Ejercicio 9
(<.>) :: Explorador b c -> Explorador a b -> Explorador a c
(<.>) = undefined

--Ejercicio 10
(<^>) :: Explorador a a -> Integer -> Explorador a a
(<^>) = undefined

--Ejercicio 11 (implementar al menos una de las dos)
listasDeLongitud :: Explorador Integer [Integer]
listasDeLongitud = undefined

(<*>) :: Explorador a a -> Explorador a [a] 
(<*>) = undefined