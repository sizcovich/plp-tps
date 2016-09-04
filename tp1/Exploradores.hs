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
                else (foldNat s (s z) (n-1))

foldRT :: (a->[b]->b) -> RoseTree a -> b
foldRT f (Rose x rts) = f x (map (foldRT f) rts)

foldAB :: (b -> a -> b -> b) -> b -> AB a -> b 
foldAB f z Nil = z
foldAB f z (Bin x y w) = f (foldAB f z x) y (foldAB f z w)

--Ejercicio 3
singletons :: Explorador [a] [a]
singletons = foldr (\x -> (:) [x]) []

sufijos :: Explorador [a] [a]
sufijos = foldr (\currentElem rec -> (currentElem : head rec) : rec) [[]]

--Ejercicio 4
--No se responder esto con claridad porque aun no me di cuenta como encararlo con foldNat ni que implicaria el hacerlo (ver luego)
listasQueSuman :: Explorador Integer [Integer]
listasQueSuman 0 = [[]] 
listasQueSuman total = [ i : ls  | i <- [1 .. total], ls <- listasQueSuman (total-i)]

--Ejercicio 5
preorder :: Explorador (AB a) a
preorder =  foldAB (\reciz raiz recder -> [raiz] ++ reciz ++ recder ) []
--inorder :: undefined
inorder ::  Explorador (AB a) a
inorder = foldAB (\reciz raiz recder -> reciz ++ [raiz] ++ recder ) []
--postorder :: undefined
postorder :: Explorador (AB a) a
postorder = foldAB (\reciz raiz recder -> reciz ++ recder ++ [raiz] ) []

--Ejercicio 6
dfsRT :: Explorador (RoseTree a) a
dfsRT = foldRT (\rose rec -> [rose] ++ concat rec)

hojasRT :: Explorador (RoseTree a) a
hojasRT = foldRT (\rose rec -> case rec of
                    []      -> [rose]
                    (x:xs)  -> concat rec)

ramasRT :: Explorador (RoseTree a) [a]
ramasRT = foldRT (\rose rec -> case rec of
                    []      -> [[rose]]
                    (x:xs)  -> map (\path -> rose : path) (concat rec))

--Ejercicio 7
ifExp :: (a->Bool) -> Explorador a b -> Explorador a b -> Explorador a b
ifExp f exp1 exp2 =  (\x -> case (f x) of 
                        True -> exp1 x 
                        False-> exp2 x ) 

--Ejercicio 8
(<++>) :: Explorador a b -> Explorador a b -> Explorador a b
(<++>) exp1 exp2 = (\x -> exp1 x ++ exp2 x)

--Ejercicio 9
(<.>) :: Explorador b c -> Explorador a b -> Explorador a c
(<.>) exp1 exp2 = (\x -> concat $ map exp1 (exp2 x))

--Ejercicio 10
(<^>) :: Explorador a a -> Integer -> Explorador a a
(<^>) expl n = foldNat ((<.>) expl) expl (n-1)

--Ejercicio 11
listasDeLongitud :: Explorador Integer [Integer]
listasDeLongitud n = map (snd) $ concat $ [paresSumatoriaLista suma n | suma <-[n .. ]]

paresSumatoriaLista :: Integer -> Integer -> [(Integer, [Integer])]
paresSumatoriaLista suma n = filter fFilter $ foldNat fFold [(0,[])] n
 								where 
 									fFilter = (\pair->fst pair == suma)
 									fFold = (\pairs -> [ (fst(pair) + i, (snd(pair) ++ [i]))   | pair <- pairs, i <- [1 .. suma - fst(pair)]])

(<*>) :: Explorador a a -> Explorador a [a] 
(<*>)  exp = undefined --takeWhile(<>[])  (\y ->iterate exp y)


