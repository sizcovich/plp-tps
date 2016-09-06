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
singletons = map (\x -> [x])

sufijos :: Explorador [a] [a]
sufijos = foldr (\currentElem rec -> (currentElem : head rec) : rec) [[]]

--Ejercicio 4
--Si usáramos foldNat deberíamos especificar el argumento con el que se va a hacer la recursión. Éste puede ser, o bien la suma, o bien el tamaño de la lista. Hacerlo sobre el tamaño implicaría agregar un elemento en cada iteración cuando en realidad el tamaño de las listas es variable. Por ejemplo, si hubiese que armar listas que suman 4, habría que poder formar [4] y [1,1,1,1], lo que no es posible con foldNat en el tamaño. Por otro lado, hacer la recursión sobre la suma tampoco tiene sentido pues el n se reduce en 1, y querríamos que se mantuviera estático.
listasQueSuman :: Explorador Integer [Integer]
listasQueSuman 0 = [[]] 
listasQueSuman total = [ i : ls  | i <- [1 .. total], ls <- listasQueSuman (total-i)]

--Ejercicio 5
preorder :: Explorador (AB a) a
preorder =  foldAB (\reciz raiz recder -> [raiz] ++ reciz ++ recder ) []

inorder ::  Explorador (AB a) a
inorder = foldAB (\reciz raiz recder -> reciz ++ [raiz] ++ recder ) []

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
                        False-> exp2 x)

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

--Descripcion Ejercicio 11, opcional 1
--La funcion paresSumatoriaLista lo que hace es generar una lista de lista de pares <Lista, sumatoria_elementos_de_Lista>; se llama a esta funcion
-- con un valor de size prefijado (n) y se itera por diferentes sumatoria_total (suma) cuyo valor minimo es en particular n ( lista de n uno's, lista de sumatoria minima de tamaño n)
--el concat es para aplanar ese primer nivel asi quedan en una lista universal todos los pares mencionados para el size fijo y para cada sumatoria total.
--luego el map se hace para quedarse con lo que me interesa de los pares que son las listas, no lo que vale la sumatoria.

--paresSumatoriaLista
--te da una lista de estos pares a partir del size que se especifique y lo que debe valer la sumatoria de sus elementos,
--despues de n iteraciones del foldNat me genera todas las listas de n elementos con sumatoria MENOR O IGUAL a la sumatoria del parametro
--Es por esto que delante del foldNat se utiliza el filter para quedarme con las listas de tamaño n con sumatoria exactamente igual a suma (que es lo que me interesa)
--El foldNat toma el n indicando las n iteraciones, se comienza con un z = [(0,[])]: primer par de una lista vacia con 0 (correspondiente sumatoria)
--en cada iteracion se generara de manera eficiente, a partir de cada par, nuevos pares que contendran la lista anterior agregandole un elemento que no sobrepasa la sumatoria esperada.
--claro que ademas se actualizara el atributo del par que indica la sumatoria total de la lista que se acabo de agregar.
--Hay que notar, nuevamente, que la lista de pares resultantes contiene pares que no nos sirven pero que son generadas por el comportamiento del algoritmo.
--por ejemplo, si llamamos a paresSumatoriaLista 4 3, el par <3, [1,1,1]> es valido ya que nunca sobre pasa el limite que es 4, aqui es donde el filter actua y excluye esta clase de resultados.

listasDeLongitud :: Explorador Integer [Integer]
listasDeLongitud n = map (snd) $ concat $ [paresSumatoriaLista suma n | suma <-[n .. ]]

paresSumatoriaLista :: Integer -> Integer -> [(Integer, [Integer])]
paresSumatoriaLista suma n = filter (\pair -> fst(pair) == suma) $ foldNat (\pairs -> [ (fst(pair) + i, (snd(pair) ++ [i]))   | pair <- pairs, i <- [1 .. suma - fst(pair)]]) [(0,[])] n

--Descripcion Ejercicio 11, opcional 2.
--Comenzando con el caso base [x] (sabemos que lo que devuelven los exploradores siempre son listas de algo entonces esto nos dio la pauta para poner el x entre corchetes como caso base)
--el iterate, en cada iteracion, aplica el explorador (mediante el map) a cada elemento de la ultima lista no explorada de la lista de listas (esta lista sera en principio [x])
--pero como los exploradores devuelven una lista cuando se lo aplica a un elemento entonces aplicarselo a algo de la pinta [x1,x2] uno por uno
-- dará algo como [[y1..yk] , [z1..zj]] entonces lo que se hace es concat de estos resultados para aplanar y juntar los resultados en una unica lista.
--dejandonos algo como esto: [y1..yk, z1..zj] y este será el resultado en un paso del iterate, el take while corrobora si se alcanzo un resultado cuyo valor sea vacio 
--(representado con la lista vacia por la cardinalidad y comportamiento de los exploradores en general.)
(<*>) :: Eq a => Explorador a a -> Explorador a [a] 
(<*>) expl x = let xs = iterate (\xs -> concat $ map expl xs) [x] in 
            takeWhile (\ys -> ys /= []) xs