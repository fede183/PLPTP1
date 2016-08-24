module Exploradores (Explorador, AB(Nil,Bin), RoseTree(Rose), foldNat, foldRT, foldAB, expNulo, expId, expHijosRT, expHijosAB, expTail, ifExp, singletons, sufijos, inorder, preorder, postorder, dfsRT, ramasRT, hojasRT, listasQueSuman, listasDeLongitud, (<.>), (<^>), (<++>), (<*>)) where

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

listaAB :: AB a-> [AB a]
listaAB Nil = []
listaAB Bin (AB a) r (AB b) = (a:(b:[]))

listaRT :: RoseTree a -> [RoseTree a]
listaRT Rose a = []
listaRT Rose a b = b

cola :: [a] -> [a]
cola [] = []
cola (x:xs) = xs

listaPorElemento :: [a] -> [[a]]
listaPorElemento [] = []
listaPorElemento (x:xs) = [x]: listaPorElemento(xs)

todosLosSubfijos :: [a] -> [[a]]
todosLosSubfijos [] = []
todosLosSubfijos (x:xs) = (x:xs):todosLosSubfijos(xs)

preOrderAux :: AB a -> [a]
preOrderAux Nil = []
preOrderAux Bin (AB a) r (AB b) = r : (preOrderAux(a) ++ preOrderAux(b))

inorderAux :: AB a -> [a]
inorderAux Nil = []
inorderAux Bin (AB a) r (AB b) = inorderAux(a) ++ [r] ++ inorderAux(b)

postorderAux :: AB a -> [a]
postorderAux Nil = []
postorderAux Bin (AB a) r (AB b) = postorderAux(a) ++ postorderAux(b) ++ [r]

--Ejercicio 1
expNulo :: Explorador a b
expNulo = (\x -> [])

expId :: Explorador a a
expId = (\x -> [x])

expHijosRT :: Explorador (RoseTree a) (RoseTree a)
expHijosRT = (\r -> listaRT(r))

expHijosAB :: Explorador(AB a) (AB a)
expHijosAB = (\a -> listaAB(a))

expTail :: Explorador [a] a
expTail = (\a -> cola(a))

--Ejercicio 2
foldNat :: (Integer -> b -> b) -> b -> Integer -> b
foldNat recu base n | n < 0 = error "No se permiten numero negativos"
                    | n == 0 = base
                    | otherwise = recu n (foldNat recu base (n-1))

foldRT :: (a -> [b] -> b) -> RoseTree a -> b
foldRT recu (Rose root hijos) = recu root (map (foldRT recu) hijos)

foldAB :: (b -> a -> b -> b) -> b -> AB a -> b
foldAB recu base Nil = base
foldAB recu base (Bin izq root der) = recu (foldAB recu base izq) root (foldAB recu base der)

--Ejercicio 3
singletons :: Explorador [a] [a]
singletons = \x -> listaPorElemento(x)

sufijos :: Explorador [a] [a]
sufijos = \x -> todosLosSubfijos(x)

--Ejercicio 4
listasQueSuman :: Explorador Integer [Integer]
listasQueSuman = (\n -> if n == 1 then [[1]] else [n]:[y:lista | y <- [1..n-1], lista <- listasQueSuman (n-y)])
-- ideas de porqué no se puede usar foldNat:
-- el caso base de foldNat es 0, y nosotros queremos parar en 1.
-- También, el primer elemento que procesa es n, cuando lo que
-- no hace recursion estructural ?

--Ejercicio 5

preorder :: Explorador (AB a) a
preorder = \x -> preOrderAux(x)

inorder :: Explorador (AB a) a
inorder = \x -> inorderAux(x)

postorder :: Explorador (AB a) a
postorder = \x -> postorderAux(x)

--Ejercicio 6
dfsRT :: Explorador (RoseTree a) a
dfsRT = foldRT (\root recu -> root:concat recu)

hojasRT :: Explorador (RoseTree a) a
hojasRT = foldRT (\root recu -> if length recu == 0 then [root] else concat recu)

ramasRT :: Explorador (RoseTree a) [a]
ramasRT = foldRT (\root recu -> if length recu == 0 then [[root]] else map ((:) root) (concat recu))

--Ejercicio 7
ifExp :: (a->Bool) -> Explorador a b -> Explorador a b -> Explorador a b
ifExp condicion exp1 exp2 = (\estructura -> if condicion estructura then exp1 estructura else exp2 estructura)

--Ejercicio 8
(<++>) :: Explorador a b -> Explorador a b -> Explorador a b
(<++>) exp1 exp2 = (\estructura -> exp1 estructura ++ exp2 estructura)

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
