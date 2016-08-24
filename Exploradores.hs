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
expNulo = \x 

expId :: Explorador a a
expId = \x -> [x]

expHijosRT :: Explorador (RoseTree a) (RoseTree a)
expHijosRT = \r -> listaRT(r)

expHijosAB :: Explorador(AB a) (AB a)
expHijosAB = \a -> listaAB(a)

expTail :: Explorador [a] a
expTail = \a -> cola(a)

--Ejercicio 2
--foldNat :: undefined
foldNat = undefined

--foldRT :: undefined
foldRT = undefined

--foldAB :: undefined
foldAB = undefined

--Ejercicio 3
singletons :: Explorador [a] [a]
singletons = \x -> listaPorElemento(x)

sufijos :: Explorador [a] [a]
sufijos = \x -> todosLosSubfijos(x)

--Ejercicio 4
--listasQueSuman :: Explorador Integer ?
listasQueSuman = undefined

--Ejercicio 5
preorder :: Explorador (AB a) a
preorder = \x -> preOrderAux(x)

inorder :: Explorador (AB a) a
inorder = \x -> inorderAux(x)

postorder :: Explorador (AB a) a
postorder = \x -> postorderAux(x)

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
