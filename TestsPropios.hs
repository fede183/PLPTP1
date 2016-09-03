module Main where
import Exploradores
import Test.HUnit
import Data.List
import Prelude hiding ((<*>))

--Nota: para poder cargar este archivo es necesario completar todas las declaraciones de tipos que faltan en Exploradores.hs.

-- Evaluar main para correr todos los tests

type Algo = Bool

allTests = test [
  "exploradoresSencillos" ~: testsExpSencillos,
  "exploradoresListas" ~: testsExpListas,
  "exploradoresNaturales" ~: testsExpNaturales,
  "exploradoresAB" ~: testsExpAB,
  "exploradoresRT" ~: testsExpRT,
  "exploradoresCondicional" ~: testsExpCondicional,
  "exploradoresConcatenacion" ~: testsExpConcatenacion,
  "exploradoresComposicion" ~: testsExpComposicion,
  "exploradoresRepeticion" ~: testsExpRepeticion,
  "exploradoresComposicion1" ~: testsExpComposicion1,
  "exploradoresListasDeLongitud" ~: testsExpListasDeLongitud
  ]

testsExpSencillos = test [
    [] ~=? expTail [1]
  ]


testsExpListas = test [
  [['C'], ['A'], ['F'], ['E']] ~=? singletons "CAFE",
  [['C'], ['A'], ['F'], ['E'], [' '], ['C'], ['O'], ['N'], [' '], ['L'], ['E'], ['C'], ['H'], ['E']] ~=? singletons "CAFE CON LECHE",
  sort ["CAFE", "AFE", "FE", "E", []] ~=? sort (sufijos "CAFE"),
  sort ["CAFE CON LECHE", "AFE CON LECHE","FE CON LECHE","E CON LECHE"," CON LECHE","CON LECHE","ON LECHE","N LECHE"," LECHE","LECHE","ECHE","CHE","HE","E",[]] ~=? sort (sufijos "CAFE CON LECHE")
  ]

testsExpNaturales = test [
  sort [[5],[4,1],[1,4],[3,2],[2,3],[3,1,1],[1,3,1],[1,1,3],[1,2,2],[2,1,2],[2,2,1],[2,1,1,1],[1,2,1,1],[1,1,2,1],[1,1,1,2],[1,1,1,1,1]] ~=? sort (listasQueSuman 5),
  sort [[4],[3,1],[1,3],[2,2],[2,1,1],[1,2,1],[1,1,2],[1,1,1,1]] ~=? sort (listasQueSuman 4)
  ]

testsExpAB = test [
  "CAFE" ~=? preorder ab8,
  "CAFE CON" ~=? preorder ab9,
  "CAFE CON LECHE" ~=? preorder ab10,
  "CAFE CON LECHE" ~=? preorder ab11,
  "CAFE CON LECHE" ~=? preorder ab12,
  "FAEC" ~=? inorder ab8,
  "FAECOCN " ~=? inorder ab9,
  "FAECOCN CEHLE " ~=? inorder ab10,
  "EHCEL NOC EFAC" ~=? inorder ab11,
  "CAFE CON LECHE" ~=? inorder ab12,
  "FEAC" ~=? postorder ab8,
  "FEAONC C" ~=? postorder ab9,
  "FEAONCCHEEL  C" ~=? postorder ab10,
  "EHCEL NOC EFAC" ~=? postorder ab11,
  "EHCEL NOC EFAC" ~=? postorder ab12
  ]

testsExpRT = test [
  " LECHE" ~=? dfsRT rt1,
  " CON" ~=? dfsRT rt2,
  "CAFE" ~=? dfsRT rt3,
  "CAFE CON LECHE" ~=? dfsRT rt4,
  "CAFE CON LECHE" ~=? dfsRT rt5,
  "CAFE CON LECHE" ~=? dfsRT rt6,

  [" L"," E"," C"," H"," E"]  ~=? ramasRT rt1,
  [" C"," O"," N"] ~=? ramasRT rt2,
  ["CA","CF","CE"]  ~=? ramasRT rt3,
  ["CA","CF","CE","C C","C O","C N","C L","C E","C C","C H","C E"] ~=? ramasRT rt4,
  ["CAFE CON LECHE"] ~=? ramasRT rt5,
  ["CA","CF","CE","C ","CC","CO","CN","C ","CL","CE","CC","CH","CE"] ~=? ramasRT rt6,

  "LECHE" ~=? hojasRT rt1,
  "CON" ~=? hojasRT rt2,
  "AFE" ~=? hojasRT rt3,
  "AFECONLECHE" ~=? hojasRT rt4,
  "E" ~=? hojasRT rt5,
  "AFE CON LECHE" ~=? hojasRT rt6
  ]

testsExpCondicional = test [
    [2] ~=? ifExp even expId expNulo 2,
    [] ~=? ifExp even expId expNulo 3,
    sort [[1,1,1],[1,2],[2,1],[3]] ~=? sort (ifExp (\x-> x < 4) listasQueSuman expNulo 3),
    [] ~=? ifExp (\x-> x < 4) listasQueSuman expNulo 100
    ]

testsExpConcatenacion = test [
  --[4,2,1,3,6,5,7,1,2,3,4,5,6,7] ~=? (preorder <++> inorder) ab5,
  --sort [[1, 1, 1], [1, 2], [2, 1], [3], [1, 1, 1], [1, 2], [2, 1], [3]] ~=? sort ((listasQueSuman <++> listasQueSuman) 3),
  --[1,2,0,5,3,5,6,7,4, 0,5,5,7,4] ~=? (dfsRT <++> hojasRT) rt6,
  sort ["w", "x", "y", "z", "wxyz", "xyz", "yz", "z", ""] ~=? sort ((singletons <++> sufijos) "wxyz")
  ]

testsExpComposicion = test [
  [1,2,3,1,2,3,4,5] ~=? ( (\x->[1..x]) <.> (map (+1)) ) [2,4],
  sort [[7],[4],[5],[4],[5],[5]] ~=? sort (( singletons <.> sufijos ) [7,4,5])
  --sort [[1,2],[2],[],[1,3,5],[3,5],[5],[],[1,3,6,7],[3,6,7],[6,7],[7],[],[1,4],[4],[]] ~=? sort (( sufijos <.> ramasRT ) rt4),
  --[abHoja 4,abHoja 5, abNil, abNil] ~=? ( expHijosAB <.> expHijosAB ) ab4
  ]

testsExpRepeticion = test [
  --[abHoja 1,abHoja 3, abHoja 5, abHoja 7] ~=? (expHijosAB <^>2) ab5,
  --[ab1,ab2] ~=? (expHijosAB <^>1) ab3,
  --[rtHoja 0, rtHoja 5, rtHoja 5, rt11] ~=? (expHijosRT <^>2) rt6,
  --expHijosRT rt6 ~=? (expHijosRT <^>1) rt6,
  expId 32 ~=? (expId <^> 100) 32,
  [[1], [2], [3]] ~=? (singletons <^> 3) [1,2,3],
  sort [[1,2,3],[2,3],[3],[],[2,3],[3],[],[3],[],[],[2,3],[3],[],[3],[],[],[3],[],[],[]] ~=? sort ((sufijos <^> 3) [1,2,3])
  ]

testsExpListasDeLongitud = test [
  all (\x->elem x (listasDeLongitud 2)) [[1,1],[1,2],[2,1],[1,3],[2,2],[3,1]] ~=? True,
  [] ~=? take 0 (listasDeLongitud 3),
  all (\x->elem x (listasDeLongitud 8)) [[1,1,1,1,1,1,1,1],[1,1,1,1,1,1,1,2],[1,1,1,1,1,1,2,1]] ~=? True
  -- True ~=? elem [9,10,9] (listasDeLongitud 3)
  ]

testsExpComposicion1 = test [
 [[1],[1],[1],[1],[1],[1]] ~=? take 6 (expId <*> 1),
 [1,6,21] ~=? map length (take 3 $ sufijos<*> [1,2,3,4,5])
 --[abHoja 4, abHoja 5, abHoja 6, abHoja 7] ~=? (last $ init $ expHijosAB <*> ab3),
 --(expHijosAB <^>2) ab3  ~=? (last $ init $ expHijosAB <*> ab3)
 ]


--Ejecución de los tests
main :: IO Counts
main = do runTestTT allTests


----------
--Estructuras para pruebas
abHoja x = Bin Nil x Nil
abNil = Nil
ab1 = Bin (abHoja 'F') 'A' (abHoja 'E')
ab2 = Bin (abHoja 'C') 'E' (abHoja 'H')
ab3 = Bin (ab2) 'L' (abHoja 'E')
ab4 = Bin (abHoja 'O') 'C' (abHoja 'N')

ab5 = Bin (ab4) ' ' (Nil)-- CON
ab6 = Bin (ab3) ' ' (Nil)-- LECHE
ab7 = Bin (ab4) ' ' (ab6)-- CON LECHE

ab8 = Bin (ab1) 'C' (Nil)--CAFE
ab9 = Bin (ab1) 'C' (ab5)--CAFE CON
ab10 = Bin (ab1) 'C' (ab7)--CAFE CON LECHE

ab11 = Bin (Bin (Bin (Bin (Bin (Bin (Bin (Bin (Bin (Bin (Bin (Bin (Bin (Bin (Nil) 'E' (Nil)) 'H' (Nil)) 'C' (Nil)) 'E' (Nil)) 'L' (Nil)) ' ' (Nil)) 'N' (Nil)) 'O' (Nil)) 'C' (Nil)) ' ' (Nil)) 'E' (Nil)) 'F' (Nil)) 'A' (Nil)) 'C' (Nil)
ab12 = Bin (Nil) 'C' (Bin (Nil) 'A' (Bin (Nil) 'F' (Bin (Nil) 'E' (Bin (Nil) ' ' (Bin (Nil) 'C' (Bin (Nil) 'O' (Bin (Nil) 'N' (Bin (Nil) ' ' (Bin (Nil) 'L' (Bin (Nil) 'E' (Bin (Nil) 'C' (Bin (Nil) 'H' (Bin (Nil) 'E' (Nil))))))))))))))

rtHoja x = Rose x []
rt11 = rtHoja 'L'
rt12 = rtHoja 'E'
rt13 = rtHoja 'C'
rt14 = rtHoja 'H'
rt15 = rtHoja 'E'
rt1 = Rose ' ' [rt11,rt12,rt13,rt14,rt15] -- LECHE
rt21 =  rtHoja 'C'
rt22 =  rtHoja 'O'
rt23 =  rtHoja 'N'
rt2 = Rose ' ' [rt21,rt22,rt23] -- CON
rt31 =  rtHoja 'A'
rt32 =  rtHoja 'F'
rt33 =  rtHoja 'E'
rt3 = Rose 'C' [rt31,rt32,rt33] --CAFE
rt4 = Rose 'C' [rt31,rt32,rt33,rt2,rt1] --CAFE CON LECHE
rt5 = Rose 'C' [Rose 'A' [Rose 'F' [Rose 'E' [Rose ' ' [Rose 'C' [Rose 'O' [Rose 'N' [Rose ' ' [Rose 'L' [Rose 'E' [Rose 'C' [Rose 'H' [Rose 'E' []]]]]]]]]]]]]]
rt6 = Rose 'C' [Rose 'A' [],Rose 'F' [],Rose 'E' [],Rose ' ' [],Rose 'C' [],Rose 'O' [],Rose 'N' [],Rose ' ' [],Rose 'L' [],Rose 'E' [],Rose 'C' [],Rose 'H' [],Rose 'E' []]

--rt1 = rtHoja 1
--rt2 = Rose 0 [rt1, rt1]
--rt3 = Rose 1 [rt7, rt8]
--rt4 = Rose 1 [(Rose 2 []),(Rose 3 [(Rose 5 []),(Rose 6 [Rose 7 []])]),(Rose 4 [])]
--rt5 = Rose () $ replicate 4 rt5
--rt6 = Rose 1 [rt9, rt10, rtHoja 4]
--rt7 = Rose 2 [rt1]
--rt8 = Rose 3 [rt2]
--rt9 = Rose 2 [rtHoja 0, rtHoja 5]
--rt10 = Rose 3 [rtHoja 5,rt11]
--rt11 = Rose 6 [rtHoja 7]


--
--Ejemplos:
--take 10 $ expId <*> 1 ~> [[1],[1],[1],[1],[1],[1],[1],[1],[1],[1]]
--take 3 $ sufijos<*> [1,2,3,4,5] ~>
--[[[1,2,3,4,5]],[[1,2,3,4,5],[2,3,4,5],[3,4,5],[4,5],[5],[]],[[1,2,3,4,5],[2,3,4,5],[3,4,5],[4,5],[5],[],[2,3,4,5],[3,4,5],[4,5],[5],[],[3,4,5],[4,5],[5],[],[4,5],[5],[],[5],[],[]]]
--expHijosAB <*> ab5
--last $ init $ expHijosAB <*> ab3 da las 4 hojas con raíces 4, 5, 6, 7.
--(expHijosAB <^>2) ab3 es equivalente a lo anterior.
--expHijosRT <*> rt4
--(takeWhileAB (< 5) ab3) == (Bin (Bin (Bin Nil 4 Nil) 2 Nil) 1 (Bin Nil 3 Nil))
--dfsRT rt4 ~> [1,2,3,5,6,7,4]
--inorder ab3 ~> [4,2,5,1,6,3,7]
--inorder ab5 ~> [1,2,3,4,5,6,7]
--dfsRT $ head $ truncarRT rt4 ~> [1,3,6]
--take 10 $ aplicacionesInfinitas (+) 1 ~> [1,2,3,4,5,6,7,8,9,10]
--take 3 $ aplicacionesInfinitas (flip podarNivel) rt5
