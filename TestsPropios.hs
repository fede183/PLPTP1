module Main where
import Exploradores
import Test.HUnit
import Data.List
import Prelude hiding ((<*>))

abEsTodoIzquierda:: AB a -> Bool
abEsTodoIzquierda Nil = True
abEsTodoIzquierda (Bin Nil a Nil) = True
abEsTodoIzquierda (Bin Nil a b) = False
abEsTodoIzquierda (Bin e a Nil) = True
abEsTodoIzquierda (Bin e a b) = False

abEsTodoDerecha:: AB a -> Bool
abEsTodoDerecha Nil = True
abEsTodoDerecha (Bin Nil a Nil) = True
abEsTodoDerecha (Bin Nil a b) = True
abEsTodoDerecha (Bin e a Nil) = False
abEsTodoDerecha (Bin e a b) = False

allTests = test [
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
    "FAECOCN CEHLE " ~=? ifExp abEsTodoIzquierda preorder inorder ab10,
    "CAFE CON LECHE" ~=? ifExp abEsTodoIzquierda preorder inorder ab11,
    "CAFE CON LECHE" ~=? ifExp abEsTodoDerecha preorder inorder ab12,

    "FAECOCN CEHLE " ~=? ifExp abEsTodoDerecha preorder inorder ab10,
    "EHCEL NOC EFAC" ~=? ifExp abEsTodoDerecha preorder inorder ab11,
    "CAFE CON LECHE" ~=? ifExp abEsTodoIzquierda preorder inorder ab12

    ]

testsExpConcatenacion = test [
  "CAFE CON LECHEAFECONLECHE" ~=? (dfsRT <++> hojasRT) rt4,
  "CAFE CON LECHEE" ~=? (dfsRT <++> hojasRT) rt5,
  "CAFE CON LECHEAFE CON LECHE" ~=? (dfsRT <++> hojasRT) rt6
  ]

testsExpComposicion = test [
  [['C'], ['A'], ['F'], ['E'], [' '], ['C'], ['O'], ['N'], [' '], ['L'], ['E'], ['C'], ['H'], ['E']] ~=? ( singletons <.> ramasRT ) rt5
  ]

testsExpRepeticion = test [
  sort ["","","","","","AFE","AFE","CAFE","E","E","E","E","FE","FE","FE"] ~=? sort ((sufijos <^> 2) "CAFE"),
  [abHoja 'F',abHoja 'E',ab4,ab6] ~=? (expHijosAB <^> 2) ab10
  ]

testsExpListasDeLongitud = test [

  [[1,1,1],[1,1,2],[1,2,1],[2,1,1],[1,1,3]]  ~=? take 5 (listasDeLongitud 3)
  ]

testsExpComposicion1 = test [
 [["CAFE"],["CAFE","AFE","FE","E",""]]  ~=? take 2 (sufijos <*> "CAFE"),
 [[ab4],[abHoja 'O',abHoja 'N'],[Nil,Nil,Nil,Nil]]  ~=? take 3 (expHijosAB <*> ab4)

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
