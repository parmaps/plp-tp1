module PPON where

import Documento
import Documento (foldDoc, vacio)

recr :: b -> (a -> [a] -> b -> b) -> [a] -> b
recr z _ [] = z
recr z f (x:xs) = f x xs (recr z f xs)

data PPON
  = TextoPP String
  | IntPP Int
  | ObjetoPP [(String, PPON)]
  deriving (Eq, Show)

pponAtomico :: PPON -> Bool
pponAtomico pepon = case pepon of 
                        TextoPP _ -> True
                        IntPP _ -> True
                        ObjetoPP _ -> False --puede mejorar

pponObjetoSimple :: PPON -> Bool
pponObjetoSimple pepon = case pepon of 
                            TextoPP _ -> False
                            IntPP _ -> False
                            ObjetoPP xs -> foldr(\ x r -> pponAtomico (snd x) && r ) True xs

intercalar :: Doc -> [Doc] -> Doc
intercalar d = recr vacio (\x xs r -> if xs /= [] then x <+> d <+> r else x <+> vacio) -- consultar con profe

entreLlaves :: [Doc] -> Doc
entreLlaves [] = texto "{ }"
entreLlaves ds =
  texto "{"
    <+> indentar
      2
      ( linea
          <+> intercalar (texto "," <+> linea) ds
      )
    <+> linea
    <+> texto "}"

aplanar :: Doc -> Doc
aplanar = foldDoc vacio <+> (\_ d -> texto " " d)

pponADoc :: PPON -> Doc
pponADoc = error "PENDIENTE: Ejercicio 9"
