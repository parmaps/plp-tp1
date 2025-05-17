module PPON where

import Documento

-- recr :: b -> (a -> [a] -> b -> b) -> [a] -> b
-- recr z _ [] = z
-- recr z f (x:xs) = f x xs (recr z f xs)

data PPON
  = TextoPP String
  | IntPP Int
  | ObjetoPP [(String, PPON)]
  deriving (Eq, Show)

pponAtomico :: PPON -> Bool
pponAtomico pepon = case pepon of 
                        TextoPP _ -> True
                        IntPP _ -> True
                        ObjetoPP _ -> False

pponObjetoSimple :: PPON -> Bool
pponObjetoSimple pepon = case pepon of 
                            TextoPP _ -> False
                            IntPP _ -> False
                            ObjetoPP xs -> foldr(\ x r -> pponAtomico (snd x) && r ) True xs

intercalar :: Doc -> [Doc] -> Doc
intercalar _ [] = vacio
intercalar sep docs = foldr1 (\doc resto -> doc <+> sep <+> resto) docs
  

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
aplanar = foldDoc vacio (\s d -> texto s <+> d) (\_ d -> texto " " <+> d)

-- Justificacion de Recursion
-- Asumiendo que las funciones(aplanar, entreLlaves) utilizadas en la funcion pponADoc no modifican el tipo de recursion
-- podemos decir que la funcion pponADoc utiliza recursion estructural, ya que cumple con tener una funcion
-- para cada constructor, pepon solo se usa en el llamado recursivo y el caso base tiene un valor fijo.
pponADoc :: PPON -> Doc
pponADoc pepon = case pepon of 
                    ObjetoPP xs -> if pponObjetoSimple pepon
                                    then aplanar (entreLlaves (recPpon xs))
                                    else entreLlaves (recPpon xs)
                    IntPP s -> texto (show s) 
                    TextoPP s -> texto (show s)
                  where recPpon = map (\(s,p) -> (texto (show s) <+> texto ": " <+> pponADoc p))

