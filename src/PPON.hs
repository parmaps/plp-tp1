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
-- Asumiendo que las funciones (aplanar, entreLlaves) utilizadas en la función pponADoc no modifican el tipo de recursión,
-- podemos decir que la función pponADoc utiliza recursión primitiva.
-- Cumple con tener una definición para cada constructor del tipo PPON, 
-- los casos base tienen un valor fijo que no depende de llamadas recursivas
-- y en el caso del constructor ObjetoPP xs, 
-- la funcion accede a la subestructura xs (una lista de pares (s,p)) para mapear su contenido
-- y procesar explicítamente la clave 's' antes de aplicar la recursion sobre el valor 'p'.


pponADoc :: PPON -> Doc
pponADoc pepon = case pepon of 
                    ObjetoPP xs -> if pponObjetoSimple pepon
                                    then aplanar (entreLlaves (recPpon xs))
                                    else entreLlaves (recPpon xs)
                    IntPP s -> texto (show s) 
                    TextoPP s -> texto (show s)
                  where recPpon = map (\(s,p) -> texto (show s) <+> texto ": " <+> pponADoc p)

