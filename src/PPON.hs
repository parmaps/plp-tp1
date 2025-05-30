module PPON where

import Documento

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

--* Justificacion de Recursion
-- La recursion que utiliza es primitiva y NO estructural ya que
-- se le pasa pepon (la subestructura) a pponObjetoSimple, 
-- lo cual hace que acceda a la subestructura sin modificar,
-- y este comportamiento no es valido en la recursion estructural 
-- Por otro lado, tambien cumple con que se accede a la subestructura en el llamado recursivo
pponADoc :: PPON -> Doc
pponADoc pepon = case pepon of 
                    ObjetoPP xs -> if pponObjetoSimple pepon
                                    then aplanar (entreLlaves (recPpon xs))
                                    else entreLlaves (recPpon xs)
                    IntPP s -> texto (show s) 
                    TextoPP s -> texto (show s)
                  where recPpon = map (\(s,p) -> texto (show s) <+> texto ": " <+> pponADoc p)

