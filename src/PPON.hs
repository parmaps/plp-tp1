module PPON where

import Documento

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
                        ObjetoPP _ -> False

pponObjetoSimple :: PPON -> Bool
pponObjetoSimple pepon = case pepon of 
                            TextoPP _ -> False
                            IntPP _ -> False
                            ObjetoPP xs -> foldr(\ x r -> pponAtomico (snd x) && r ) True xs

intercalar :: Doc -> [Doc] -> Doc
-- intercalar d = recr vacio (\x xs r -> if xs /= [] then x <+> d <+> r else x <+> vacio) -- hacer con foldr1, hacer un caso de lista vacia
-- intercalar vacio = vacio
intercalar d = foldr1 (\x r-> if r == vacio then x <+> d <+> vacio else x <+> d <+> r)

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

pponADoc :: PPON -> Doc
pponADoc pepon = case pepon of 
                    ObjetoPP xs -> if pponObjetoSimple pepon
                                    then aplanar (entreLlaves (recPpon xs))
                                    else entreLlaves (recPpon xs)
                    IntPP s -> texto (show s) 
                    TextoPP s -> texto (show s)
                  where recPpon = map (\(s,p) -> (texto (show s) <+> texto ": " <+> pponADoc p))
