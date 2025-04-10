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
intercalar d = recr vacio (\x xs r -> if xs /= [] then x <+> d <+> r else x <+> vacio) -- hacer con foldr1, hacer un caso de lista vacia

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
                    -- ObjetoPP ((s,p):xs) -> (texto s (pponADoc p)) 
                    ObjetoPP xs -> texto "{" <+> foldr(\(s,p) r -> (texto s <+> pponADoc p) <+> r) vacio xs 
                    IntPP s -> texto (show s) -- usar entre llaves
                    TextoPP s -> texto (show s) -- condicion objeto simple

-- aDoc :: [Doc] -> Doc
-- aDoc [] = vacio
-- aDoc (x:xs) = x <+> aDoc xs

--ObjetoPP cosas -> aDoc(map(\(x,o) -> (texto ((show x)++ ":" ++ (show o) ++ ",")))cosas)

-- data PPON
--   = TextoPP String
--   | IntPP Int
--   | ObjetoPP [(String, PPON)]
--   deriving (Eq, Show)