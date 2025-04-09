module Documento
  ( Doc,
    vacio,
    linea,
    texto,
    foldDoc,
    (<+>),
    indentar,
    mostrar,
    imprimir,
  )
where

data Doc
  = Vacio
  | Texto String Doc
  | Linea Int Doc
  deriving (Eq, Show)

vacio :: Doc
vacio = Vacio

linea :: Doc
linea = Linea 0 Vacio

texto :: String -> Doc
texto t | '\n' `elem` t = error "El texto no debe contener saltos de línea"
texto [] = Vacio
texto t = Texto t Vacio

-- Ejercicio 1 
foldDoc :: b -> (String -> b -> b) -> (Int -> b -> b) -> Doc -> b
foldDoc fVacio fTexto fLinea documento = case documento of
                                              Vacio -> fVacio
                                              Texto s d -> fTexto s (rec d)
                                              Linea i d -> fLinea i (rec d)
                                              where rec = foldDoc fVacio fTexto fLinea

-- NOTA: Se declara `infixr 6 <+>` para que `d1 <+> d2 <+> d3` sea equivalente a `d1 <+> (d2 <+> d3)`
-- También permite que expresiones como `texto "a" <+> linea <+> texto "c"` sean válidas sin la necesidad de usar paréntesis.
infixr 6 <+>

-- ejercicio 2
(<+>) :: Doc -> Doc -> Doc
d1 <+> d2 = foldDoc d2 concatText Linea d1 --


concatText :: String -> Doc -> Doc
concatText s d = case d of                   
                  Texto s' doc -> Texto (s ++ s') doc
                  x -> Texto s x --
                
-- ejercicio 3
indentar :: Int -> Doc -> Doc
indentar i = foldDoc Vacio Texto (\i' d -> Linea (i+i') d)

-- ejercicio 4
mostrar :: Doc -> String
mostrar = foldDoc "" (++) (\i d -> "\n" ++ replicate i ' ' ++ d)

-- | Función dada que imprime un documento en pantalla

-- ghci> imprimir (Texto "abc" (Linea 2 (Texto "def" Vacio)))
-- abc
--   def

imprimir :: Doc -> IO ()
imprimir d = putStrLn (mostrar d)
