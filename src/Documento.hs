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
d1 <+> d2 = case d1 of
              vacio -> procesDoc d2
              Texto s d -> concatText s (procesDoc d2)
              Linea i d -> concatLinea i (procesDoc d2)

procesDoc :: Doc -> Doc
procesDoc = foldDoc Vacio concatText concatLinea;

concatText :: String -> Doc -> Doc
concatText s d = case d of 
                  vacio -> Texto s Vacio 
                  Texto s' doc -> Texto (s ++ s') doc
                  Linea i doc -> Texto s (Linea i doc)

concatLinea :: Int -> Doc -> Doc
concatLinea i d = case d of
                  vacio -> Linea i Vacio
                  Texto s doc -> Linea i (Texto s doc)
                  Linea i' doc -> Linea i (Linea i' doc)
                

-- ejercicio 3
indentar :: Int -> Doc -> Doc
indentar = indentar
-- indentar i = foldDoc <+> (Linea i <+>) tiene errores

-- ejercicio 4
mostrar :: Doc -> String
mostrar = mostrar
-- mostrar = foldDoc "" (++) (no se)

-- | Función dada que imprime un documento en pantalla

-- ghci> imprimir (Texto "abc" (Linea 2 (Texto "def" Vacio)))
-- abc
--   def

imprimir :: Doc -> IO ()
imprimir d = putStrLn (mostrar d)
