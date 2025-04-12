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

-- Justificacion Invariante
-- Se satisface el Invariante de Doc porque:
-- En primer lugar, al usar foldDoc nos aseguramos de estar procesando la estructura recursiva de manera ordenada,
-- sin mezclar los constructores de Vacio, Texto y Linea.
-- Los documentos Vacio no modifican los documentos con los que se concatenan.
-- Los documentos Texto solo se concatenarán directamente con documentos Texto válidos procesados mediante {concatText}, 
-- ya que de esto se encarga la función {texto} (que evitar construir textos con saltos de linea y strings vacios).
-- Los documentos Linea mantendrán i >= 0 por la propia definicion del constructor Linea.
(<+>) :: Doc -> Doc -> Doc
d1 <+> d2 = foldDoc d2 concatText Linea d1 --


concatText :: String -> Doc -> Doc
concatText s d = case d of                   
                  Texto s' doc -> Texto (s ++ s') doc
                  x -> Texto s x --
                
-- Justificacion Invariante
-- Se mantiene el Invariante de Doc porque:
-- Se mantienen las razones del ejercio 2 (<+>) respecto a foldDoc, los constructores Vacio, Texto y la funcion {texto},
-- ya que estos casos se preseveran inalterados.
-- Se modifica la indenteacion usando (Linea (i+i'), 
-- y la precondicion explicita que siempre se agrega un numero mayor que 0 de espacios (i > 0),
-- y como el el invariante del Doc original era i' >= 0 siempre se cumplirá que Linea (i+i') >= 0.
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
