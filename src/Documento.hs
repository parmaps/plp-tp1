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
-- En el caso de ser texto, el texto aplica la función texto (ya que en la parte que el foldDoc discrimina ese caso se aplica en la función texto en <+>)
-- Si no viene con saltos de línea, no se agregan, dado que texto replica el mismo texto que viene sin agregar nada
-- d ingresa nuevamente al foldDoc con las funciones especificadas en el <+>
-- Siendo línea, entra en el caso de la funcion Linea, que se da de la misma manera en la que ingresa, y de esta manera no cambia el hecho de que i >= 0. Simplemente no se modifica la entrada
-- A su vez, como aclara el PDF de la consigna, nos aseguramos de mantener los invariantes internos usando las funciones que nos evitan exportar los constructores

(<+>) :: Doc -> Doc -> Doc
d1 <+> d2 = foldDoc d2 concatText Linea d1 --


concatText :: String -> Doc -> Doc
concatText s d = case d of                   
                  Texto s' doc -> Texto (s ++ s') doc
                  x -> Texto s x --
                
-- Justificacion Invariante
-- Se mantiene el Invariante de Doc porque:
-- En el caso de ser texto, el string de entrada no se modifica, usando texto para mantener el invariante.
-- con respecto a los saltos de línea, pasa lo mismo. No se modifica el string, y esto significa que no se le agrega ningún salto de línea.
-- D ingresa y por foldDoc entra a indentar de nuevo, no se modifica el invariante con respecto a el, ya que solo se amplían los saltos de línea, el texto y el d no se modifican en esa instancia.
-- En el caso de que sea Línea, se mantiene el invariante de que i>=0, ya que solo se suma un i positivo al número de línea que por invariante de entrada debe ser positivo.

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
