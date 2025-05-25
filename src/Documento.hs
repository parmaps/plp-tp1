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

-- Ejercicio 2

-- Justificacion Invariante
-- Se satisface el Invariante de Doc:
-- En esta funcion trabajamos con foldDoc que esta bien contruido porque trabaja con constructores bien formados 
-- y no genera nuevos de forma arbitraria.

-- Texto s d: utiliza la funcion concatText que nunca va a generar un Texto s d con s vacío ni con saltos de línea.
--Esto se garantiza porque en los dos casos posibles:
--si el segundo argumento (d2) es tambien Texto s' doc, entonces contruye uno nuevo concatenando el s de un documento d1 
--y el s' d2 (y sabemos que s, s' y doc ya cumplen el invariante)
--sino, se contruye un nuevo Texto con s y un doc que puede ser Vacio o Linea i d y también siguen cumpliendo con el invariante.

--Linea s d: en este caso, trabaja directamente con el contructor Linea y sin modificar del valor de i por lo que podemos 
--asegurar que i sigue cumpliendo la condición de i >= 0.

--Por último, dado que no se exportan constructores Doc para uso directo, nos aseguramos que en la generación o 
--concatenación se preserve el invariante.

(<+>) :: Doc -> Doc -> Doc
d1 <+> d2 = foldDoc d2 concatText Linea d1 


concatText :: String -> Doc -> Doc
concatText s d = case d of                   
                  Texto s' doc -> Texto (s ++ s') doc
                  x -> Texto s x --
                
-- Ejercicio 3
-- Justificacion Invariante
-- Se mantiene el Invariante de Doc porque:

-- En el caso de ser texto, el string de entrada no se modifica, usando texto para mantener el invariante.

-- con respecto a los saltos de línea, pasa lo mismo. No se modifica el string, y esto significa que no se
-- le agrega ningún salto de línea.

-- D ingresa y por foldDoc entra a indentar de nuevo, no se modifica el invariante con respecto a el, 
--ya que solo se amplían los saltos de línea, el texto y el d no se modifican en esa instancia.

-- En el caso de que sea Línea, se mantiene el invariante de que i>=0, ya que solo se suma un
-- i positivo al número de línea que por invariante de entrada debe ser positivo.

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
