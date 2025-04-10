module Main (main) where

import Documento
import PPON
import Test.HUnit

main :: IO ()
main = runTestTTAndExit allTests

allTests :: Test
allTests =
  test
    [ "Ejercicio 2" ~: testsEj2,
      "Ejercicio 3" ~: testsEj3,
      "Ejercicio 4" ~: testsEj4,
      "Ejercicio 6" ~: testsEj6,
      "Ejercicio 7" ~: testsEj7,
      "Ejercicio 8" ~: testsEj8,
      "Ejercicio 9" ~: testsEj9
    ]

testsEj2 :: Test
testsEj2 =
  test
    [ vacio <+> vacio ~?= vacio,
      texto "a" <+> texto "b" ~?= texto "ab",
      (texto "a" <+> linea) <+> texto "b" ~?= texto "a" <+> (linea <+> texto "b"),

      -- Casos propios:      
      vacio <+> texto "a" <+> vacio ~?= texto "a", -- texto entre documentos vacios
      (texto "a" <+> texto "b") <+> texto "c"  ~?=  texto "a" <+> (texto "b" <+> texto "c"),  -- asociatividad con textos
      linea <+> texto "a" <+> texto "b" ~?= linea <+> (texto "a" <+> texto "b"), -- asociatividad trivial (por infixr) con linea
      (linea <+> texto "a") <+> texto "b" ~?= linea <+> (texto "a" <+> texto "b"), -- asociatividad con linea

      linea <+> texto "a" <+> texto "b" <+> linea <+> texto "c" <+> texto "d" ~?= linea <+> (texto "a" <+> (texto "b" <+> linea <+> texto "c" <+> texto "d")), -- concatenacion de multiples documentos
      linea <+> texto "a" <+> vacio <+> texto "b" <+> vacio <+> texto "c" <+> linea ~?= linea <+> (texto "a" <+> (texto "b" <+> texto "c")) <+> linea, -- concatenacion de multiples documentos con vacio entre medio
      (linea <+> texto "a" <+> linea <+> (texto "b" <+> linea <+>  texto "c" <+> texto "d")) ~?= linea <+> texto "a" <+> linea <+> texto "b" <+> linea <+> texto "c" <+> texto "d"-- concatenacion de multiples documentos es equivalente a aplanar los docs
      
    ]

testsEj3 :: Test
testsEj3 =
  test
    [ indentar 2 vacio ~?= vacio,
      indentar 2 (texto "a") ~?= texto "a",
      indentar 2 (texto "a" <+> linea <+> texto "b") ~?= texto "a" <+> indentar 2 (linea <+> texto "b"),
      indentar 2 (linea <+> texto "a") ~?= indentar 1 (indentar 1 (linea <+> texto "a")),

      -- Casos propios:         
      indentar 1 (vacio <+> texto "a" <+> vacio <+> linea) ~?= texto "a" <+> indentar 1 linea, -- indentar con vacios intermedios
      indentar 1 (linea <+> linea <+> texto "a" <+> linea) ~?= indentar 1 linea <+> indentar 1 linea <+> texto "a" <+> indentar 1 linea, -- indentar con varias lineas seguidas
      indentar 2 (texto "a") <+> linea <+> texto "b" <+> linea <+> texto "c" ~?= texto "a" <+> linea <+> texto "b" <+> linea <+> texto "c", -- indentar solo texto no modifica la estructura de un documento con multiples concatenaciones
      indentar 3 (texto "a" <+> linea <+> (texto "b" <+> linea <+> texto "c")) ~?= texto "a" <+> indentar 3 linea <+> texto "b" <+> indentar 3 linea <+> texto "c" -- indentar documentos anidados
    ]

testsEj4 :: Test
testsEj4 =
  test
    [ mostrar vacio ~?= "",
      mostrar linea ~?= "\n",
      mostrar (indentar 2 (texto "a" <+> linea <+> texto "b")) ~?= "a\n  b",

      -- Casos propios:
      mostrar (vacio <+> vacio <+> vacio) ~?= "",--mostrar multiples vacios concatenados
      mostrar (linea <+> linea <+> linea) ~?= "\n\n\n",--mostrar multiples lineas concatenadas
      mostrar (indentar 1 (texto "a" <+> vacio <+> texto "b" <+> vacio <+> texto "c")) ~?= "abc",--mostrar multiples textos concatenados e intercalados con vacios e indentacion
      mostrar (indentar 2 (texto "a" <+> linea <+> texto "b" <+> linea <+> texto "c")) ~?= "a\n  b\n  c", --mostrar multiples elementos indentados y concatenados
      mostrar (indentar 2 (texto "a" <+> linea <+> texto "b" <+> linea <+> texto "c")) ~?= "a\n  b\n  c", --mostrar multiples elementos indentados y concatenados
      mostrar (indentar 3 (linea <+> texto "a") <+> indentar 2 (linea <+> texto "b" <+> linea <+> texto "c" <+> linea)) ~?= "\n   a\n  b\n  c\n  " --mostrar multiples elementos indentados y concatenados
    ]

pericles, merlina, addams, familias :: PPON
pericles = ObjetoPP [("nombre", TextoPP "Pericles"), ("edad", IntPP 30)]
merlina = ObjetoPP [("nombre", TextoPP "Merlina"), ("edad", IntPP 24)]
addams = ObjetoPP [("0", pericles), ("1", merlina)]
familias = ObjetoPP [("Addams", addams)]

testsEj6 :: Test
testsEj6 =
  test
    [ pponObjetoSimple pericles ~?= True,
      pponObjetoSimple addams ~?= False,

      -- Casos propios:
      pponObjetoSimple merlina ~?= True,
      pponObjetoSimple familias ~?= False
    ]

a, b, c :: Doc
a = texto "a"
b = texto "b"
c = texto "c"

testsEj7 :: Test
testsEj7 =
  test
    [ mostrar (intercalar (texto ", ") []) ~?= "",
      mostrar (intercalar (texto ", ") [a, b, c]) ~?= "a, b, c",
      mostrar (entreLlaves []) ~?= "{ }",
      mostrar (entreLlaves [a, b, c]) ~?= "{\n  a,\n  b,\n  c\n}",

      -- Casos propios:
      mostrar (intercalar (texto ": ") [a, linea, b, linea, c]) ~?= "a: \n: b: \n: c", -- intercalar con lineas
      mostrar (intercalar (texto "! ") [a, linea, vacio, b, linea, vacio, c]) ~?= "a! \n! ! b! \n! ! c", -- intercalar con lineas y vacios
      mostrar (entreLlaves [a, linea, b, linea, c]) ~?= "{\n  a,\n  \n  ,\n  b,\n  \n  ,\n  c\n}",-- mostrar entre llaves con lineas
      mostrar (entreLlaves [a, linea, vacio, b, linea, vacio, c]) ~?= "{\n  a,\n  \n  ,\n  ,\n  b,\n  \n  ,\n  ,\n  c\n}"-- mostrar entre llaves con lineas y vacios
    ]

testsEj8 :: Test
testsEj8 =
  test
    [ mostrar (aplanar (a <+> linea <+> b <+> linea <+> c)) ~?= "a b c",

    -- Casos propios:
      mostrar (aplanar (a <+> b <+> c)) ~?= "abc", -- aplanar solo texto
      mostrar (aplanar (a <+> indentar 2 (linea <+> b))) ~?= "a b", -- aplanar lineas e indentados 
      mostrar (aplanar ((linea <+> a <+> linea) <+> indentar 2 ( linea <+> b <+> linea <+> c) <+> linea)) ~?= " a  b c " -- aplanar múltiples lineas e indentados 
    ]

testsEj9 :: Test
testsEj9 =
  test
    [ mostrar (pponADoc pericles) ~?= "{ \"nombre\": \"Pericles\", \"edad\": 30 }",
      mostrar (pponADoc addams) ~?= "{\n  \"0\": { \"nombre\": \"Pericles\", \"edad\": 30 },\n  \"1\": { \"nombre\": \"Merlina\", \"edad\": 24 }\n}",
      mostrar (pponADoc familias) ~?= "{\n  \"Addams\": {\n    \"0\": { \"nombre\": \"Pericles\", \"edad\": 30 },\n    \"1\": { \"nombre\": \"Merlina\", \"edad\": 24 }\n  }\n}",

      -- Casos propios:
      mostrar (pponADoc merlina) ~?= "{ \"nombre\": \"Merlina\", \"edad\": 24 }"

    ]
