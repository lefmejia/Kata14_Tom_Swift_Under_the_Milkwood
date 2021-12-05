{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Trigramas where
import System.Environment
import System.IO
import System.Directory
import Control.Monad
import Data.List
import Data.Tuple
import Data.Map (toList, fromListWith, findWithDefault)

limpiarTexto :: String -> String
limpiarTexto xs = [ x | x <- xs, x `notElem` ",.?!-:;\"\915\199\214\915\199\170\163" ]

main :: IO ()
main = do
    arg <- getArgs
    n <- getFileName arg
    print $ crearTexto $ crearListaDeTuplas 0 $ eliminarRepetidos $ trigrama (map listaATupla (ngram 2 n))

getFileName :: [String] -> IO [String]
getFileName [nombre] = leerArchivo nombre

--Lee el archivo, elimina los signos de puntuacion y crea una lista con todas las palabras
leerArchivo :: String -> IO [String]
leerArchivo nombre = do
    content <- readFile nombre
    return $ words $ unwords (map limpiarTexto (lines content))

--Crea una lista de listas de tamaño n
ngram :: Int -> [a] -> [[a]]
ngram n xs
  | n <= length xs = take n xs : ngram n (drop 1 xs)
  | otherwise = []

--Convierte una lista a una tupla
listaATupla :: [String] -> (String, String)
listaATupla [x, xx] = (x, xx)

eliminarRepetidos :: (Eq a) => [a] -> [a]
eliminarRepetidos (x:xs) = x : eliminarRepetidos (filter (/= x) xs)
eliminarRepetidos [] = []

-- Crea una lista de tuplas de tuplas y listas [(x, y)], x es una tupla que contiene una combinacion de palabras, y es una lista de posibles palabras que pueden seguir al par
trigrama :: (Eq a1, Eq a2) => [(a1, a2)] -> [((a1, a2), [a2])]
trigrama xs = [(x, posibilidades x xs) | x <- xs]

--Busca recursivamente todas las palabras que pueden seguir a una combinacion de par de palabras
posibilidades :: (Eq a1, Eq a2) => (a1, a2) -> [(a1, a2)] -> [a2]
posibilidades n (x:xs)
        | null xs = []
        | n == x = snd (head xs): posibilidades n xs
        | otherwise = posibilidades n xs

--Crea una combinacion de tuplas para generar el texto, los primeros 2 elementos se pueden tomar directamente pero los deben ser creados recursivamente
crearListaDeTuplas :: Eq b => Int -> [((b, b), [b])] -> [(b, b)]
crearListaDeTuplas n xs = fst (xs!!n) : crearTupla 0 (xs!!n) : tuplaRecursiva (crearTupla 0 (xs!!n)) xs

-- Continuacion de la creacion de lista de tuplas, agrega tuplas hasta que tuplaDeMap retorna una lista vacia
tuplaRecursiva :: Eq b => (b, b) -> [((b, b), [b])] -> [(b, b)]
tuplaRecursiva t xs 
    | null (tuplaDeMap t xs) = []
    | otherwise = head (tuplaDeMap t xs) : tuplaRecursiva (head (tuplaDeMap t xs)) xs

--Busca una tupla especifica en la lista y retorna una lista con una unica tupla
tuplaDeMap :: (Eq a1, Eq a2) => (a1, a2) -> [((a1, a2), [b])] -> [(a2, b)]
tuplaDeMap t xs = [crearTupla (length (snd x) - 1) x | x <- xs, t == fst x && (length (snd x) - 1) >= 0]

-- Crea una tupla con el segundo elemento del par y una de las posibles palabras
crearTupla :: Int -> ((a1, a2), [b]) -> (a2, b)
crearTupla n x = (snd (fst x), (snd x)!!n)

--Se toma la lista de tuplas y se crea el texto
crearTexto :: [(String, String)] -> String
crearTexto (x:xs) = unwords (fst x : snd x : [ snd n | n <- xs])

esDigito:: Char -> Bool
esDigito '9' = True
esDigito '8' = True
esDigito '7' = True
esDigito '6' = True
esDigito '5' = True
esDigito '4' = True
esDigito '3' = True
esDigito '2' = True
esDigito '1' = True
esDigito '0' = True
esDigito _ = False

esNumero:: [Char] -> Bool
esNumero (x:xs) = esDigito x && (esNumero xs)
esNumero [] = True
