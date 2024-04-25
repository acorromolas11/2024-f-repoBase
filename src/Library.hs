module Library where
import PdePreludat
import GHC.Base (List)

-- Tenemos información sobre artistas y sus canciones:
-- Se puede agregar mas artistas :)

--1
-- Saber la calificacion de un cancion, que equivale a la cantidad de letras 
--(sin espacios, números ni caracteres especiales) de la canción, más 10.

--2    
-- Averiguar si es exitoso un artista, lo que sucede cuando la suma de las calificaciones buenas
--de las canciones de un artista es mayor a 50 (son buenas las que tienen calificacion mayor a 20)

--3
-- Obtener todos los artistas exitosos, a partir de un conjunto de artistas:

--4
-- Hacer todo lo anterior en una función definida en una sola línea, sin definir funciones auxiliares


type Cancion = String

data Artista = UnArtista {
    nombre :: String,
    canciones :: [Cancion]
} deriving Show

cerati :: Artista
cerati = UnArtista "Gustavo Cerati" ["Crimen", "Sudestada", "Puente"]

fitito :: Artista
fitito = UnArtista "Fitito Paez" ["11 y 6", "El amor despues del amor", "Mariposa Tecknicolor"]

calamardo :: Artista
calamardo = UnArtista "Andres Calamardo" ["Flaca", "Sin Documentos", "Tuyo siempre"]

paty :: Artista
paty = UnArtista "Taylor Paty" ["Shake It Off", "Lover"]

agus :: Artista
agus = UnArtista "Agus Corro" ["Uno","Dos"]

desconocido :: Artista
desconocido = UnArtista "Desconocido" ["A"]

--Respuestas

grupoArtistas = [agus,calamardo,paty,desconocido,fitito,cerati]

--1

calificacionCancion :: Cancion -> Number
calificacionCancion = (+ 10) . cantidadLetras

cantidadLetras :: Cancion -> Number
cantidadLetras = length . filter validarLetra

validarLetra :: Char -> Bool
validarLetra letra = elem letra ['a'..'z'] ||  elem letra ['A'..'Z']

--2

sumarCalificaciones :: [Number] -> Number
sumarCalificaciones = sum

calificarCanciones :: [Cancion] -> [Number]
calificarCanciones = map calificacionCancion   

esExitoso :: [Cancion] -> Bool
esExitoso = (>50).sumarCalificaciones.calificarCanciones

esBueno :: [Cancion] -> Bool
esBueno = (>20).sumarCalificaciones.calificarCanciones

tipoArtista :: Artista -> String
tipoArtista artista
    |esBueno (canciones artista) = "Es Bueno"
    |esExitoso (canciones artista) = "Es Exitoso"
    |otherwise = "Es malo"

--3

artistaExitoso :: Artista -> Bool
artistaExitoso artista = esExitoso (canciones artista) 

buscarArtistasExitosos :: [Artista] -> [Artista]
buscarArtistasExitosos = filter artistaExitoso

nombrarArtistasExitosos :: [Artista] -> [String]
nombrarArtistasExitosos = map nombre 

artistasExitosos :: [Artista] -> [String]
artistasExitosos = nombrarArtistasExitosos.buscarArtistasExitosos

--4

lambda :: [Artista] -> [Artista]
lambda = filter (\artista -> sum (map (\cancion -> length (filter (\letra -> letra `elem` ['a'..'z'] || letra `elem` ['A'..'Z']) cancion) + 10) (canciones artista)) > 50)
