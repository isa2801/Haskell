import Data.Time.Clock
import Data.List
import System.IO
import Control.Exception
import Control.DeepSeq (deepseq)

-- Definición del tipo de datos para representar la información de un estudiante
data Estudiante = Estudiante {
    idEstudiante :: String,
    entrada :: UTCTime,
    salida :: Maybe UTCTime  -- Usamos Maybe para representar que el estudiante aún está en la universidad o ya salió
} deriving (Show, Read)

-- Función para registrar la entrada de un estudiante a la universidad
registrarEntrada :: String -> UTCTime -> [Estudiante] -> [Estudiante]
registrarEntrada id tiempo universidad =
    Estudiante id tiempo Nothing : universidad

-- Función para registrar la salida de un estudiante de la universidad
registrarSalida :: String -> UTCTime -> [Estudiante] -> [Estudiante]
registrarSalida id tiempo universidad =
    map (\e -> if id == idEstudiante e then e { salida = Just tiempo } else e) universidad

-- Función para buscar un estudiante por su ID en la universidad
buscarEstudiante :: String -> [Estudiante] -> Maybe Estudiante
buscarEstudiante id universidad =
    find (\e -> id == idEstudiante e && isNothing (salida e)) universidad
    where
        isNothing Nothing = True
        isNothing _       = False

-- Función para calcular el tiempo que un estudiante permaneció en la universidad
tiempoEnUniversidad :: Estudiante -> IO NominalDiffTime
tiempoEnUniversidad estudiante = do
    tiempoActual <- getCurrentTime
    return $ diffUTCTime tiempoActual (entrada estudiante)

-- Función para guardar la información de los estudiantes en un archivo de texto
guardarUniversidad :: [Estudiante] -> IO ()
guardarUniversidad universidad = do
    withFile "universidad.txt" WriteMode $ \h -> do
        hPutStr h (unlines (map mostrarEstudiante universidad))
    putStrLn "Universidad guardada en el archivo universidad.txt."

-- Función para cargar la información de los estudiantes desde un archivo de texto
cargarUniversidad :: IO [Estudiante]
cargarUniversidad = do
    contenido <- withFile "universidad.txt" ReadMode $ \h -> do
        contenido <- hGetContents h
        contenido `deepseq` return contenido
    let lineas = lines contenido
    return (map leerEstudiante lineas)
    where
        leerEstudiante linea = read linea :: Estudiante

-- Función para mostrar la información de un estudiante como cadena de texto
mostrarEstudiante :: Estudiante -> String
mostrarEstudiante (Estudiante id entrada salida) =
    "Estudiante {idEstudiante = \"" ++ id ++ "\", entrada = " ++ show entrada ++ ", salida = " ++ maybe "Nothing" show salida ++ "}"

-- Función para listar los estudiantes en la universidad
listarEstudiantes :: [Estudiante] -> IO ()
listarEstudiantes [] = putStrLn "No hay estudiantes en la universidad."
listarEstudiantes estudiantes = do
    putStrLn "Estudiantes en la universidad:"
    mapM_ (putStrLn . mostrarEstudiante) estudiantes

-- Función principal del programa
main :: IO ()
main = do
    -- Cargar la universidad desde el archivo de texto
    universidad <- cargarUniversidad
    putStrLn "¡Bienvenido al Sistema de Gestión de Universidad!"

    -- Ciclo principal del programa
    cicloPrincipal universidad

-- Función para el ciclo principal del programa
cicloPrincipal :: [Estudiante] -> IO ()
cicloPrincipal universidad = do
    putStrLn "\nSeleccione una opción:"
    putStrLn "1. Registrar entrada de estudiante"
    putStrLn "2. Registrar salida de estudiante"
    putStrLn "3. Buscar estudiante por ID"
    putStrLn "4. Listar estudiantes"
    putStrLn "5. Salir"

    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese el ID del estudiante:"
            id <- getLine
            tiempoActual <- getCurrentTime
            let universidadActualizada = registrarEntrada id tiempoActual universidad
            putStrLn $ "Estudiante con ID " ++ id ++ " ingresado a la universidad."
            guardarUniversidad universidadActualizada
            cicloPrincipal universidadActualizada

        "2" -> do
            putStrLn "Ingrese el ID del estudiante a salir:"
            id <- getLine
            tiempoActual <- getCurrentTime
            let universidadActualizada = registrarSalida id tiempoActual universidad
            putStrLn $ "Estudiante con ID " ++ id ++ " salido de la universidad."
            guardarUniversidad universidadActualizada
            cicloPrincipal universidadActualizada

        "3" -> do
            putStrLn "Ingrese el ID del estudiante a buscar:"
            id <- getLine
            case buscarEstudiante id universidad of
                Just estudiante -> do
                    tiempoTotal <- tiempoEnUniversidad estudiante
                    putStrLn $ "El estudiante con ID " ++ id ++ " se encuentra en la universidad."
                    putStrLn $ "Tiempo en universidad: " ++ show tiempoTotal ++ " segundos."
                Nothing -> putStrLn "Estudiante no encontrado en la universidad."
            cicloPrincipal universidad

        "4" -> do
            listarEstudiantes universidad
            cicloPrincipal universidad

        "5" -> putStrLn "¡Hasta luego!"

        _ -> do
            putStrLn "Opción no válida. Por favor, seleccione una opción válida."
            cicloPrincipal universidad