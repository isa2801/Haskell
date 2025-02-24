import Data.Time.Clock
import Data.List
import System.IO
import Control.Exception
import Control.DeepSeq (deepseq)

-- Definición del tipo de datos para representar la información de un estudiante
data Estudiante = Estudiante {
    idEstudiante :: String,
    entrada :: UTCTime,
    salida :: Maybe UTCTime  -- Usamos Maybe para representar que el estudiante aún está en el universidad o ya salió
} deriving (Show, Read)

-- Función para registrar la entrada de un estudiante al universidad
registrarEntrada :: String -> UTCTime -> [Estudiante] -> [Estudiante]
registrarEntrada idEstudiante tiempo universidad =
    Estudiante idEstudiante tiempo Nothing : universidad

-- Función para registrar la salida de un estudiante del universidad
registrarSalida :: String -> UTCTime -> [Estudiante] -> [Estudiante]
registrarSalida id tiempo universidad =
    map (\e -> if id == idEstudiante e then e { salida = Just tiempo } else e) universidad

-- Función para buscar un estudiante por su ID
buscarEstudiante :: String -> [Estudiante] -> Maybe Estudiante
buscarEstudiante id universidad =
    find (\e -> id == idEstudiante e && isNothing (salida e)) universidad
    where
        isNothing Nothing = True
        isNothing _       = False

-- Función para calcular el tiempo que un estudiante permaneció en el universidad
tiempoEnuniversidad :: Estudiante -> IO NominalDiffTime
tiempoEnuniversidad estudiante = do
    tiempoActual <- getCurrentTime
    return $ diffUTCTime tiempoActual (entrada estudiante)

-- Función para guardar la información de los estudiantes en un archivo de texto
guardaruniversidad :: [Estudiante] -> IO ()
guardaruniversidad universidad = do
    withFile "universidad.txt" WriteMode $ \h -> do
        hPutStr h (unlines (map mostrarEstudiante universidad))
    putStrLn "universidad guardado en el archivo universidad.txt."

-- Función para cargar la información de los estudiantes desde un archivo de texto
cargaruniversidad :: IO [Estudiante]
cargaruniversidad = do
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
    "Estudiante {id = \"" ++ id ++ "\", entrada = " ++ show entrada ++ ", salida = " ++ maybe "Nothing" show salida ++ "}"

-- Función para listar los estudiantes en el universidad
listarEstudiantes :: [Estudiante] -> IO ()
listarEstudiantes [] = putStrLn "No hay estudiantes en el universidad."
listarEstudiantes estudiantes = do
    putStrLn "Estudiantes en el universidad:"
    mapM_ (putStrLn . mostrarEstudiante) estudiantes

-- Función principal del programa
main :: IO ()
main = do
    -- Cargar el universidad desde el archivo de texto
    universidad <- cargaruniversidad
    putStrLn "¡Bienvenido al Sistema de Control de Acceso de Estudiantes!"

    -- Ciclo principal del programa
    cicloPrincipal universidad

-- Función para el ciclo principal del programa
cicloPrincipal :: [Estudiante] -> IO ()
cicloPrincipal universidad = do
    putStrLn "\nSeleccione una opción:"
    putStrLn "1. Registrar entrada de estudiante"
    putStrLn "2. Registrar salida de estudiante"
    putStrLn "3. Buscar estudiante por ID"
    putStrLn "4. Lista de estudiantes"
    putStrLn "5. Salir"

    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese el ID del estudiante:"
            idEstudiante <- getLine
            tiempoActual <- getCurrentTime
            let universidadActualizado = registrarEntrada idEstudiante tiempoActual universidad
            putStrLn $ "Estudiante con ID " ++ idEstudiante ++ " registrado."
            guardaruniversidad universidadActualizado
            cicloPrincipal universidadActualizado

        "2" -> do
            putStrLn "Ingrese el ID del estudiante que sale:"
            idEstudiante <- getLine
            tiempoActual <- getCurrentTime
            let universidadActualizado = registrarSalida idEstudiante tiempoActual universidad
            putStrLn $ "Estudiante con ID " ++ idEstudiante ++ " ha salido."
            guardaruniversidad universidadActualizado
            cicloPrincipal universidadActualizado

        "3" -> do
            putStrLn "Ingrese el ID del estudiante a buscar:"
            idEstudiante <- getLine
            case buscarEstudiante idEstudiante universidad of
                Just estudiante -> do
                    tiempoTotal <- tiempoEnuniversidad estudiante
                    putStrLn $ "El estudiante con ID " ++ idEstudiante ++ " se encuentra en el universidad."
                    putStrLn $ "Tiempo en el universidad: " ++ show tiempoTotal ++ " segundos."
                Nothing -> putStrLn "Estudiante no encontrado en el universidad."
            cicloPrincipal universidad

        "4" -> do
            listarEstudiantes universidad
            cicloPrincipal universidad

        "5" -> putStrLn "¡Hasta luego!"

        _ -> do
            putStrLn "Opción no válida. Por favor, seleccione una opción válida."
            cicloPrincipal universidad