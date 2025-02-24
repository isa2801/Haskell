import Data.Time.Clock
import Data.List
import System.IO
import Control.Exception
import Control.DeepSeq (deepseq)

-- Definición del tipo de datos para representar la información de un Estudiante
data Estudiante = Estudiante {
    idEstudiante :: String,
    entrada :: UTCTime,
    salida :: Maybe UTCTime  -- Usamos Maybe para representar que el Estudiante aún está en el universidad o ya salió
} deriving (Show, Read)

-- Función para registrar la entrada de un Estudiante al universidad
registrarEntrada :: String -> UTCTime -> [Estudiante] -> [Estudiante]
registrarEntrada id tiempo universidad =
    Estudiante id tiempo Nothing : universidad

-- Función para registrar la salida de un Estudiante del universidad
registrarSalida :: String -> UTCTime -> [Estudiante] -> [Estudiante]
registrarSalida id tiempo universidad =
    map (\v -> if id == idEstudiante v then v { salida = Just tiempo } else v) universidad

-- Función para buscar un Estudiante por su id en el universidad
buscarEstudiante :: String -> [Estudiante] -> Maybe Estudiante
buscarEstudiante id universidad =
    find (\v -> id == idEstudiante v && isNothing (salida v)) universidad
    where
        isNothing Nothing = True
        isNothing _       = False

-- Función para calcular el tiempo que un Estudiante permaneció en el universidad
tiempoEnuniversidad :: Estudiante -> IO NominalDiffTime
tiempoEnuniversidad estudiante = do
    tiempoActual <- getCurrentTime
    return $ diffUTCTime tiempoActual (entrada estudiante)

-- Función para guardar la información de los Estudiantes en un archivo de texto
guardaruniversidad :: [Estudiante] -> IO ()
guardaruniversidad universidad = do
    withFile "universidad.txt" WriteMode $ \h -> do
        hPutStr h (unlines (map mostrarEstudiante universidad))
    putStrLn "cambio guardado en el archivo universidad.txt."

-- Función para cargar la información de los Estudiantes desde un archivo de texto
cargaruniversidad :: IO [Estudiante]
cargaruniversidad = do
    contenido <- withFile "universidad.txt" ReadMode $ \h -> do
        contenido <- hGetContents h
        contenido `deepseq` return contenido
    let lineas = lines contenido
    return (map leerEstudiante lineas)
    where
        leerEstudiante linea = read linea :: Estudiante

-- Función para mostrar la información de un Estudiante como cadena de texto
mostrarEstudiante :: Estudiante -> String
mostrarEstudiante (Estudiante id entrada salida) =
    "Estudiante {id = \"" ++ id ++ "\", entrada = " ++ show entrada ++ ", salida = " ++ maybe "Nothing" show salida ++ "}"

-- Función para listar los Estudiantes en el universidad
listarEstudiantes :: [Estudiante] -> IO ()
listarEstudiantes [] = putStrLn "No hay Estudiantes en la universidad."
listarEstudiantes estudiantes = do
    putStrLn "Estudiantes en la universidad:"
    mapM_ (putStrLn . mostrarEstudiante) estudiantes

-- Función principal del programa
main :: IO ()
main = do
    -- Cargar el universidad desde el archivo de texto
    universidad <- cargaruniversidad
    putStrLn "¡Bienvenido al Sistema de Gestión de universidad!"

    -- Ciclo principal del programa
    cicloPrincipal universidad

-- Función para el ciclo principal del programa
cicloPrincipal :: [Estudiante] -> IO ()
cicloPrincipal universidad = do
    putStrLn "\nSeleccione una opción:"
    putStrLn "1. Registrar entrada del estudiante"
    putStrLn "2. Registrar salida del estudiante"
    putStrLn "3. Buscar estudiante por id"
    putStrLn "4. Lista de Estudiantes"
    putStrLn "5. Salir"

    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese la id del Estudiante:"
            idEstudiante <- getLine
            tiempoActual <- getCurrentTime
            let universidadActualizado = registrarEntrada idEstudiante tiempoActual universidad
            putStrLn $ "Estudiante con id " ++ idEstudiante ++ " ingresado al universidad."
            guardaruniversidad universidadActualizado
            cicloPrincipal universidadActualizado

        "2" -> do
            putStrLn "Ingrese la id del Estudiante a salir:"
            idEstudiante <- getLine
            tiempoActual <- getCurrentTime
            let universidadActualizado = registrarSalida idEstudiante tiempoActual universidad
            putStrLn $ "Estudiante con id " ++ idEstudiante ++ " salido del universidad."
            guardaruniversidad universidadActualizado
            cicloPrincipal universidadActualizado

        "3" -> do
            putStrLn "Ingrese la id del Estudiante a buscar:"
            idEstudiante <- getLine
            case buscarEstudiante idEstudiante universidad of
                Just estudiante -> do
                    tiempoTotal <- tiempoEnuniversidad estudiante
                    putStrLn $ "El Estudiante con id " ++ idEstudiante ++ " se encuentra en el universidad."
                    putStrLn $ "Tiempo en universidad: " ++ show tiempoTotal ++ " segundos."
                Nothing -> putStrLn "Estudiante no encontrado en el universidad."
            cicloPrincipal universidad

        "4" -> do
            listarEstudiantes universidad
            cicloPrincipal universidad

        "5" -> putStrLn "¡Hasta luego!"

        _ -> do
            putStrLn "Opción no válida. Por favor, seleccione una opción válida."
            cicloPrincipal universidad

