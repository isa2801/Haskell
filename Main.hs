import Data.Time.Clock --Proporciona tipos y funciones para trabajar con tiempo (UTCTime, NominalDiffTime)
import Data.List --Contiene funciones para manipular listas, como `find`
import System.IO --Permite operaciones de entrada/salida como lectura/escritura de archivos
import Control.Exception--Manejo de excepciones
import Control.DeepSeq (deepseq) --Proporciona la función `deepseq` para forzar la evaluación completa de estructuras de datos-}

-- Definición del tipo de datos para representar la información de un estudiante
data Estudiante = Estudiante {
    id :: String,
    entrada :: UTCTime,
    salida :: Maybe UTCTime  -- Usamos Maybe para representar que el estudiante aún está en la universidad o ya salió
} deriving (Show, Read) --Permite convertir automáticamente entre Estudiante y String

-- Función para registrar la entrada de un estudiante a la universidad
registrarEntrada :: String -> UTCTime -> [Estudiante] -> [Estudiante]
registrarEntrada idEstudiante tiempo universidad =
    Estudiante idEstudiante tiempo Nothing : universidad

-- Función para registrar la salida de un estudiante de la universidad
registrarSalida :: String -> UTCTime -> [Estudiante] -> [Estudiante]
registrarSalida idEstudiante tiempo universidad =
    map (\e -> if idEstudiante == Main.id e then e { salida = Just tiempo } else e) universidad {-Usa `Main.id` para evitar conflictos 
                                                                                                con la función `id` de Haskell-}
-- Función para buscar un estudiante por su ID en la universidad
buscarEstudiante :: String -> [Estudiante] -> Maybe Estudiante
buscarEstudiante idEstudiante universidad =
    find (\e -> idEstudiante == Main.id e && isNothing (salida e)) universidad
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
    putStrLn "Registro guardado en el archivo universidad.txt."

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
    "Estudiante {id = \"" ++ id ++ "\", entrada = " ++ show entrada ++ ", salida = " ++ maybe "Nothing" show salida ++ "}"

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
    putStrLn "¡Bienvenido al Sistema de Gestión de Registro de Estudiantes!"

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
            idEstudiante <- getLine
            tiempoActual <- getCurrentTime
            let universidadActualizada = registrarEntrada idEstudiante tiempoActual universidad
            putStrLn $ "Estudiante con ID " ++ idEstudiante ++ " registrado en la universidad."
            guardarUniversidad universidadActualizada
            cicloPrincipal universidadActualizada

        "2" -> do
            putStrLn "Ingrese el ID del estudiante que sale:"
            idEstudiante <- getLine
            tiempoActual <- getCurrentTime
            let universidadActualizada = registrarSalida idEstudiante tiempoActual universidad
            putStrLn $ "Estudiante con ID " ++ idEstudiante ++ " ha salido de la universidad."
            guardarUniversidad universidadActualizada
            cicloPrincipal universidadActualizada

        "3" -> do
            putStrLn "Ingrese el ID del estudiante a buscar:"
            idEstudiante <- getLine
            case buscarEstudiante idEstudiante universidad of
                Just estudiante -> do
                    tiempoTotal <- tiempoEnUniversidad estudiante
                    putStrLn $ "El estudiante con ID " ++ idEstudiante ++ " se encuentra en la universidad."
                    putStrLn $ "Tiempo en la universidad: " ++ show tiempoTotal ++ " segundos."
                Nothing -> putStrLn "Estudiante no encontrado en la universidad."
            cicloPrincipal universidad

        "4" -> do
            listarEstudiantes universidad
            cicloPrincipal universidad

        "5" -> putStrLn "¡Hasta luego!"

        _ -> do
            putStrLn "Opción no válida. Por favor, seleccione una opción válida."
            cicloPrincipal universidad