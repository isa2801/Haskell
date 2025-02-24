-- Función principal del programa
main :: IO ()
main = do
    -- Cargar el universidad desde el archivo de texto
    universidad <- cargaruniversidad
    putStrLn "¡Bienvenido al Sistema de Gestión de universidad!"

    -- Ciclo principal del programa
    cicloPrincipal universidad