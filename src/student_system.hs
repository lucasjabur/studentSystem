type Student = (Int, String, String, Int)

initialDb :: [Student]
initialDb = []

addStdnt :: Student -> [Student] -> [Student]
addStdnt student db = student : db

printDb :: [Student] -> IO ()
printDb [] = putStr "\n"
printDb (head : tail) = do
    putStrLn ("ID do estudante: " ++ show studentID ++ ", Nome: " ++ firstName ++ " " ++ lastName ++ ", Idade: " ++ show age)
    printDb tail
    where (studentID, firstName, lastName, age) = head

searchID :: Int -> [Student] -> [Student]
searchID id db = filter (\(studentID, _, _, _) -> studentID == id) db

attStdnt :: Int -> Student -> [Student] -> [Student]
attStdnt id student db = do 
    map (\(studentID, firstName, lastName, age) -> 
        if studentID == id then student
        else (studentID, firstName, lastName, age)) db

menu :: [Student] -> IO()
menu db = do
    putStrLn "\n[1] - Adicionar um estudante"
    putStrLn "[2] - Recuperar um estudante pela ID"
    putStrLn "[3] - Atualizar as informações de um estudante"
    putStrLn "[4] - Sair\n"

    putStr "Escolha uma opcao: "

    option <- getLine

    case option of
        "1" -> do
            putStrLn "\nPreencha os dados do estudante"
            putStr "ID do estudante: "
            studentID <- readLn
            putStr "Primeiro nome: "
            firstName <- getLine
            putStr "Ultimo nome: "
            lastName <- getLine
            putStr "Idade: "
            age <- readLn
            putStr "\n"

            let newStdnt = (studentID, firstName, lastName, age)
            let newDb = addStdnt newStdnt db

            printDb newDb
            menu newDb

        "2" -> do
            putStrLn "\nInsira a ID do estudante desejado: "
            putStr "ID do estudante: "
            studentID <- readLn
            putStr "\n"

            let foundIDs = searchID studentID db
            if foundIDs == [] then do 
                putStrLn "ID não encontrada!" 
                menu db
            else do
                putStrLn ("ID encontrada: " ++ show foundIDs)
                menu db

        "3" -> do
            putStrLn "\nInsira a ID do estudante que deseja atualizar as informações: "
            putStr "ID do estudante: "
            studentID <- readLn

            putStrLn "\nInsira as novas informações deste estudante: "
            putStr "Primeiro nome atualizado: "
            newFstName <- getLine
            putStr "Segundo nome atualizado: "
            newSndName <- getLine
            putStr "Idade atualizada: "
            newAge <- readLn
            putStr "\n"

            let newStdnt = (studentID, newFstName, newSndName, newAge)
            let newDb = attStdnt studentID newStdnt db

            printDb newDb
            menu newDb
        
        "4" -> putStrLn "\nEncerrando o programa...\n"

        _ -> do
            putStrLn "Opcao invalida!\n"
            menu db

main :: IO ()
main = do
    menu initialDb
