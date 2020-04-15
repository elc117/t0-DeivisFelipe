main = do 
  putStrLn "Vamos testar as funcoes abaixo!"
  
square :: Int -> Int
square x = x^2

squareAll :: [Int] -> [Int]
squareAll lis = map square lis 

ficaemcasa :: String -> String
ficaemcasa fulano = fulano ++ ", fica em casa!"

quarentena :: [String] -> [String]
quarentena pessoas = map ficaemcasa pessoas

podesair :: String -> Bool
podesair profissao = profissao == "Medico"

idadeadulta :: Int -> Bool
idadeadulta idade = idade >= 18

verificaFebre :: [Float] -> [Float]
verificaFebre n = filter (>37.8) n

ultimo :: [Int] -> [Int]
ultimo list = map (\a -> a^2 + a + 2) list