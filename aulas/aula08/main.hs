main = do 
  putStrLn "Vamos testar as funcoes abaixo!"

isBin :: String -> Bool
isBin [] = False
isBin (x:xs)
  | (x == '1' || x == '0') && (xs == []) = True
  | (x == '1' || x == '0') && (xs /= []) = isBin xs
  | otherwise = False

isBin2 :: String -> Bool
isBin2 [] = False
isBin2 x = length (filter (\y -> notElem y "10") x) == 0

bin2dec :: [Int] -> Int
bin2dec [] = undefined
bin2dec bits = auxBin2Dec bits ((length bits)-1)

auxBin2Dec :: [Int] -> Int -> Int
auxBin2Dec [] (-1) = 0 
auxBin2Dec (x:xs) z = ((2^z)*x) + auxBin2Dec xs ((length xs)-1)

bin2dec2 :: [Int] -> Int
bin2dec2 [] = undefined
bin2dec2 y = sum [ 2^(x) | x <- [length y-1..0] , y !! x == 1]

dec2bin :: Int -> [Int]
dec2bin 0 = [0]
dec2bin 1 = [1]
dec2bin x = dec2bin (x `div` 2) ++ [x `mod` 2]

isHex :: String -> Bool
isHex [] = False
isHex x = length (filter (\y -> notElem y "0123456789abcdefABCDEF") x) == 0

dec2hex :: Int -> String
dec2hex 0 = "0"
dec2hex 1 = "1"
dec2hex 2 = "2"
dec2hex 3 = "3"
dec2hex 4 = "4"
dec2hex 5 = "5"
dec2hex 6 = "6"
dec2hex 7 = "7"
dec2hex 8 = "8"
dec2hex 9 = "9"
dec2hex 10 = "A"
dec2hex 11 = "B"
dec2hex 12 = "C"
dec2hex 13 = "D"
dec2hex 14 = "E"
dec2hex 15 = "F"
dec2hex x = dec2hex (x `div` 16) ++ dec2hex (x `mod` 16)
