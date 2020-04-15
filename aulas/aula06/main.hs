main = do 
  putStrLn "Vamos testar as funcoes abaixo!"

add10toall :: [Int] -> [Int]
add10toall n = [x+10 | x <- n] 

multN :: Int -> [Int] -> [Int]
multN n b = [n*x | x <- b]

applyExpr :: [Int] -> [Int]
applyExpr n = [3*x+2 | x <- n]

addSuffix :: String -> [String] -> [String]
addSuffix n b = [ x ++ n | x <- b]

selectgt5 :: [Int] -> [Int]
selectgt5 n = [ x | x <- n, x > 5 ]

sumOdds :: [Int] -> Int
sumOdds n = sum [ x | x <- n, (x `mod` 2) == 1]

selectExpr :: [Int] -> [Int]
selectExpr n = [ x | x <-  n, x >= 20, x <= 50, (x `mod` 2) == 0 ]

countShorts :: [String] -> Int
countShorts n = sum [ 1 | x <- n, (length x) < 5]

calcExpr :: [Float] -> [Float]
calcExpr n = [x^2/2 | x <- n, x^2/2 > 10]

trSpaces :: String -> String
trSpaces n = [if x == ' ' then '-' else x | x <- n]

selectSnd :: [(Int,Int)] -> [Int]
selectSnd n = [ x | (_,x) <- n]

dotProd :: [Int] -> [Int] -> Int
dotProd n b = sum [ x * y | (x,y) <- zip n b]

genRects :: Int -> (Int,Int) -> [(Float,Float,Float,Float)]
genRects n (o,i) = [((fromIntegral x)*5.5+(fromIntegral o),(fromIntegral i),5.5,5.5) | x <- [0..n-1]]