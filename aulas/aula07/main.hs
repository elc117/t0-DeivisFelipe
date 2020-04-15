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



ends :: [Int] -> [Int]
ends x = head x : last x : []

ends2 :: [Int] -> [Int]
ends2 x = [head x , last x]

deduzame :: [Integer] -> [Integer]
deduzame [] = []
deduzame lst = (2 * head lst) : deduzame (tail lst)

deduzame2 :: [Integer] -> [Integer]
deduzame2 [] = []
deduzame2 lst = if (head lst) > 2
  then head lst : deduzame2 (tail lst) 
  else deduzame2 (tail lst)

deduzame12 :: [Integer] -> [Integer]
deduzame12 [] = []
deduzame12 (x:xs) = 2 * x : deduzame xs

deduzame22 :: [Integer] -> [Integer]
deduzame22 [] = []
deduzame22 (x:xs) = if x > 2
  then x : deduzame2 xs 
  else deduzame2 xs

geraTabela :: Int -> [(Int,Int)]
geraTabela 0 = []
geraTabela x = (x,x^2) : geraTabela (x-1)

contido :: Char -> String -> Bool
contido x "" = False
contido x (z:zs) = if x == z
  then True
  else contido x zs

translate :: [(Float,Float)] -> [(Float,Float)]
translate [] = []
translate ((xa,xb):xs) = (xa+2,xb+2) : translate xs

countLongs :: [String] -> Int
countLongs [] = 0;
countLongs (x:xs) = if (length x) > 5
  then 1 + countLongs xs
  else countLongs xs

onlyLongs  :: [String] -> [String]
onlyLongs  [] = []
onlyLongs  (x:xs) = if (length x) > 5
  then x : onlyLongs xs
  else onlyLongs xs