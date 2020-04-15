import Data.List

main = do 
  putStrLn "Vamos testar as funcoes abaixo!"

anoIdade :: Int -> (Int, Int)
anoIdade n = (n,2020-n)

selectName :: (String,Int,Int) -> String
selectName (n,_,_) = n 

allNames :: [(String,Int,Int)] -> [String]
allNames n = map selectName n

allNames2 :: [(String,Int,Int)] -> [String]
allNames2 n = map (\(n,_,_) -> n) n

distance :: (Float, Float) -> (Float, Float) -> Float
distance (x1,y1) (x2,y2) = sqrt (((x2 -x1)^2) + ((y2 -y1)^2))

links :: [(String,String)] -> [String]
links n = map linkAux n

linkAux :: (String,String) -> String
linkAux (n,d) = ("<a href=\""++d++"\">"++n++"</a>") 

rect :: [(Float, Float, Float, Float)] -> [Float]
rect n = map rextAux n

rextAux :: (Float, Float, Float, Float) -> Float
rextAux (x,y,w,h) = w*h

rect2 :: [(Float, Float, Float, Float)] -> [Float]
rect2 n = map (\(x,y,w,h) -> w*h) n

testSort1 :: [Int] -> [Int]
testSort1 n = sort n

testSort2 :: [String] -> [String]
testSort2 n = sort n

testSort12 :: [String] -> [String]
testSort12 dat = sortBy (flip compare) dat

testSort123 :: [String] -> [String]
testSort123 dat = sortBy compare dat

testZip :: [Int] -> [Int] -> [(Int,Int)]
testZip n b = filter (\(r,e) ->r+e >= 20) (zip n b) 