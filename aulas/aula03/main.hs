import Data.Char

main = do 
  putStrLn "Vamos testar as funcoes abaixo!"

isVowel :: Char -> Bool
isVowel n = elem n "aeiouAeiou"

addComma :: [String] -> [String]
addComma n = map (\t -> t ++ ",") n

htmlListItems :: [String] -> [String]
htmlListItems n = map (\t -> "<li>" ++ t ++ "</li>") n

htmlListItems2 :: [String] -> [String]
htmlListItems2 n = map htmlListItemsAux n

htmlListItemsAux :: String -> String
htmlListItemsAux n = "<li>" ++ n ++ "</li>"

semVogais :: String -> String
semVogais n = filter (\t -> notElem t "aeiouAEIOU") n

semVogais2 :: String -> String
semVogais2 n = filter semVogaisAux n

semVogaisAux :: Char -> Bool
semVogaisAux n =  notElem n "aeiouAEIOU"

codifica :: String -> String
codifica n = map (\t -> if t == ' ' then ' ' else '-') n

codifica2 :: String -> String
codifica2 n = map codificaAux n

codificaAux :: Char -> Char
codificaAux t = if t == ' ' then ' ' else '-'

firstName :: String -> String
firstName n = takeWhile (' '<) n

isInt :: String -> Bool
isInt n = isIntAux2 (map isIntAux1 n)

isIntAux1 :: Char -> Bool
isIntAux1 n
 | n == '1' = True
 | n == '2' = True
 | n == '3' = True
 | n == '4' = True
 | n == '5' = True
 | n == '6' = True
 | n == '7' = True
 | n == '8' = True 
 | n == '9' = True 
 | n == '0' = True 
 | otherwise = False

isIntAux2 :: [Bool] -> Bool
isIntAux2 n = if ( filter (==False) n ) /= [] then False else True

lastName :: String -> String
lastName m = last ( words m )

userName :: String -> String
userName name = map toLower ([head name] ++ lastName name)


encodeName :: String -> String
encodeName n = map encodeNameAux n

encodeNameAux :: Char -> Char
encodeNameAux n
 | elem n "aA" = '4'
 | elem n "eE" = '3'
 | elem n "iI" = '2'
 | elem n "oO" = '1'
 | elem n "uU" = '0'
 | otherwise = n

betterEncodeName :: String -> String
betterEncodeName n = concat (map betterEncodeNameAux (splitString n))

splitString :: String -> [String]
splitString str = map (\x -> [x]) str

betterEncodeNameAux :: String -> String
betterEncodeNameAux n
  | n == "a" = "4"
  | n == "e" = "3"
  | n == "i" = "1"
  | n == "o" = "0"
  | n == "u" = "00"
  | n == "A" = "4"
  | n == "E" = "3"
  | n == "I" = "1"
  | n == "O" = "0"
  | n == "U" = "00"
  | otherwise = n

func1 :: [String] -> [String]
func1 n = map (\m -> take 10 (m ++ "..........")) n