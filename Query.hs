module Query where

import Data.List.Split
import Data.List
import qualified Data.Map as Map
import Numeric
import Movie
import Rating
import UserInfo

type Column = String
type TableSchema = [Column]

type Field = String
type Entry = [Field]
data Table = Table TableSchema [Entry]

type ColSeparator = Char
type LnSeparator = Char

read_table :: ColSeparator -> LnSeparator -> String -> Table
read_table col ln s = Table (endBy [col] (head (endBy [ln] s)))
                      (tail (map (endBy [col]) (endBy [ln] s)))

movie = read_table '|' '\n' movie_str
rating = read_table ' ' '\n' rating_str
user_info = read_table '|' '\n' user_info_str

-- Operatii cu Table
header :: Table -> TableSchema
header (Table h e) = h
body :: Table -> [Entry]
body (Table he e) = e

-- concateneaza 2 tabele pe veriticala
add :: Table -> Table -> Table
add (Table [] _) t = t
add t (Table [] _) = t
add (Table h e) (Table h' e') = Table (h ++ h') (zipWith (++) e e')

-- intoarce un tabel cu header-ul format doar din field-ul primit
column :: Column -> Table -> Table
column _ (Table [] _) = Table [] []
column col (Table header entry) | head header == col = Table [col] (map (\l -> [head l]) entry)
                                | otherwise = column col (Table (tail header) (map tail entry))

-- intoarce un tabel cu ac header si primele n linii din tabelul dat
limitLine :: Int -> Table -> Table
limitLine n (Table hdr entris) = Table hdr (take n entris)

-- intoarce a cata coloana este cea primita
colNum :: Integer -> Column -> TableSchema -> Int
colNum _ _ [] = -1
colNum acc c (col:cols) = if c == col then fromInteger acc else colNum (acc + 1) c cols

-- primieste o lista de String reprezentand o coloana cu tot cu header si intoarce lungimea maxima
maxLen :: [String] -> Int
maxLen = foldr (\s acc -> if length s > acc then length s else acc) 0

-- intoarce o lista cu nr maxim de caractere de pe fiecare coloana
maxWidth :: [Int] -> Table -> [Int]
maxWidth acc (Table [] _) = acc
maxWidth acc (Table header entries) = maxWidth (acc ++ [maxLen (head header : map head entries)])
                                    (Table (tail header) (map tail entries))

showField :: String -> (String, Int) -> String
showField acc (col, width) = acc ++ '|':col ++ replicate (width - length col) ' '

-- show pentru fiecare linie si pentru header
showEntry :: Table -> [Int] -> TableSchema -> String
showEntry t list line = foldl showField "" (zipWith (\a b -> (a, b)) line list) ++ "|\n"

showEntries :: Table -> [Int] -> String
showEntries t list = concatMap (showEntry t list) (body t)

instance Show Table where
    show t = replicate (sum padding + length padding + 1) '-' ++ "\n"
        ++ showEntry t padding (header t) ++ replicate (sum padding + length (header t) + 1) '-'
        ++ "\n" ++ showEntries t padding ++ replicate (sum padding + length padding + 1) '-'++"\n"
            where padding = maxWidth [] t


data FilterCondition = Lt Column Integer | Eq Column String | In Column [String]
                     | Not FilterCondition

-- TODO 3
getFilter :: FilterCondition -> TableSchema -> (Entry -> Bool)
getFilter _ [] = const False
-- intorc functii care iau un Entry si verifica da al n-lea element corespunde cerintei din 
-- FilterCondition, unde n este pe a cata coloana din header este cea data
getFilter (Lt c n) header = \ e -> (read (head (drop (colNum 0 c header) e)) :: Integer) < n 
getFilter (Eq c s) header = \ e -> head (drop (colNum 0 c header) e) == s
getFilter (In c l) header = \ e -> head (drop (colNum 0 c header) e) `elem` l
getFilter (Not f) header = not . getFilter f header

-- TODO 4
data Query = Filter FilterCondition Query |  
             Select [String] Query |
             SelectLimit [String] Integer Query |
             Cosine Query |
             Query :|| Query |
             Atom Table

eval :: Query -> Table
eval (Atom t) = t
eval (Select [] _) = Table [] []
eval (Select (col:xs) q) = column col (eval q) `add` eval (Select xs q)
eval (SelectLimit [] n _) = Table [] []
eval (SelectLimit list n q) = limitLine (fromInteger n) (eval (Select list q))
eval (Filter cond q) = Table (header $ eval q) 
                            (filter (getFilter cond (header $ eval q)) (body $ eval q))
eval (q :|| q') = Table (header $ eval q) 
                 (foldr (\x l -> if x `elem` l then l else x:l) (body $ eval q') (body $ eval q))
eval (Cosine q) = Table ["user_id1", "user_id2", "sim"] (it (foldr f [] $ sort $ body $ eval q))

-- TODO 5
same_zone :: String -> Query
same_zone userID = Filter (Not (Eq "user_id" userID))
                 $ Select ["user_id", "occupation"]
                 $ Filter (Eq "zone" (propOf (Atom user_info) "user_id" userID "zone"))
                 $ Atom user_info

-- intoarce ce e pe coloana col' la linia care are pe coloana data, col, field-ul dat, fi
propOf :: Query -> Column -> Field -> Column -> Field
propOf q col fi col' = head $ head $ body (eval (Select [col'] (Filter (Eq col fi) q)))

male_within_age :: Integer -> Integer -> Query
male_within_age x y = Select ["occupation", "zone"]
                $ Filter (Not (Eq "age" (show x)))
                $ Filter (Not (Lt "age" x))
                $ Filter (Lt "age" y) 
                $ Filter (Eq "sex" "M")
                $ Atom user_info

mixed :: [String] -> [String] -> Integer -> Query
mixed zones occupations x = Select ["user_id"]
                          $ Filter (In "zone" zones) 
                          $ Filter (In "occupation" occupations)
                          $ Filter (Lt "age" x)
                          $ Atom user_info

-- functii pentru eval Cosine

-- funtie folosita la transformarea liste de entry uri intr lista cu dictionare
-- care contin rating urile dat de un anumit ultilizator tuturor filmele 
f :: Entry -> [(String, Map.Map String Float)] -> [(String, Map.Map String Float)]
f entry acc | null acc = [(head entry, Map.singleton (entry !! 1) (read (last entry)::Float))]
            | head entry == fst (head acc) 
        = (head entry, Map.insert (entry !! 1) (read (last entry)::Float) (snd (head acc))) : tail acc
            | otherwise = (head entry, Map.singleton (entry !! 1) (read (last entry)::Float)) : acc

-- iteraza prin toti utilizatorii si pe fiecare il compara cu toti ceilati de mai jos
-- (primeste entry uri deja sortate) 
it :: [(String, Map.Map String Float)] -> [Entry]
it list = foldr (\(user, dict) acc -> compareTo user dict list ++ acc) [] list

-- primeste un utilizator si iterazara prin toti ceilalti trurneaza o lista cu entry uri penru
-- (pentru tabelul nou) ale utilizatorului dat
compareTo :: String -> Map.Map String Float -> [(String, Map.Map String Float)] -> [Entry]
compareTo user1 dict1 = foldr (\ (user2, dict2) acc -> 
                        if user1 < user2 then [user1, user2, sim dict1 dict2] : acc else acc) []

-- ia un dictionar cu ratingurile unui utilizator si o lista formata dintr-un dictionar al altui
-- ultilizator si intoarce un singur dictionar cu cu notele ambilor utilizatori pentru multimea
-- intersetie a filmelor 
complet :: Map.Map String Float -> [(String, Float)] -> Map.Map String (Float, Float)
complet dict = foldr
				(\(key, el) acc -> Map.insertWith (\new old -> (fst old, snd new)) key (0, el) acc)
				(Map.map (\el -> (el, 0)) dict)

-- apeleaza funtia cosinus cu argument format dintr-un dicttionar obtinut din celele doua primite
-- si intoarce reziltatul in forma dorita
sim :: Map.Map String Float -> Map.Map String Float -> String
sim d1 d2 = showFFloat (Just 4) (cosinus (complet d1 (zip (Map.keys d2) (Map.elems d2)))) ""

-- primeste un dictionar cu toate notele de la filmele comune dintre 2 useri si intoarce
-- coeficientul de similaritate dintre ei
cosinus :: Map.Map String (Float, Float) -> Float
cosinus dict = sum ( zipWith (*) (map fst (Map.elems dict)) (map snd (Map.elems dict)) )
            / ( norma  (map fst (Map.elems dict)) * norma (map snd (Map.elems dict)) )

norma :: [Float] -> Float
norma = sqrt . sum . map (\ f -> f * f)
