module Questoes where

--1
myEnumFromTo :: Int -> Int -> [Int]
myEnumFromTo x n
                    |x<=n = x : myEnumFromTo (x+1) n
                    |otherwise = []
--2
myenumFromThenTo :: Int -> Int -> Int -> [Int]
myenumFromThenTo a b c
                            |a<=c = a : myenumFromThenTo b (b+b-a) c
                            |otherwise = []
--3
myplus :: [a] -> [a] -> [a]
myplus [] [] = []
myplus [] l = l
myplus l [] = l
myplus (x1:xs) l2 = x1 : myplus xs l2

--4
encontraElem :: [a] -> Int -> a
encontraElem (x:xs) n
                            |n== 0 = x
                            |n>0 = encontraElem xs (n-1)

--5
myreverse :: [a] -> [a]
myreverse [] = []
myreverse (x:xs) = myreverse xs ++ [x]

--6
mytake :: Int -> [a] -> [a]
mytake 0 l1 = []
mytake _ [] = []
mytake n (x:xs) = x : mytake (n-1) xs

--7
drop1 :: Int -> [a] -> [a]
drop1 0 l1 = l1
drop1 _ [] = []
drop1 n (x:xs)
                    |n>0 = drop1 (n-1) xs
                    |otherwise = xs

--8
myzip :: [a] -> [b] -> [(a,b)]
myzip [] _ = []
myzip _ [] = []
myzip (x1:xs1) (x2:xs2) = (x1,x2) : myzip xs1 xs2

--9
myreplicate :: Int -> a -> [a]
myreplicate 0 x = []
myreplicate n x = x : myreplicate (n-1) x

--10
myintersperse :: a -> [a] -> [a]
myintersperse _ [] = []
myintersperse _ [h] = [h]
myintersperse n (x:xs) = [x] ++ [n] ++ myintersperse n xs

--11
mygroup :: Eq a => [a] -> [[a]]
mygroup [] = []
mygroup [x] = [[x]]
mygroup (h1:h2:t)
                        |h1==h2 = (h1:head gt) : tail gt
                        |otherwise = [h1]:gt
                         where gt = mygroup (h2:t)
--ou
group' :: Eq a => [a] -> [[a]]
group' [] = []
group' [x] = [[x]]
group' (x:xs)
                | elem x (head r) = (x:head r) : tail r
                |otherwise = [x] : r
        where r= group' xs

--12
concat1 :: [[a]] -> [a]
concat1 [] = []
concat1 (x:xs) = x ++ concat1 xs

--13
myinits :: [a] -> [[a]]
myinits [] = [[]]
myinits l = myinits (init l) ++ [l]

--14
mytails :: [a] -> [[a]]
mytails [] = [[]]
mytails l = [l] ++ mytails (tail l)

--15
myheads :: [[a]] -> [a]
myheads [] = []
myheads ([]:t) = myheads t
myheads (x:xs) = head x : myheads xs

--16
mytotal :: [[a]] -> Int
mytotal [] = 0
mytotal (x:xs) = length x + mytotal xs

--17
fun1 :: [(a,b,c)] -> [(a,c)]
fun1 [] = []
fun1 ((x,y,z):xs) = (x,z) : fun1 xs

--18
cola1 :: [(String,b,c)] -> String
cola1 [] = ""
cola1 ((x,y,z):xs) = x ++ cola1 xs

--19
idade :: Int -> Int -> [(String,Int)] -> [String]
idade _ _ [] = []
idade ano idademin ((nome,nascimento):xs)
                                            |ano-nascimento >= idademin = nome : idade ano idademin xs
                                            |otherwise = idade ano idademin xs

--20
powerEnumFrom :: Int -> Int -> [Int]
powerEnumFrom n 0 = [1]
powerEnumFrom n 1 = [n]
powerEnumFrom n m = aux n m 0
                            where   aux :: Int -> Int -> Int -> [Int]
                                    aux n m acc
                                                |acc/= m = n^acc : aux n m (acc+1)
                                                |otherwise = []
--ou 
powerEnumFrom1 :: Int -> Int -> [Int]
powerEnumFrom1 n 1 = [n]
powerEnumFrom1 n m
                    | m > 1 = powerEnumFrom n (m - 1) ++ [n^(m-1)]
                    | otherwise = []
--21
isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = if aux n 2 == 0 then True
                else False
                    where   aux :: Int -> Int ->  Int
                            aux x y
                                    |y>=x = 0
                                    |mod x y == 0 = 1
                                    |otherwise = aux x (y+1)

--22
isPrefixOf1 :: Eq a => [a] -> [a] -> Bool
isPrefixOf1 [] _ = True
isPrefixOf1 _ [] = True
isPrefixOf1 (x1:xs1) (x2:xs2)
                        |length (x1:xs1) <= length (x2:xs2) = if x1==x2 then isPrefixOf1 xs1 xs2 else False
                        |otherwise = False

--23
isSuffixOf1 :: Eq a => [a]-> [a] -> Bool
isSuffixOf1 [] _ = True
isSuffixOf1 _ [] = False
isSuffixOf1 (x1:xs1) (x2:xs2)
                        |x1==x2 = isPrefixOf1 xs1 xs2
                        |otherwise = isSuffixOf1 (x1:xs1) xs2

--24
isSubsequenceOf1 :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf1 [] _ = True
isSubsequenceOf1 _ [] = False
isSubsequenceOf1 (x1:xs1) (x2:xs2) = if x1 == x2 then isSubsequenceOf1 xs1 xs2 else isSubsequenceOf1 (x1:xs1) xs2

--25
elemIndices1 :: Eq a => a -> [a] -> [Int]
elemIndices1 x [] = []
elemIndices1 x l = aux 0 x l
                    where aux i x [] = []
                          aux i x (h:t) = if x == h then (i) : aux (i+1) x t else aux (i+1) x t
--26
nub1 :: Eq a => [a] -> [a]
nub1 [] = []
nub1 (x:xs) = x : nub1 (filter (/=x) xs)

--ou

nub2 :: Eq a => [a] -> [a]
nub2 [] = []
nub2 (x:xs)
                |elem x xs = nub2 xs
                |otherwise = x: nub2 xs
--ou

nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (h:t) = if pertence h t then nub' t
            else h : nub' t

pertence :: Eq a => a -> [a] -> Bool
pertence x [] = False
pertence x (h:t) = if x == h then True
                   else pertence x t


--27
delete1 :: Eq a => a -> [a] -> [a]
delete1 _ [] = []
delete1 x (h:h2:t) | x == h = h2:t
                   | x == h2 = h:t
                   |otherwise = delete1 x t

--28

barra :: Eq a => [a] -> [a] -> [a]
barra [] _ = []
barra l [] = l
barra (h:t) (x:xs) = barra (delete1 x (h:t)) xs

--29
uniao :: Eq a => [a] -> [a] -> [a]
uniao [] l = l
uniao l [] = l
uniao l (h:t) = if elem h l then uniao l t
                     else uniao (l ++ [h]) t
--30
intersect :: Eq a => [a] -> [a] -> [a]
intersect l [] = []
intersect [] l = []
intersect (h:t) (x:xs) = if elem h (x:xs) then h : intersect t (x:xs)
                         else intersect t (x:xs)

--31
insert1 :: Ord a => a -> [a] -> [a]
insert1 x [] = [x]
insert1 x (h:t) = if x <= h then x:h:t
                 else h : insert1 x t

--32
unwordss' :: [String] -> String
unwordss' [] = []
unwordss' [x] = x
unwordss' (h:t) = h ++ " " ++ unwordss' t

--33
unliness' :: [String] -> String
unliness' [] = []
unliness' (h:t) = h ++ "\n" ++ unliness' t

--34
pMaior :: Ord a => [a] -> Int
pMaior (h:t) = aux3 h t 0 1

aux3 :: Ord a => a -> [a] -> Int -> Int -> Int
aux3 _ [] x _ = x
aux3 m (h:t) x n = if h > m then aux3 h t n (n+1)
                   else aux3 m t x (n+1)
{---ou
pmaior1 :: Ord a => [a] -> Int
pmaior1 [x] = 0
pmaior1 (x:xs)
                |x >= (xs !! x) = 0
                |otherwise = 1+x
                    where x= pmaior1 xs
                    -}
--35
lookupp' :: Eq a => a -> [(a,b)] -> Maybe b
lookupp' _ [] = Nothing
lookupp' x ((a,b):t) = if x == a then Just b
                       else lookupp' x t

--36
preCrescente :: Ord a => [a] -> [a]
preCrescente [] = []
preCrescente [x] = [x]
preCrescente (x:y:t) = if x <= y then x : preCrescente (y:t)
                       else [x]

--37
iSortt :: Ord a => [a] -> [a]
iSortt [] = []
iSortt (h:t) = insert2 h (iSortt t)

insert2 :: Ord a => a -> [a] -> [a]
insert2 x [] = [x]
insert2 x (h:t) = if x <= h then x:h:t
                  else h : insert2 x t

--38
menor :: String -> String -> Bool
menor [] [] = True
menor l [] = False
menor [] l = True
menor (x:xs) (h:t) | x < h =  True
                   | x > h = False
                   | otherwise = menor xs t

--39
elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet _ [] =  False
elemMSet x ((a,b):t) = if x == a then True
                       else elemMSet x t

--40
converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((a,b):t) = if b > 0 then a : converteMSet ((a,b-1):t)
                         else converteMSet t

--41
insereMSet :: Eq a => a -> [(a,Int)] -> [(a, Int)]
insereMSet x [] = [(x,1)]
insereMSet x ((a,b):t) = if x == a then (a,b+1):t
                         else (a,b) : insereMSet x t

--42
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet x [] = []
removeMSet x ((a,b):t) | x == a && b == 1 = t
                       | x == a = (a,b-1):t
                       | otherwise = (a,b) : removeMSet x t

--43 há formas muito mais eficientes de fazer isto
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet (h:t) = auxVersao19mil h 1 t : constroiMSet (dropwhile'' h t)

auxVersao19mil :: Ord a => a -> Int -> [a] -> (a, Int)
auxVersao19mil h n [] = (h,n)
auxVersao19mil h n (x:xs) = if h == x then auxVersao19mil h (n+1) xs
                            else (h,n)

dropwhile'' :: Eq a => a -> [a] -> [a]
dropwhile'' h [] = []
dropwhile'' h (x:xs) = if (h == x) then dropwhile'' h xs
                       else (x:xs)
--ou
constroiMSet1 :: Ord a => [a] -> [(a,Int)]
constroiMSet1 [] = []
constroiMSet1 (l:ls) = insereMSet l (constroiMSet ls)


--44
partitionEitherss :: [Either a b] -> ([a], [b])
partitionEitherss [] = ([],[])
partitionEitherss (h:t) = case h of
        Left a -> (a:x , y)
        Right b -> (x , b:y )
    where (x,y) = partitionEitherss t
--ou 
partitionEithers1 :: [Either a b] -> ([a],[b])
partitionEithers1 [] = ([],[])
partitionEithers1 ((Left a):t) = (a : as,bs)
    where (as,bs) = partitionEithers1 t
partitionEithers1 ((Right b):t) = (as,b : bs)
    where (as,bs) = partitionEithers1 t
--45
catMaybes' :: [Maybe a] -> [a]
catMaybes' [] = []
catMaybes' (h:t) = case h of
    Just a -> a : catMaybes' t
    Nothing -> catMaybes' t

--46
data Movimento = Norte | Sul | Este | Oeste
               deriving Show

caminho :: (Int, Int) -> (Int, Int) -> [Movimento]
caminho (x,y) (x2,y2) | x > x2 = Oeste : caminho (x-1,y) (x2,y2)
                      | x < x2 = Este : caminho (x+1, y) (x2,y2)
                      | y > y2 = Sul : caminho (x, y-1) (x2,y2)
                      | y < y2 = Norte : caminho (x, y+1) (x2,y2)
                      | otherwise = []
{-
--47 que feio, tentar otimizar
hasLoops :: (Int, Int) -> [Movimento] -> Bool
hasLoops (x,y) [] = False
hasLoops (x,y) (h:t) = case h of
    Norte -> aux'' (x,y) (x,y+1) t
    Sul -> aux'' (x,y) (x,y-1) t
    Este -> aux'' (x,y) (x+1,y) t
    Oeste -> aux'' (x,y) (x-1, y) t

aux'' :: (Int, Int) -> (Int, Int) -> [Movimento] -> Bool
-- (xi,yi) x inicial y inicial   ...  (xa,ya) x atual y atual
aux'' (xi, yi) (xa, ya) [] = if xi == xa && yi == ya then True
                             else False
aux'' (xi, yi) (xa,ya) (h:t) = if xi == xa && yi == ya then True
                               else case h of
                                   Norte -> aux'' (xi,yi) (xa,ya+1) t
                                   Oeste -> aux'' (xi,yi) (xa-1,ya) t
                                   Sul -> aux'' (xi,yi) (xa,ya-1) t
                                   Este -> aux'' (xi,yi) (xa+1, ya) t
                                   -}
posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao p [] = p
posicao (x, y) (Norte:t) = posicao (x, y + 1) t
posicao (x, y) (Sul:t) = posicao (x, y - 1) t
posicao (x, y) (Este:t) = posicao (x + 1, y) t
posicao (x, y) (Oeste:t) = posicao (x - 1, y) t


hasLoops :: (Int,Int) -> [Movimento] -> Bool
hasLoops _ [] = False
hasLoops posi movs = posi == posicao posi movs || hasLoops posi (init movs)

--48
type Ponto = (Float,Float)
data Rectangulo = Rect Ponto Ponto

contaQuadrados :: [Rectangulo] -> Int
contaQuadrados [] = 0
contaQuadrados ((Rect (x1,y1)(x2,y2)):xs)
                                                |abs (x2-x1) == abs (y2-y1) = 1 + contaQuadrados xs
                                                |otherwise = contaQuadrados xs

--49
areaTotal :: [Rectangulo] -> Float
areaTotal [] = 0
areaTotal ((Rect (x1,y1)(x2,y2)):xs) = (abs (x2-x1) * abs (y2-y1)) + areaTotal xs

--50
data Equipamento = Bom | Razoavel | Avariado
                    deriving (Show)
naoReparar :: [Equipamento] -> Int
naoReparar [] = 0
naoReparar (Bom:xs) = 1 + naoReparar xs
naoReparar (Razoavel:xs) = 1 + naoReparar xs
naoReparar (Avariado:xs) = naoReparar xs
