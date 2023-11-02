nzp :: [Int] -> (Int,Int,Int)
nzp [] = (0,0,0)
nzp (x:xs)
            |x > 0 = (n,z,p+1)
            |x < 0 = (n+1,z,p)
            |x == 0 = (n,z+1,p)
                where (n,z,p) = nzp xs


intersect :: Eq a => [a] -> [a] -> [a]
intersect [] _ = []
intersect (h:t) l
    | elem h l = h : intersect t l
    | otherwise = intersect t l

    