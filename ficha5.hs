sortOn :: Ord b => (a->b) -> [a] -> [a]
sortOn f [] = []
sortOn f (x:xs) = insere f x (sortOn f xs)

insere :: Ord b => (a->b) -> a -> [a] -> [a]
insere f x [] = [x]
insere f x (y:ys)
                    |f x <= f y = x:y:ys
                    |otherwise = y : insere f x ys

type Polinomio = [Monomio]
type Monomio = (Float,Int) -- 2a componente é o grau (ex: 2x^2 fica (2,2))

selgrau :: Int -> Polinomio -> Polinomio
selgrau x l = filter (\(c,g) -> g==x) l