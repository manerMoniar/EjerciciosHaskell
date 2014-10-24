import Data.List

ultimo :: [a] -> a
ultimo [x] = x
ultimo (_:xs) = ultimo xs
-- usa parentesis por que a lo ultimo envia un solo valor
-- xs == [] = (1,[x])



--lista ::[a] -> ([a], [a])
lista (h:xs) = listaAux [h] xs


listaAux l (x:xs)  | xs == [] = [(l, [x])]
				| otherwise = (l, (x:xs)) : listaAux y xs where y = l++[x]



subsets [] = [[]]
subsets (x:xs) = let subsets_xs = subsets xs
				in subsets_xs ++ [(x:z) | z <- subsets_xs]				

powerList :: [a] -> [[a]]
powerList [] = [[]]
powerList (x:xs) = (powerList xs) ++ (map (x:) (powerList xs))


validPotencia :: [a] -> [a]-> Bool
validPotencia [] = False
validPotencia b | length  == 2 ^ length xs = True
				| otherwise = False


main = do
	let a = [1,2,3]
	print(powerList a)
	let b = subsets a
	print(validPotencia a b)
