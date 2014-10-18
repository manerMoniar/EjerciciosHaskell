logEquiv2 :: (Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool) -> Bool
logEquiv2 bf1 bf2 = 
	and [(bf1 p q) == (bf2 p q) | p <- [True,False],
								  q <- [True,False]]

logEquiv3 :: (Bool -> Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool -> Bool) -> Bool
logEquiv3 bf1 bf2 = 
	and [(bf1 p q r) == (bf2 p q r) | p <- [True,False],
									  q <- [True,False],
									  r <- [True,False]]


numero15 = logEquiv3 (\p q r -> (p <= q) <= r)
					 (\ a b c -> a <= ( b <= c))

numero16 = logEquiv2 (\ p q -> p <= q)
					 (\ a b -> (not b) <= (not a)) 
 
numero17 = logEquiv2 (\ p q -> (not p) == q)
					 (\ p q -> p == (not q))

numero18 = logEquiv2 (\ p q -> not(p /= q))
					 (\ p q -> p == q)

numero19 = logEquiv2 (\ p q -> not(p) == not(q))
					 (\ p q -> p == q)

numero20 = logEquiv3 (\ p q r -> (p <=q ) && (p <= r))
					 (\ p q r -> p <= (q && r))

main =  do 	
	putStr "Ejercicio 15: "
	print numero15
	putStr "Ejercicio 16: "
	print numero16
	putStr "Ejercicio 17: "
	print numero17	
	putStr "Ejercicio 18: "
	print numero18
	putStr "Ejercicio 19: "
	print numero19
	putStr "Ejercicio 20: "
	print numero20