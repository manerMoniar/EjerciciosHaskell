logEquiv2 :: (Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool) -> Bool
logEquiv2 bf1 bf2 = 
	and [(bf1 p q) == (bf2 p q) | p <- [True,False],
									  q <- [True,False]]

logEquiv3 :: (Bool -> Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool -> Bool) -> Bool
logEquiv3 bf1 bf2 = 
	and [(bf1 p q r) == (bf2 p q r) | p <- [True,False],
									  q <- [True,False],
									  r <- [True,False]]




--ejemplo1 =  (\ p q r -> not(p &&  q && r))
--		(\ a b c -> (not a) || (not b) || (not c))

--numero15 = (\p q r -> (p <= q) <= r)
--			(\ a b c -> a <= ( b <= c)) 

--numero16 = (\ p q -> p <= q) (\ a b -> (not b) <= (not a)) 
 
--numero17 = (\ p q -> (not p) == q) (\ p q -> p == (not q))

main =  do 
	print (logEquiv3 (\p q r -> (p <= q) <= r) (\ a b c -> a <= ( b <= c)) ) 
	print (logEquiv2 (\ p q -> p <= q) (\ a b -> (not b) <= (not a)) )
	print (logEquiv2 (\ p q -> (not p) == q) (\ p q -> p == (not q)) )
	
	
	
