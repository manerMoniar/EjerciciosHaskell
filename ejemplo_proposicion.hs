logEquiv3 :: (Bool -> Bool -> Bool -> Bool) ->
				(Bool -> Bool -> Bool -> Bool) -> Bool
logEquiv3 bf1 bf2 = 
	and [(bf1 p q r) == (bf2 p q r) | p <- [True,False],
									   q <- [True,False],
									   r <- [True,False]]

 
main = print (logEquiv3
		(\ p q r -> not(p &&  q && r))
		(\ a b c -> (not a) || (not b) || (not c))
	)
