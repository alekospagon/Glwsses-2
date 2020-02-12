{-# OPTIONS_GHC -O2 -fno-warn-tabs #-}
{-# LANGUAGE BangPatterns #-}

import Data.Maybe  
import Data.Word
import qualified Data.ByteString.Char8 as B

type MYTYPE = Word64  --faster than Integer or Int64.


-- https://www.reddit.com/r/haskell/comments/mqtk6/fast_power_function/c33n70a/
-- (base ^ exp) % modulo
fastpow :: MYTYPE -> MYTYPE -> MYTYPE -> MYTYPE
fastpow base exp modulo = 
	-- put extra variable r to 
	fastpow' (base `rem` modulo) exp modulo 1
	where 
		-- FORCE r EVALUATION in pattern matching
		-- base ^ 0 --> result
		fastpow' b 0 m !r = r
		-- general case: b = b^2, e = e/2 , res
		fastpow' b e m r = 
			fastpow' (b * b `rem` m) (e `div` 2) m (if even e then r else (r * b `rem` m))


-- degree of p in n!  (exponent of p in the factorization of n!)
fact_exp :: MYTYPE -> MYTYPE -> MYTYPE
fact_exp n p = 
	-- loop. init: e = 0, u = p. 
	-- while u <= n:
	-- 	e += n/u,  u *= p
	fact_exp_loop 0 p
	where
		fact_exp_loop e u = 
			if u > n then e 
			else fact_exp_loop (e + (n `div` u)) (u * p)


-- convert given number n into array of its base b representation
-- most significant digit is at rightmost position in array

-- make list from left to right cause insertion is O(1) then reverse
-- so O(n^2) becomes O(n)
get_base_digits_rev :: MYTYPE -> MYTYPE -> [MYTYPE]
get_base_digits_rev n b = base_d_loop n [ ]
	where
		base_d_loop n res = 
			if n <= 0 then res
			else
				base_d_loop (n `div` b) ((n `rem` b) : res)



-- Using Fermat's little theorem to compute nCk mod p
-- considering cancelation of p in numerator and denominator
fermat_binom :: MYTYPE -> MYTYPE -> MYTYPE -> MYTYPE
fermat_binom n k p = 
	let
		num_degree = (fact_exp n p) - (fact_exp (n-k) p)
		den_degree = fact_exp k p 
	in
		-- quick check (avoid computations)
		if (num_degree > den_degree) || (k > n) then
			0
		-- compute
		else
			if k > (n-k+1) then
				epikalupsh_bin
			else
				bin
	where
		-- Binom			
		num = range (n-k+1) n p 1 	-- O(n)
		den = range 1 k p 1 		-- O(n)
		bin = (num * (fastpow den (p-2) p))  `rem` p

		-- Epik Binom
		epik_num = range (k+1) n p 1
		epik_den = range 1 (n-k) p 1
		epikalupsh_bin = 
			(epik_num * (fastpow epik_den (p-2) p))  `rem` p

		-- Loop
		range !from to p !r = 
			if from == to+1 then r 
			else 
				range (from+1) to p ((r*from) `rem` p)




-- when p < n,k its real fast. 
lucas_binom n k p = 
	let 
		np = reverse (get_base_digits_rev n p)
		kp = reverse (get_base_digits_rev k p)
	in 
		lb_loop np kp 1 

		where 
			-- | no np. stop
			lb_loop [ ] _  !binom = binom
			-- | np but no kp. put 0 as coefficient
			lb_loop (ni:ns) [ ] !binom = 
				lb_loop ns [ ] ((binom * (fermat_binom ni 0  p)) `rem` p)
			-- | both.
			lb_loop (ni:ns) (ki:ks) !binom = 
				lb_loop ns ks  ((binom * (fermat_binom ni ki p)) `rem` p)

			



final :: Int -> IO ()
final t = read_and_exec t 
	where 
		read_and_exec t = 
			if t == 0 then return ()
			else do
				content <- B.getLine

				let 
					[maybe_n, maybe_k, maybe_p] = map (B.readInt) (B.words content)
					n = fst $ fromMaybe (42, B.empty) maybe_n
					k = fst $ fromMaybe (42, B.empty) maybe_k
					p = fst $ fromMaybe (42, B.empty) maybe_p
				
					itow = fromIntegral -- Int to Word64

					res = lucas_binom (itow n) (itow k) (itow p)

				
			    	B.putStrLn $ B.pack $ show $ res

				read_and_exec (t-1)



main = do
	raw_t <- getLine
	let t = (read raw_t) :: Int
	final t
