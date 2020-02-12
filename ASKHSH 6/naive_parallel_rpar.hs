{-# OPTIONS_GHC -O2 -fno-warn-tabs #-}
{-# LANGUAGE BangPatterns #-}

import Data.Maybe  
import Data.Word
import Control.DeepSeq
import qualified Data.ByteString.Char8 as B
import Control.Monad.Par
import Control.Parallel.Strategies

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



-- Using Fermat's little theorem to compute nCk mod p
-- considering cancelation of p in numerator and denominator
fermat_binom :: MYTYPE -> MYTYPE -> MYTYPE -> MYTYPE
fermat_binom n k p = 
	-- quick check (avoid computations)
	if (num_degree > den_degree) || (k > n) then
		0
	-- compute
	else
		if k > n-k+1 then 
			epik
		else
			aplo
	where
		deep :: NFData a => a -> a
		deep a = deepseq a a 

		-- check degree
		num_degree = (fact_exp n p) - (fact_exp (n-k) p)
		den_degree = fact_exp k p 


		-- numerator and denominator 
		num = range (n-k+1) n p 1
		den = range 1 k p 1
		aplo = runEval $ do
			a <- rpar (deep num)
			b <- rpar (deep den)
			rseq a 
			rseq b
			return $ ((a*(fastpow b (p-2) p)) `rem` p)



		--epikalupsh
		epik_num = range (k+1) n p 1
		epik_den = range 1 (n-k) p 1
		epik = runEval $ do
			a <- rpar (deep epik_num) 
			b <- rpar (deep epik_den)
			rseq a 
			rseq b
			return $ (a * (fastpow b (p-2) p))  `rem` p
			


		-- Loop
		range !from to p !r = 
			if from == to+1 then r 
			else 
				range (from+1) to p ((r*from) `rem` p)



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



-- when p < n,k its real fast. 
-- split into smaller fermat problems
lucas_binom :: MYTYPE -> MYTYPE -> 	MYTYPE -> MYTYPE
lucas_binom n k p = 
	lb_loop np kp 1 
	where 
		-- base p coefficients of n and k
		np = reverse (get_base_digits_rev n p)
		kp = reverse (get_base_digits_rev k p)

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
