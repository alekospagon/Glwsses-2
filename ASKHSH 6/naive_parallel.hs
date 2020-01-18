{-# OPTIONS_GHC -O2 -fno-warn-tabs #-}
{-# LANGUAGE BangPatterns #-}

import Data.Word
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


-- O(n)
get_num :: MYTYPE -> MYTYPE -> MYTYPE -> MYTYPE
get_num n k p = -- gn_loop (n-k+1) 1
	withStrategy strat  ((upper_half * lower_half) `rem` p)
	where
		half = n - (k `div` 2)
		upper_half = gn_loop_high half 1
		lower_half = gn_loop_low (n-k+1) 1
		-- Run in parallel
		strat v = do rpar upper_half; rseq lower_half; return v


		-- for i in range(n, n-k, -1): returns num
		-- range n-k to n splits into two:
		-- half point is (n + (n-k))/2 == n - k/2. so:
		-- upper half gets range: half to n 
		-- lower half gets range: n-k+1 to half 
		-- half to n (half included)
		gn_loop_high i r = 
			if (i >= n + 1) then r   -- exceeded n
			else
				-- go higher
				gn_loop_high (i+1) ( ((gn_strip_p i) * r) `rem` p)

		-- n-k+1 to half   (half excluded)
		gn_loop_low i r = 
			if (i >= half) then r
			else
				gn_loop_low (i+1) ( ((gn_strip_p i) * r) `rem` p)

		-- while cur%p == 0: returns cur
		gn_strip_p !cur = 
			if (cur `rem` p) /= 0 then cur
			else
				gn_strip_p (cur `div` p)


-- O(n)
get_den :: MYTYPE -> MYTYPE -> MYTYPE
get_den k p = gd_loop 1 1
	where 
		-- for i in range(1, k+1): returns denom
		gd_loop !i !r = 
			if i == k+1 then r 
			else
				-- denom = (cur*denom)%p
				gd_loop (i+1) ( ((gd_strip_p i) * r) `rem` p)

			where
				-- while cur%p == 0: returns cur
				gd_strip_p !cur = 
					if (cur `rem` p) /= 0 then cur
					else
						gd_strip_p (cur `div` p)






-- Using Fermat's little theorem to compute nCk mod p
-- considering cancelation of p in numerator and denominator
fermat_binom :: MYTYPE -> MYTYPE -> MYTYPE -> MYTYPE
fermat_binom n k p = 
	-- quick check (avoid computations)
	if (num_degree > den_degree) || (k > n) then
		0
	-- compute
	else
		withStrategy strat  ((num*(fastpow den (p-2) p)) `rem` p)
	where
		-- check degree
		num_degree = (fact_exp n p) - (fact_exp (n-k) p)
		den_degree = fact_exp k p 
		-- numerator and denominator 
		num = get_num n k p
		den = get_den k p
		-- Run in parallel
		strat v = do rpar num; rseq den; return v



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

			

main = do
	--let a = lucas_binom 950 100 7

	-- 1 sec
	--let a = lucas_binom 54262776 30644515 67250069

	-- 1.8 sec
	--let a = lucas_binom 339446636 51256371 833504351

	-- 18 secs
	let a = lucas_binom 580086636 520507822 833504351

	-- 0.066 sec
	-- let a = fermat_binom 580086636 520507822 4253309


	print a
