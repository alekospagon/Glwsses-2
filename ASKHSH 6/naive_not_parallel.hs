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
			let 
				num = get_num n k p 	-- O(n)
				den = get_den k p 		-- O(n)
			in 
				(num * (fastpow den (p-2) p))  `rem` p



	where
		-- O(n)
		get_num n k p = gn_loop (n-k+1) 1
			where
				-- for i in range(n, n-k, -1): returns num
				gn_loop !i !r = 
					if (i == n+1) then r
					else 
						-- num = (cur*num)%p
						gn_loop (i+1)  ( ((gn_strip_p i) * r) `rem` p)

					where 
						-- while cur%p == 0: returns cur
						gn_strip_p cur = 
							if (cur `rem` p) /= 0 then cur
							else
								gn_strip_p (cur `div` p)

		-- O(n)
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
						gd_strip_p cur = 
							if (cur `rem` p) /= 0 then cur
							else
								gd_strip_p (cur `div` p)



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
