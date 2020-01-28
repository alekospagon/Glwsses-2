{-# OPTIONS_GHC -O2 -fno-warn-tabs #-}
{-# LANGUAGE BangPatterns, BlockArguments #-}

import Data.Word
import Control.DeepSeq
import Control.Monad.Par
import Control.Parallel.Strategies
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

-- compute product i where i is: {from `from` to `to`} modulo p
-- striping on every i any occurence of p
parr_range !from !to !p = 
	par_fact
	where
		-- split into four zones
		half = (from + to)   `div` 2
		low  = (from + half) `div` 2
		high = (half + to)   `div` 2

		-- ways to compute four zones
		fst = g_loop from low 1
		snd = g_loop low half 1
		thr = g_loop half high 1
		frt = g_loop high (to+1) 1

		-- Run four computations in parallel and merge results
		par_fact = runPar $ do 
			res1 <- spawnP fst
			res2 <- spawnP snd
			res3 <- spawnP thr
			res4 <- spawnP frt
			a <- get res1
			b <- get res2
			c <- get res3
			d <- get res4
			let -- merge two
				merge1 = ((a*b)`rem`p)
				merge2 = ((c*d)`rem`p)
			-- merge four
			return $ ((merge1 * merge2)`rem`p)


		-- product from lower to upper
		g_loop !lower !upper !res =
			if (lower >= upper) then res
			else
				-- striping p is no good. p > n,k
				-- g_loop (lower+1) upper (((g_strip_p lower)*res) `rem` p)
				g_loop (lower+1) upper ((lower*res) `rem` p)


		-- while cur%p == 0: returns cur
		g_strip_p !cur = 
			if (cur `rem` p) /= 0 then cur
			else
				g_strip_p (cur `div` p)



-- numenator: (n-k+1)(n-k+2)...(n-1)n 	<- O(n)
get_num :: MYTYPE -> MYTYPE -> MYTYPE -> MYTYPE
get_num n k p =
	parr_range (n-k+1) n p 


-- denominator: k! 	<- O(n)
get_den :: MYTYPE -> MYTYPE -> MYTYPE
get_den k p =
	parr_range 1 k p 



-- Using Fermat's little theorem to compute nCk mod p
-- considering cancelation of p in numerator and denominator
fermat_binom :: MYTYPE -> MYTYPE -> MYTYPE -> MYTYPE
fermat_binom n k p = 
	-- quick check (avoid computations)
	if (num_degree > den_degree) || (k > n) then
		0
	-- compute
	else
		my_par -- numerator and denominator in parallel
	where
		-- check degree
		num_degree = (fact_exp n p) - (fact_exp (n-k) p)
		den_degree = fact_exp k p 
		-- numerator and denominator 
		num = get_num n k p
		den = get_den k p

		my_par = runPar $ do 
			res1 <- spawnP num 
			res2 <- spawnP den 
			a <- get res1 
			b <- get res2
			return $ (a*(fastpow b (p-2) p)) `rem` p
		



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





-- | SOLVE IN PARALLEL | --

-- solve query
solve :: [MYTYPE] -> MYTYPE
solve [n, k, p] = 
	lucas_binom n k p 	--same as fermat_binom when p > n,k
	

-- apply f in parallel for all queries
my_parMap :: (a->b) -> [a] -> Eval [b]
my_parMap f xs = mapM (rpar . f) xs

{-
deep :: NFData a => a -> a
deep a = deepseq a a 
-}

-- solve queries in parallel
parSolve :: [[MYTYPE]] -> [MYTYPE]
parSolve xs = {- deep $ -} runEval $ my_parMap solve xs



-- | IO FUNCS | --

-- fast print
fast_print :: MYTYPE -> IO ()
fast_print x = 
	B.putStrLn $ B.pack $ show $ x


-- read a single integer
rList :: String -> MYTYPE
rList = read


-- | main read, solve | --
main = do
	input <- getContents
	let (t:raw_queries) = lines input
	let queries = map (map rList . words) raw_queries
	let solutions = parSolve queries 
	mapM_ fast_print solutions
