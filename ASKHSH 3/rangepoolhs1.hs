{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns #-}
--import Data.IntMap (IntMap, (!), fromList, fromDistinctAscList)
--import qualified Data.IntMap as M
import Data.Array
import Control.DeepSeq (deepseq, force)
import qualified Data.ByteString.Char8 as B
import Data.Maybe
import Numeric
import Data.Int



{-
	!!!!!!!!!!!!!!!!!
	IMMUTABLE ARRAY IS ALSO PURE -> 
	but you can switch to map going O(logn) instead of O(1)
	kanw import se auta pou exw kanei sxolia kai twra kanw sxolia
	to array. kai bazw auth th grammh gia IntMap. den brhka pio grhgoro
	map giauto evala array. to map paei sta 4,5 deuterolepta

	pre_calculated_ways n m = M.fromDistinctAscList [ (x, save_ways x m) | x <- [0..n]]
-}



--Euclidean Modulo. always returns positive. ta merika
--a8roismata ta afairw to ena apo to allo. ara paizei timh < 0
em :: Int64 -> Int64 -> Int64
em !a !b = 
	let !m = a `rem` b
	in
		if m < 0 then
			if b < 0 then
				m - b
			else
				m + b
		else
			m



--Pre-calculated winstreaks
wins :: [Int]	--wins = [2^_pow - 1 | _pow <- [1..19]]
wins = [1,3,7,15,31,63,127,255,511,1023,2047,4095,8191,16383,32767,65535,131071,262143,524287]



--COMPUTE DP_ARRAY i-th ELEMENT
--traverse winstreaks. accumulate earnings 
dp_sum_up :: Array Int Int64 -> Int -> [Int] -> Int64 -> Int64 -> Int64
dp_sum_up dp_arr i [] !acc m = --all winstreaks done
	force (em acc m)
dp_sum_up dp_arr i (x:xs) !acc m = 
	-- cannot do that much a winstreak -> stop
	if x > i then 
		force (em acc m)
	-- continue searching lower in dp_array
	else 
		let elem = dp_arr ! (i-x)
		in
		dp_sum_up dp_arr i xs (acc+elem) m



-- WAYS n CAN BE REACHED
ways :: Int -> Int64 -> Array Int Int64
ways n mod = force arr	--memoized (mapped) solutions
	where
		arr = pre_calculated_ways n mod
	
		pre_calculated_ways n m = listArray (0, n) [force (save_ways x m) | x <- [0..n]]
	
		save_ways n m = 
			if n == 0 || n == 1 then 1
			else force $ em (sum_) m
				where 	
					sum_ = force $ dp_sum_up arr n wins 0 m
			

		
-- PARTIAL SUMS OF DP_ARRAY UP TO n
partial :: Int -> Int64 -> Array Int Int64 
partial n mod = force arr2	--memoized (mapped) solutions
	where
		arr2 = pre_calculated_part n mod
	
		pre_calculated_part n m = listArray (0,n) [force (save_part x m) | x <- [0..n]]
	
		take_ways = ways n mod
		save_part n m = 
			if n == 0 then 1
			else em ( arr2 ! (n-1) + take_ways ! n ) m
			

		--pre_calculated_part n m = M.fromDistinctAscList [ (x, save_part x m)  | x <- [0..n] ]
		

		



--calculate result given partial sums
solve :: Int -> Int -> Int64 -> Array Int Int64 -> Int64
solve n1 n2 modulo part = 
	-- [0,x]
	if n1 == 0 then
		-- [0,0]
		if n2 == 0 then
			0
		else
			em (2 * (part!n2)) modulo - 1

	else
		let temp_res = 2 * ((part!n2) - (part!(n1-1)))
		in em temp_res modulo



--make partial sums (ONCE). read and print
final :: Int64 -> Int64 -> IO ()
final q modulo = my_loop q modulo
	where

	helper = force $ partial 1000009 modulo	--ONCE


	my_loop q modulo = 
		if q == 0 then return ()
		else do
			-- ============= READ ============= --
			content <- B.getLine 
			let [maybe_n1, maybe_n2] = map (B.readInt) (B.words content)
			let n1 = force $ fst $ fromMaybe (42, B.empty) maybe_n1
			let n2 = force $ fst $ fromMaybe (42, B.empty) maybe_n2
			
			
			-- ============= FETCH ============ --
			let res  = force $ solve n1 n2 modulo helper
			
			-- ============= PRINT ============ --
			B.putStrLn $ B.pack $ show $ (res :: Int64)
			
			-- ============= NEXT ============= --
			my_loop (q-1) modulo



main = do
	content <- getLine
	
	let [q, mod] = map read $ words content :: [Int64]	
	
	final q mod
