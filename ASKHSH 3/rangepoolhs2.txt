{-# OPTIONS_GHC -O2 -threaded #-}
{-# LANGUAGE BangPatterns #-}
import Data.Array.IO
import Control.Monad
import Control.DeepSeq (deepseq, force)
import qualified Data.ByteString.Char8 as B
import Data.Maybe
import Numeric
import Data.Int


--suntatic sugar. dont overdo
(!) = readArray

--Euclidean Modulo. always returns positive. ta merika
--a8roismata ta afairw to ena apo to allo. ara paizei timh < 0
em :: Int64 -> Int64 -> Int64
em !a !b = 	
	let m = a `rem` b
	in
		if m < 0 then
			if b < 0 then
				m - b
			else
				m + b
		else
			m

-- winstreaks pre-calculated
wins = [1,3,7,15,31,63,127,255,511,1023,2047,4095,8191,16383,32767,65535,131071,262143,524287] :: [Int]






-- function to compute DP-ith element. i == index, (x:xs) == wins, m == modulo
dp_sum_up :: IOUArray Int Int64 -> Int -> [Int] -> Int64 -> Int64 -> IO Int64
dp_sum_up dp_arr i [] !acc m = do return (em acc m)		--all winstreaks done

dp_sum_up dp_arr i (x:xs) !acc m = 
	-- cannot do that much a winstreak return accumulator
	if x > i then 
		do return (em acc m) :: IO Int64
	-- continue searching lower in dp_array
	else 
		do {	
			elem <- dp_arr ! (i - x) ;
			dp_sum_up dp_arr i xs (acc+elem) m
		}






--main :: IO ()
main = do

	-- ============= READ ============= --
	content <- getLine
	
	let [q, modulo]   = force $ map read $ words content :: [Int64]	
	let arr_size = 1000009
	
	
	-- ============= _DP_ ============= --
	
	dp_arr <- newArray_ (0, arr_size) 	-- undefined elements - faster
	let _ = dp_arr :: IOUArray Int Int64	-- declare type
	writeArray dp_arr 0 1				-- init values
	writeArray dp_arr 1 1
	
	
	-- __init__  DP_ARRAY
	forM_ [2..arr_size] $ \i -> do
		dp_elem <- dp_sum_up dp_arr i wins 0 modulo
		writeArray dp_arr i (force (em dp_elem modulo))
	
	
	-- ========= PARTIAL SUM ========== --
	
	part_arr <- newArray_ (0,arr_size)
	let _ = part_arr :: IOUArray Int Int64
	writeArray part_arr 0 1
	forM_ [1..arr_size] $ \i -> do
		!so_far <- part_arr ! (i-1)
		!now <- dp_arr ! i
		writeArray part_arr i (em (so_far + now) modulo)
	
	
	-- ============ PRINT ============= --
	
	forM_ [1..q] $ \i -> do 
		-- ============= READ ============= --
		--more efficient than read and print
		--use bytestrings and readInt
		content <- B.getLine 
		let [maybe_n1, maybe_n2] = map (B.readInt) (B.words content)
		let n1 = force $ fst $ fromMaybe (42, B.empty) maybe_n1
		let n2 = force $ fst $ fromMaybe (42, B.empty) maybe_n2
		
		-- ============= FETCH ============ --
		solution <-
			if n1 == 0 then 
				do {
					partial_n2 <- part_arr ! n2 ;
					return $ em (2 * partial_n2) modulo - 1
				}
			else do
				{
					partial_n1_1 <- part_arr ! (n1-1) ;
					partial_n2   <- part_arr ! n2 ;
					return $ em (2 * (partial_n2 - partial_n1_1)) modulo
				}
		
		-- ============= PRINT ============ --
		B.putStrLn $ B.pack $ show $ (solution :: Int64)
			
