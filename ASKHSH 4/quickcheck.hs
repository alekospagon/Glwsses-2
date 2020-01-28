{-# OPTIONS_GHC -O2 -fno-warn-tabs #-}

import Test.QuickCheck 
import Control.Monad
import Text.Show.Functions

-- | A polymorphic n-ary tree data structure.
-- | Note: a tree cannot be empty
data Tree a = Node a [Tree a]
	deriving Show


-- | Task 1 & 2
instance Arbitrary a => Arbitrary (Tree a) where
	-- | Task 1
	-- | from QuickCheck{ sized :: (Int -> Gen a) -> Gen a }
	arbitrary = sized testTree
		where
			testTree :: Arbitrary a => Int -> Gen (Tree a)

			-- | just one node (no subtrees)
			testTree 0 = do
				a <- arbitrary
				return $ Node a []

			-- | n nodes to add
			testTree nodes = do
				-- | take a `random` number
				(Positive children) <- arbitrary
				-- | how many nodes per subtree?
				let perChild = nodes `div` (children + 1)
				-- | same process for every subtree
				subTrees <- replicateM children (testTree perChild)
				-- | this node info
				nodeinfo <- arbitrary
				-- | return whole Tree
				return $ Node nodeinfo subTrees

	-- | Task 2
	{-
	shrink = shrinkTree
		where	
			-- | shrink tree
			shrinkTree :: Arbitrary a => Tree a -> [Tree a]	
			shrinkTree (Node a []) = [ Node a [] ]
			shrinkTree (Node a [chl]) = 
				-- | 1. shrink to the extreme: keep only root
				[(Node a [])] ++
				-- | 2. keep any subtree as it is
				[chl] ++ 
				-- | 3. Shrink root info
				[Node a' [chl] | a' <- shrink a] ++
				-- | 4. Shrink every child recursively
				[Node a [chl'] | ]


			-- | Iterate every subtree and either shrink it or not
			shrinkEveryChild [] = []
			shrinkEveryChild (chl:chls) = 
				-- | shrink here
				(shrink chl : chls)
	-}
				
	
{-
 __    __     ______     ______     ______     ______    
/\ "-./  \   /\  ___\   /\  == \   /\  ___\   /\  ___\   
\ \ \-./\ \  \ \  __\   \ \  __<   \ \ \__ \  \ \  __\   
 \ \_\ \ \_\  \ \_____\  \ \_\ \_\  \ \_____\  \ \_____\ 
  \/_/  \/_/   \/_____/   \/_/ /_/   \/_____/   \/_____/
-}

-- | Task 5	
merge :: (a -> a -> a) -> (Tree a) -> (Tree a) -> (Tree a)
merge f t1 t2 = tree_merge f t1 t2
	where
		-- | merge infos and merge children
		tree_merge :: (a -> a -> a) -> (Tree a) -> (Tree a) -> (Tree a)
		tree_merge f (Node a c1) (Node b c2) =
			Node (f a b) (tree_merge_children f c1 c2)


		-- | merge subtrees
		tree_merge_children :: (a -> a -> a) -> [Tree a] -> [Tree a] -> [Tree a]
		tree_merge_children f [ ] c = c 	-- No more subtrees on t1
		tree_merge_children f c [ ] = c 	-- No more subtrees on t2

		-- | both have a subtree here
		tree_merge_children f (c1:c1s) (c2:c2s) = 
			tree_merge f c1 c2 : tree_merge_children f c1s c2s





list_max :: [Int] -> Int
list_max xs = foldr max 0 xs

tree_height :: Tree a -> Int
tree_height (Node a []) = 1
tree_height (Node a ls) = 
	1 + list_max (map (tree_height) ls)

tree_size :: Tree a -> Int
tree_size (Node a []) = 1
tree_size (Node a ls) = 
	1 + sum (map (tree_size) ls)


{-
 ______   ______     ______     ______   __     __   __     ______    
/\__  _\ /\  ___\   /\  ___\   /\__  _\ /\ \   /\ "-.\ \   /\  ___\   
\/_/\ \/ \ \  __\   \ \___  \  \/_/\ \/ \ \ \  \ \ \-.  \  \ \ \__ \  
   \ \_\  \ \_____\  \/\_____\    \ \_\  \ \_\  \ \_\\"\_\  \ \_____\ 
    \/_/   \/_____/   \/_____/     \/_/   \/_/   \/_/ \/_/   \/_____/ 
                                                                      
-}


-- | Task 3

-- | dfs - bfs: index or root must be one
prop_root_one :: (Tree a -> Tree (a, Integer)) -> Tree a -> Bool
prop_root_one f tree = 
	root_num (f tree) == 1
	where
		root_num :: Tree (a, Integer) -> Integer
		root_num (Node (_, b)  _) = b
		

-- | applying func to tree conserves size (#nodes)
prop_conserve_size :: (Tree a -> Tree b) -> Tree a -> Bool
prop_conserve_size func tree = 
	tree_size tree == tree_size (func tree)


-- | applying func to tree conserves height
prop_conserve_height :: (Tree a -> Tree b) -> Tree a -> Bool
prop_conserve_height func tree = 
	tree_height tree == tree_height (func tree)



-- | merge must give a (possibly) larger tree
prop_merge_size :: (a-> a -> a) -> (Tree a) -> (Tree a) -> Bool
prop_merge_size f t1 t2 = 
	tree_size (merge f t1 t2) >= max (tree_size t1) (tree_size t2)

-- | wrong must EXPLODE here. 
prop_wrong_size :: (a-> a -> a) -> (Tree a) -> (Tree a) -> Bool
prop_wrong_size f t1 t2 = 
	tree_size (wrong f t1 t2) >= max (tree_size t1) (tree_size t2)




{-
 _____     ______   ______           ______     ______   ______    
/\  __-.  /\  ___\ /\  ___\         /\  == \   /\  ___\ /\  ___\   
\ \ \/\ \ \ \  __\ \ \___  \        \ \  __<   \ \  __\ \ \___  \  
 \ \____-  \ \_\    \/\_____\        \ \_____\  \ \_\    \/\_____\ 
  \/____/   \/_/     \/_____/         \/_____/   \/_/     \/_____/
-}

-- | Function that annotates each node of a tree with the order in which
-- | a DFS traversal would visit it.

dfs :: Tree a -> Tree (a, Integer)
dfs t = fst (aux 1 t)
	where 
		aux :: Integer -> Tree a -> (Tree (a, Integer), Integer)
		aux k (Node x ts) = (Node (x, k) ts', k')
			where (ts', k') = auxs (k+1) ts
		auxs :: Integer -> [Tree a] ->
				([Tree (a, Integer)], Integer)
		auxs k [] = ([], k)
		auxs k (t : ts) = (t' : ts', k'')
			where 
				(t', k') = aux k t
				(ts', k'') = auxs k' ts
		                
-- | Function that annotates each node of a tree with the order in which
-- | a ΒFS traversal would visit it.
                
bfs :: Tree a -> Tree (a, Integer)
bfs t = t'
	where 
		(t', ks') = aux ks t
		ks = 1 : ks'
		aux (k : ks) (Node x ts) = (Node (x, k) ts', (k+1) : ks')
			where (ts', ks') = auxs ks ts
		auxs ks [] = ([], ks)
		auxs ks (t : ts) = (t' : ts', ks'')
			where 
				(t', ks') = aux ks t
				(ts', ks'') = auxs ks' ts


{-
 ______   ______     ______     ______   ______    
/\__  _\ /\  ___\   /\  ___\   /\__  _\ /\  ___\   
\/_/\ \/ \ \  __\   \ \___  \  \/_/\ \/ \ \___  \  
   \ \_\  \ \_____\  \/\_____\    \ \_\  \/\_____\ 
    \/_/   \/_____/   \/_____/     \/_/   \/_____/ 
-}





-- | Examples

t1 = Node 1 [ Node 2 [ Node 3 []
                     , Node 4 []
                     ]
            , Node 5 [ Node 6 [] ]
            ]

t2 = Node 'a' [ Node 'b' []
              , Node 'c' [ Node 'e' []
                         , Node 'f' []
                         ]
              , Node 'd' []
              ]

t3 = Node 1 [ Node 2 [ Node 3 []
                     , Node 4 []
                     ]
            , Node 5 [ Node 6 [] ]
            ]


-- | zipWith doesnt fill empty space of one tree by the other
wrong :: (a-> a -> a) -> Tree a -> Tree a -> Tree a
wrong f (Node x tsx) (Node y tsy) = Node (f x y) $ zipWith (wrong f) tsx tsy


main = do
	-- print (merge (+) t1 t3)
	-- print (wrong (+) t1 t3)

	-- | Task 4
	putStrLn "Checking index of root after DFS"
	quickCheck (prop_root_one dfs :: Tree Int -> Bool)
	putStrLn "Checking index of root after BFS"
	quickCheck (prop_root_one bfs :: Tree Int -> Bool)


	putStrLn "\nChecking size conservation of DFS"
	quickCheck (prop_conserve_size dfs :: Tree Int -> Bool)
	putStrLn "Checking size conservation of BFS"
	quickCheck (prop_conserve_size bfs :: Tree Int -> Bool)
	
	putStrLn "\nChecking height conservation of DFS"
	quickCheck (prop_conserve_height dfs :: Tree Int -> Bool)
	putStrLn "Checking height conservation of BFS"
	quickCheck (prop_conserve_height bfs :: Tree Int -> Bool)


	-- | Task 6

	-- a. Prove wrong is `wrong`
	putStrLn "\nChecking size property for wrong merge"
	quickCheck (prop_wrong_size (+) :: (Tree Int) -> (Tree Int) -> Bool)



	-- c. My merge satisfies the property
	putStrLn "\nChecking size increment by merge "
	quickCheck (prop_merge_size (+) :: (Tree Int) -> (Tree Int) -> Bool)
