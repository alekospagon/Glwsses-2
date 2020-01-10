{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns #-}



-- | A polymorphic n-ary tree data structure.

data Tree a = Node a [Tree a]
  deriving Show


-- | Function that annotates each node of a tree with the order in which
-- | a DFS traversal would visit it.

dfs :: Tree a -> Tree (a, Integer)
dfs t = fst (aux 1 t)
	where aux :: Integer -> Tree a ->
			   (Tree (a, Integer), Integer)
		aux k (Node x ts) = (Node (x, k) ts', k')
			where (ts', k') = auxs (k+1) ts
		auxs :: Integer -> [Tree a] ->
				([Tree (a, Integer)], Integer)
		auxs k [] = ([], k)
		auxs k (t : ts) = (t' : ts', k'')
			where (t', k') = aux k t
				(ts', k'') = auxs k' ts
                
-- | Function that annotates each node of a tree with the order in which
-- | a ΒFS traversal would visit it.
                
bfs :: Tree a -> Tree (a, Integer)
bfs t = t'
	where (t', ks') = aux ks t
		ks = 1 : ks'
		aux (k : ks) (Node x ts) = (Node (x, k) ts', (k+1) : ks')
			where (ts', ks') = auxs ks ts
		auxs ks [] = ([], ks)
		auxs ks (t : ts) = (t' : ts', ks'')
			where (t', ks') = aux ks t
				(ts', ks'') = auxs ks' ts




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



main = do
	print (dfs t1)
	print (dfs t2)
	print (bfs t1)
	print (bfs t2)
