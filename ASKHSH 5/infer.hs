{-# OPTIONS_GHC -O2 -optc-O2 #-}

-- Pagwnhs Alexandros 03116076 

import Data.Char
import System.IO
import Text.Read
import Data.Maybe
import Control.Monad

-- Define 

data Type  =  Tvar Int | Tfun Type Type                        deriving Eq
data Expr  =  Evar String | Eabs String Expr | Eapp Expr Expr  deriving Eq
type Constraints = [(Type, Type)]	
type Rules = [(Type, Type)]
type Env = [(String, Int)]



-- Printing expressions -- showsPrec: Convert a value to a readable String.

instance Show Expr where
	-- showsPrec a b: (from Text.Show)
	-- a: operator precedence
	-- b: expression to show

	-- showParen Bool Func
	-- puts parentheses when Bool is True

	showsPrec p (Evar x) = (x ++)
	showsPrec p (Eabs x e) =
		showParen (p > 0) ((("\\" ++ x ++ ". ") ++) . showsPrec 0 e)
	showsPrec p (Eapp e1 e2) =
		showParen (p > 1) (showsPrec 1 e1 . (" " ++) . showsPrec 2 e2)


-- Parsing of expressions


-- lexP: parse a single lexeme (=unit of lexical meaning)
-- (<++): Local, exclusive, left-biased choice: If left 
-- 		parser locally produces any result at all, then
-- 		right parser is not used.
instance Read Expr where
  readPrec = (do Ident x <- lexP
                 return (Evar x)) <++
             (do Punc "(" <- lexP
                 Punc "\\" <- lexP
                 Ident x <- lexP
                 Symbol "." <- lexP
                 e <- readPrec
                 Punc ")" <- lexP
                 return (Eabs x e)) <++
             (do Punc "(" <- lexP
                 e1 <- readPrec
                 e2 <- readPrec
                 Punc ")" <- lexP
                 return (Eapp e1 e2))


-- Pretty printing of types

instance Show Type where
	-- print type of var a
	showsPrec p (Tvar alpha) = ("@" ++) . showsPrec 0 alpha
	-- print type of fun s->t
	showsPrec p (Tfun sigma tau) =
		showParen (p > 0) (showsPrec 1 sigma . (" -> " ++) . showsPrec 0 tau)


-- Creating constraints

cons :: Expr -> Env -> Int -> Maybe (Type, Constraints, Int)
-- Var. no Constaints. 
cons (Evar x) g vars = do
	_type <- lookup x g
	Just (Tvar _type, [], vars)
-- Lambda. Put this constraint on constraints recursively unfolding e. 
cons (Eabs x e) g vars = do
	(_type, _c, _vars) <- cons e ((x, vars + 1):g) (vars + 1)
	Just ((Tfun (Tvar (vars + 1)) _type), _c, _vars + 1)
-- Application. take constraints of e1. pass them to constraints of e2.
-- Result: all constraints toghether. 
cons (Eapp e1 e2) g vars = do
	(_type, _cons, _vars)    <- cons e1 g vars
	(__type, __cons, __vars) <- cons e2 g _vars
   	Just 
   		(Tvar (__vars + 1), 
   		(_type, Tfun __type (Tvar (__vars + 1))):(_cons ++ __cons), __vars + 1)



sub_type :: Type -> Type -> Bool
sub_type x y@(Tvar a) = x == y
sub_type x y@(Tfun a b) =
    (sub_type x a) || (sub_type x b)


-- Replaces all occurrences of the first Type with the second Type in the Constraints
transform :: Type -> Type -> Constraints -> Constraints -> Constraints
-- no constraint. nothing to do
transform _ _ [] res_cons = res_cons
-- constraint (t1', t2') pass it and continue in c1.
transform x y ((a, b):cons) res_cons = 
	transform x y cons ((new_a, new_b):res_cons)
	where 
		-- replace wherever you can
		new_a = replace x y a
		new_b = replace x y b


replace t1 t2 t'@(Tvar a)
	| t1 == t' = t2
	| otherwise = t'
replace t1 t2 (Tfun t21 t22) =
	Tfun (replace t1 t2 t21) (replace t1 t2 t22)


-- W-algorithm

bind :: Constraints -> Rules -> Maybe Rules
-- no constraints
bind [] subs = Just subs
-- constraint: equality of types: I can ignore that constraint
bind ((t1, t2):c) subs
    | t1 == t2 = bind c subs
-- t1 not in t2
bind ((t1@(Tvar a), t2):c) subs
    | not (t1 `sub_type` t2) = bind (transform t1 t2 c []) ((t1, t2):subs)
-- t2 not in t1
bind ((t1, t2@(Tvar a)):c) subs
    | not (t2 `sub_type` t1) = bind (transform t2 t1 c []) ((t2, t1):subs)
-- Two funcs. I merge constraints and try again. 
bind ((t1@(Tfun t11 t12), t2@(Tfun t21 t22)):c) subs =
    bind ((t11, t21):(t12, t22):c) subs
bind _ _ = Nothing



-- Applies Type Substitution
change :: Type -> Rules -> Type
change t@(Tvar a) r =
    case lookup t r of
        Just t' -> replace t t' t
        Nothing -> t
change t@(Tfun t1 t2) r =
    Tfun (change t1 r) (change t2 r)



change_deep :: Type -> Rules -> Maybe Type
change_deep t r =
	if t == _type then Just t else change_deep _type r
	where
		_type = (change t r)


-- Sorts output types
sortTypes :: Type -> Maybe Type
sortTypes t = Just (change t (fst sorted_types))
    where sorted_types = createSortedList t 0 []
          createSortedList t@(Tvar a) counter subs =
            case lookup t subs of
                Just t -> (subs, counter)
                Nothing -> (((t, Tvar counter):subs), counter + 1)
          createSortedList t@(Tfun t1 t2) counter subs =
            let (subs1, counter1) = createSortedList t1 counter subs
                (subs2, counter2) = createSortedList t2 counter1 subs1
            in (subs2, counter2)


-- tun testcase

infer = do 
	s <- getLine
	-- Read Expression
	let e = read s :: Expr
	-- Make Constraints -> bind -> 
	-- Substitue -> Sort
	let typ = cons e [] 0 >>=
		\(t, c, _) -> bind c [] >>=
		change_deep t >>= sortTypes
		
	maybe (putStrLn "type error") print typ

-- run test-cases
runtest :: Int -> IO ()
runtest 1 = do infer
runtest n = do
	infer
	runtest (n-1)

-- take number and run testcases
main = do 
	n <- readLn
	runtest n
