
> import Control.Monad.State

> foo :: (a -> b) -> Maybe a -> Maybe b -- (b)
> foo f z = case z of
>		Just x -> Just (f x)
>		Nothing -> Nothing

> iomap :: (a -> b) -> IO a -> IO b -- (d)
> iomap f x = do y <- x
>		 return $ f y

> data Expr1 = Val1 Int
>	     | Div1 Expr1 Expr1

> myExpr = (Div1 (Val1 10) (Val1 0))
> myExpr2 = (Div1 (Val1 10) (Div1 (Val1 20) (Val1 4)))

> eval1 :: Expr1 -> Int
> eval1 (Val1 n)   = n
> eval1 (Div1 x y) = eval1 x `div` eval1 y

> eval1' :: Expr1 -> Maybe Int
> eval1' (Val1 n)   = Just n
> eval1' (Div1 x y) = case eval1' x of 
>			Nothing -> Nothing
>			Just n1 -> case eval1' y of
>				    Nothing -> Nothing
>				    Just n2 -> n1`safeDiv` n2

> safeDiv :: Int -> Int -> Maybe Int
> safeDiv n m = if m == 0 then Nothing else Just (n `div` m)

> seqn :: Maybe a -> Maybe b -> Maybe (a,b)
> seqn Nothing	_	    = Nothing
> seqn _	Nothing	    = Nothing
> seqn (Just x) (Just y)    = Just (x,y)

> eval :: Expr1 -> Maybe Int 
> eval (Val1 n)   = Just n
> eval (Div1 x y) = do  n <- eval x
>			m <- eval y
>			safeDiv n m

> apply f Nothing  = Nothing
> apply f (Just x) = f x

> type State = Int

> data ST0 a = S (State -> (a, State))

> apply1 :: ST0 a -> State -> (a,State)
> apply1 (S f) x = f x

> instance Monad ST0 where 
>
>   return x = S (\s -> (x,s))
> 
>   st >>= f = S $ \s -> let (x, s') = apply1 st s in
>			 apply1 (f x) s'

> fresh :: ST0 Int
> fresh = S (\n -> (n,n+1))

> wtf1 = fresh >> fresh >> fresh >> fresh

> myreturn x = S(\n -> (x,n))

> wtf2 = fresh >>= \n1 ->
>	    fresh >>= \n2 ->
>		fresh >>
>		    fresh >>
>		    myreturn[n1, n2]

> data Tree a = Leaf a 
>	      | Node (Tree a) (Tree a)
>	      deriving (Eq, Show)

> tree :: Tree Char
> tree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

> tagTree :: Tree a -> Tree (a,Int)
> tagTree t = snd $ helper 0 t

> helper n (Leaf x)	= (n+1, Leaf (x,n))
> helper n (Node l r)	= (n'', Node l' r')
>   where
>	(n',l')		= helper n l
>	(n'', r')	= helper n' r

> mlabel :: Tree a -> ST0 (Tree(a,Int))
> mlabel (Node l r) = do l' <- mlabel l
>			 r' <- mlabel r
>			 return (Node l' r')
> mlabel (Leaf x)   = do n <- fresh
>			 return (Leaf(x,n))

> label :: Tree a -> Tree (a,Int)
> label t = fst (apply1 (mlabel t) 0)


> freshS :: ST Int Int
> freshS = do n <- get
>	      put (n+1)
>	      return n















