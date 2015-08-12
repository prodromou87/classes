
 import Control.Monad.State hiding (when)

> import Data.Map

> data ST s a = S (s -> (a,s))

> instance Monad (ST s) where
>   return x   = S (\s -> (x, s))
>   st >>= f   = S (\s -> let (x, s') = apply st s 
>                         in apply (f x) s')

> apply :: ST s a -> s -> (a, s)
> apply (S f) x = f x

> get = S (\s -> (s,s))

> put s' = S (\_ -> ((), s'))

> data Tree a = Leaf a
>             | Node (Tree a) (Tree a)
>             deriving (Eq, Show)

> freshS :: ST Int Int
> freshS = do n <- get
>             put (n+1)
>             return n

> mlabelS :: Tree a -> ST Int (Tree (a, Int))
> mlabelS (Leaf x)	= do n <- freshS
>			     return (Leaf (x,n))
> mlabelS (Node l r)	= do l' <- mlabelS l
>			     r' <- mlabelS r
>			     return (Node l' r')

> data MyST a = M { index :: Int
>		  ,  freq :: Map a Int}
>	        deriving (Eq, Show)

> freshM = do
>   s	    <- get
>   let n   = index s
>   put $ s { index = n + 1 }
>   return n

> updFreqM k = do
>   s <- get
>   let f = freq s
>   let n = findWithDefault 0 k f
>   put $ s {freq = insert k (n+1) f}

> mlabelM (Leaf x)    = do updFreqM x
>			   n <- freshM
>			   return $ Leaf (x,n)
> mlabelM (Node l r) = do l' <- mlabelM l
>			  r' <- mlabelM r
>			  return $ Node l' r'

> initM = M 0 empty

> tree = Node  (Node (Leaf 'a') (Leaf 'b'))
>		(Node (Leaf 'a') (Leaf 'c'))







