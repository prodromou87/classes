-- CIS552, Advanced Programming
-- Lecture 9: Monadic evaluators and monad transformers

-- Based on chapter 10 of Bird's book, with a few minor changes by BCP
-- for style and flow.

module Main where

------------------------------------------------------------------------------
-- Some simple evaluators

-- A very simple abstract syntax tree for terms
data Term = 
     Con Int         -- constant
   | Div Term Term   -- division
  deriving Show

-- A straightforward recursive evaluator for terms
eval :: Term -> Int
eval (Con x) = x
eval (Div t u) = (eval t) `div` (eval u)

-- Two sample terms that we'll use for the rest of the lecture -- one that
-- evaluates normally and returns an interesting answer, and one that
-- fails with a divide-by-zero exception.
answer, wrong :: Term
answer = Div (Div (Con 1972) (Con 2)) (Con 23)
wrong  = Div (Con 2) (Div (Con 1) (Con 0)) 

-- main = print $ eval answer
--   ==> 
--   42

-- main = print $ eval wrong
--   ==> 
--   divide by zero

------------------------------
-- An evaluator with exceptions

-- Let's extend the basic evaluator above so that, instead of crashing
-- the whole process when a division by zero is encountered, it
-- returns either a normal result or a data structure describing the
-- exception.

type Exception = String
data Exc a = 
       Raise Exception 
     | Return a    -- N.b.: Not to be confused with the monadic
                   -- 'return' operation (which is easy to do when the
                   -- two are used together, below)!
  deriving Show

evalEx :: Term -> Exc Int
evalEx (Con x)   = Return x
evalEx (Div t u) = case evalEx t of
                     Raise e  -> Raise e
                     Return x -> 
                       case evalEx u of
                         Raise e' -> Raise e'
                         Return y -> if y==0 
                                       then Raise "Divide by 0!"
                                       else Return (x `div` y)

-- main = print $ evalEx wrong
--   ==> 
--   Raise "Divide by 0!"

------------------------------
-- An evaluator with state

-- Now let's extend the original evaluator in a different way, so that
-- it counts the number of divisions that it performs while evaluating
-- an expression.

type State = Int
newtype St a = MkSt (State -> (a,State))

instance Show a => Show (St a) where
  show f = "value: " ++ show x ++ ", count: " ++ show s
           where (x,s) = apply f 0

apply :: St a -> State -> (a,State)
apply (MkSt f) s = f s

evalSt :: Term -> St Int
evalSt (Con x)   = MkSt $ \s -> (x,s)
evalSt (Div t u) = MkSt $ \s -> 
                             let (x,s') = apply (evalSt t) s in
                             let (y,s'') = apply (evalSt u) s' in
                             (x `div` y, s''+1)

-- main = print $ evalSt answer
--   ==> 
--   value: 42, count: 2

------------------------------
-- An evaluator with output 

-- Finally, let's extend the original evaluator one last time to
-- incorporate a notion of "output" -- generating a trace of each
-- operation that is performed (in the form of a string that is
-- returned along with the final result).

type Output = String
newtype Out a = MkOut (Output,a)

instance Show a => Show (Out a) where
  show (MkOut (ox,x)) = ox ++ "value: " ++ show x

line :: Term -> Int -> Output
line t x = "term: " ++ show t ++ ", yields " ++ show x ++ "\n"

evalOut :: Term -> Out Int
evalOut (Con x)     = MkOut (line (Con x) x, x)
evalOut v@(Div t u) = let MkOut (ox,x) = evalOut t in
                      let MkOut (oy,y) = evalOut u in
                      let z = x `div` y in
                      MkOut (ox ++ oy ++ line v z, z)

-- main = print $ evalOut answer
--   ==>
--   term: Con 1972, yields 1972
--   term: Con 2, yields 2
--   term: Div (Con 1972) (Con 2), yields 986
--   term: Con 23, yields 23
--   term: Div (Div (Con 1972) (Con 2)) (Con 23), yields 42
--   value: 42

------------------------------------------------------------------------------
-- Monadic evaluators

-- These four evaluators all have a very similar basic structure,
-- but the way they look is very different, because each has to do
-- somewhat different plumbing.  This is a shame.
--
-- Monads to the rescue...

------------------------------
-- A MONADIC evaluator

-- Here is the basic evaluator again, rewritten in a monadic style.
evalM :: Monad m => Term -> m Int
evalM (Con x) = return x
evalM (Div t u) = do x <- evalM t
                     y <- evalM u
                     return (x `div` y)

-- Note that it works fine over ANY monad (because it doesn't actually
-- use any operations that would be specific to a particular monad,
-- unlike the evaluators below).  We can even instantiate it with the
-- trivial IDENTITY monad:
newtype Id a = MkId a

instance Monad Id where
  return x       = MkId x
  (MkId x) >>= q = q x

instance Show a => Show (Id a) where
  show (MkId x) = "value: " ++ show x

evalMId :: Term -> Id Int
evalMId = evalM

-- main = print $ evalMId answer
--    ==> 
--    value: 42

------------------------------
-- A monadic evaluator with exceptions

-- For a more interesting example, let's rewrite the
-- exception-handling evaluator in a monadic style.  For this, we'll
-- need to begin by making the Exc datatype into a monad, following
-- the pattern for exception monads that we've seen a few times now.
instance Monad Exc where
  return x         = Return x
  (Raise e) >>= q  = Raise e
  (Return x) >>= q = q x
  
-- We define a single special operation 'raise', which yields actions
-- in the exception monad.
raise :: Exception -> Exc a
raise e = Raise e

-- Now here is the evaluator, using exceptions.  Note (1) the absence
-- of plumbing and (2) how similar it is to the basic monadic
-- evaluator above.
evalMEx :: Term -> Exc Int
evalMEx (Con x) = return x
evalMEx (Div t u) = do x <- evalMEx t
                       y <- evalMEx u
                       if y==0 
                         then raise "Divide by 0!"
                         else return (x `div` y)

-- main = print $ evalMEx wrong
--    ==> 
--    Raise "Divide by 0!"

------------------------------
-- A monadic evaluator with exceptions

-- Similarly, we can rewrite the stateful evaluator in a monadic
-- style.  We begin by making the St type constructor into a monad.
instance Monad St where
  return x = MkSt $ \s -> (x,s)
  p >>= q  = MkSt $ \s -> 
                      let (x,s') = apply p s in
                      apply (q x) s'

-- Again, we define just one operation specific to this particular
-- monad.
tick :: St ()
tick = MkSt $ \s -> ((), s+1)

-- Now our evaluator is again very straightforward to write (and very
-- similar to the others).
evalMSt :: Term -> St Int
evalMSt (Con x) = return x
evalMSt (Div t u) = do x <- evalMSt t
                       y <- evalMSt u
                       tick
                       return (x `div` y)

-- main = print $ evalMSt answer
--    ==> 
--    value: 42, count: 2

------------------------------
-- A monadic evaluator with output 

-- Same story.  We first make Out into a monad...
instance Monad Out where
  return x = MkOut ("", x)
  p >>= q = let MkOut (ox,x) = p in
            let MkOut (oy,y) = q x in
            MkOut (ox++oy, y)

-- We define the specific operations that we need on this monad to
-- write the evaluator (again, just one in this simple example).
out :: Output -> Out()
out ox = MkOut (ox,())

-- The evaluator is again simple:
evalMOut :: Term -> Out Int
evalMOut v@(Con x) = do out (line v x)
                        return x
evalMOut v@(Div t u) = do x <- evalMOut t
                          y <- evalMOut u
                          let z = x `div` y
                          out (line v z)
                          return z

-- main = print $ evalMOut answer
--    ==> 
--    term: Con 1972, yields 1972
--    term: Con 2, yields 2
--    term: Div (Con 1972) (Con 2), yields 986
--    term: Con 23, yields 23
--    term: Div (Div (Con 1972) (Con 2)) (Con 23), yields 42
--    value: 42

---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
-- Monad transformers

-- There is one fly in the ointment: We've written three separate
-- evaluators for three separate features -- exceptions, state, and
-- output.  But if we want a single evaluator that handles ALL of
-- these features, we can't just combine bits of the three individual
-- evaluators; we're also going to have to combine all three MONADS
-- into a single complicated monad that supports all three kinds of
-- operations.  This looks like rather hard work!

-- Fortunately, we can do better.  Instead of defining exception
-- handling, state passing, and output as monads, we define them as
-- MONAD TRANSFORMERS -- intuitively, functions from monads to monads!
-- This requires a bit more infrastructure to set up, but once this
-- infrastructure is in place we can mix and match features in a
-- pretty modular way.  For example, we can build a monad that handles
-- both exceptions and state by applying a State monad transformer and
-- then an Exception monad transformer to the identity monad.

------------------------------
-- An evaluator with both state and exceptions

-- Let us begin by seeing how the evaluator itself is going to look.
-- This is the least complex part of the picture, since all we need to
-- do is to USE the features of the combined monad that we are going
-- to build later.  We begin by defining two subclasses of the Monad
-- class...

-- An EXCEPTION MONAD is a monad with an operation raise' that takes a
-- description of an exception and yields a monad action.
class Monad m => ExMonad m where
  raise' :: Exception -> m a      
         -- (We add a ' to the name to avoid clashing with the raise function
         -- defined above)

-- Similarly, a STATE MONAD is a monad with an action tick'.
class Monad m => StMonad m where
  tick' :: m ()

-- Now it is child's play to write an evaluator that both keeps track
-- of division operations and protects against division by zero.
evalTExSt :: (ExMonad m, StMonad m) => Term -> m Int
evalTExSt (Con x) = return x
evalTExSt (Div t u) = do x <- evalTExSt t
                         y <- evalTExSt u
                         tick'
                         if y==0 
                           then raise' "Divide by 0!"
                           else return (x `div` y)

------------------------------
-- Monad transformers

-- A monad transformer is a type operator t that maps each monad m to
-- a monad (t m), equipped with an operation promote that lifts an
-- action x :: m a from the original monad to an action (promote x) ::
-- (t m) a on the monad (t m).
class Transformer t where
  promote :: Monad m => m a -> (t m) a

------------------------------
-- A monad transformer for exceptions

-- The (binary) type constructor EXC ``puts exceptions inside''
-- another monad m
newtype EXC m a = MkEXC (m (Exc a))

-- The 'recover' function just strips off the outer MkEXC constructor,
-- for convenience
recover :: EXC m a -> m (Exc a)
recover (MkEXC g) = g

-- Now the interesting part: If m is a monad, then so is (EXC m)...
instance Monad m => Monad (EXC m) where
  return x = MkEXC (return (Return x))
  p >>= q  = MkEXC (recover p >>= r)
             where r (Raise e)  = return (Raise e) 
                   r (Return x) = recover (q x) 

-- Moreover, (EXC m) is an exception monad, not just a plain one...
instance Monad m => ExMonad (EXC m) where
  raise' e = MkEXC (return (Raise e))

-- Finally, EXC is a monad transformer because we can lift any action
-- in m to an action in (EXC m) by wrapping its result in a 'Return'
-- constructor...
instance Transformer EXC where
  promote g = MkEXC $ do {x<-g; return (Return x)}

-- We can now use the promote operation to show how to make (EXC m)
-- into a state monad whenever m is one, by lifting m's tick'
-- operation to (EXC m).
instance StMonad m => StMonad (EXC m) where
  tick' = promote tick'

------------------------------
-- A monad transformer for states

-- Now repeat the same story for state monads...

newtype STT m a = MkSTT (State -> m (a,State))

apply' :: STT m a -> State -> m (a,State)
apply' (MkSTT f) = f

instance Monad m => Monad (STT m) where
  return x = MkSTT $ \s -> return (x,s)
  p >>= q = MkSTT $
              \s ->
                 do (x,s') <- apply' p s
                    apply' (q x) s'

instance Transformer STT where
  promote g = MkSTT $
                \s ->
                   do { x <- g; return (x,s) }

instance Monad m => StMonad (STT m) where
  tick' = MkSTT $ \s -> return ((), s+1)

instance ExMonad m => ExMonad (STT m) where
  raise' e = promote (raise' e)

------------------------------
-- A quick digression 

-- We will also need a general notion of a "Showable monad"
class Monad m => ShowMonad m where
  showm :: m String -> String

-- Id is a ShowMonad
instance ShowMonad Id where
  showm (MkId cs) = cs

-- (EXC m) is a ShowMonad if m is
instance ShowMonad m => ShowMonad (EXC m) where
  showm p = showm (recover p >>= q)
            where q (Raise e) = return ("exception: " ++ e)
                  q (Return x) = return x

-- (STT m) is a ShowMonad if m is
instance ShowMonad m => ShowMonad (STT m) where
  showm p = showm $ do (x,s) <- apply' p 0
                       return (x ++ ", count: " ++ show s)
               
-- Finally, if m is a ShowMonad and a is a showable type, then (EXC a)
-- and (STT a) are showable.

mapm :: Monad m => (a->b) -> m a -> m b
mapm f p = do { x<-p; return (f x) }

instance (ShowMonad m, Show a) => Show (EXC m a) where
  show = showm . mapm show 

instance (ShowMonad m, Show a) => Show (STT m a) where
  show = showm . mapm show 

------------------------------
-- Putting it all together...

-- Now it is just a matter of assembling the pieces.  Interestingly,
-- though, there are TWO ways to build a monad with exceptions and
-- state:
evalExSt :: Term -> STT (EXC Id) Int
evalExSt = evalTExSt

evalStEx :: Term -> EXC (STT Id) Int
evalStEx = evalTExSt

-- At first glance, it appears they do the same thing:

-- main = print $ evalExSt answer
--   ==>
--   42, count: 2

-- main = print $ evalStEx answer
--   ==>
--   42, count: 2

-- But...!

-- main = print $ evalExSt wrong
--   ==>
--   exception: Divide by 0!

-- main = print $ evalStEx wrong
--   ==>
--   exception: Divide by 0!, count: 1

