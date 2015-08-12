---
title: Homework #2, Due Sunday, January 30th
---

This week's homework is presented as a literate Haskell file,
just like the lectures.
This means that every line beginning with ">" is interpreted as
Haskell code by the compiler, while every other line is ignored.
(Think of this as the comments and code being reversed from what
they usually are.)
You can load this file into `ghci` and compile it with `ghc`
just like any other Haskell file, so long as you remember to save
it with a `.lhs` suffix.

To complete this homework, download [this file as plain text](hw2.new.lhs) and
answer each question, filling in code where
noted (some questions ask for explanations in addition to or instead
of code).
Your code must typecheck against the given type signatures.
Feel free to add your own tests to this file to exercise the functions
you write.  Submit your homework by sending this file, filled in
appropriately, to `cse230@goto.ucsd.edu` with the subject "HW2"; you
will receive a confirmation email after submitting.  Please note that
this address is unmonitored; if you have any questions about the
assignment, email Pat at `prondon@cs.ucsd.edu`.

This homework requires the graphics libraries from
The Haskell School of Expression:

> import Animation hiding (planets, translate)
> import Picture
> import Control.Applicative

Part 0: All About You
---------------------

Tell us your name, email and student ID, by replacing the respective
strings below

> myName  = "Akshay Balsubramani"
> myEmail = "abalsubr@cs.ucsd.edu"
> mySID   = "A50057955"


Part 1: All About `foldl`
-------------------------

Define the following functions by filling in the "error" portion:

1. Describe `foldl` and give an implementation:
foldl applies recursively the function on every element of the list by folding from the left. 
This means that at the first call the base value and the first element of the list are given 
as input to the function, then this output and the second element are used and so on.


> list1 = [1,2,3,4,5,6]
> list2 = ['a','b','c','d','e']

> myFoldl :: (a -> b -> a) -> a -> [b] -> a
> myFoldl f b [] = b
> myFoldl f b (x:xs) = myFoldl f (b `f` x) xs

2. Using `foldl`, define the list reverse function:

> myReverse :: [a] -> [a]
> myReverse [] = []
> myReverse xs = foldl (\list x -> x:list) [] xs

3. Define `foldr` in terms of `foldl`:

> myFoldr :: (a -> b -> b) -> b -> [a] -> b
> myFoldr f b [] = b
> myFoldr f b xs = foldl (\base xn -> xn `f` base) b (reverse xs)


4. Define `foldl` in terms of `foldr`:

> myFoldl2 :: (a -> b -> a) -> a -> [b] -> a
> myFoldl2 f b [] = b
> myFoldl2 f b xs = foldr (\x1 base -> base `f` x1) b (reverse xs)	

5. Try applying `foldl` to a gigantic list. Why is it so slow?
   Try using `foldl'` instead; can you explain why it's faster?

The function foldl is very slow because it uses lazy evaluation; in other words, it does not apply the input function f to anything at all until the whole list's arguments are expanded out in one large thunk as an intermediate step. In contrast, foldl' uses strict evaluation, so it evaluates f sequentially as soon as it can. Therefore, it does not have to work with a massive thunk in memory before evaluating everything, simply a constant-sized intermediate calculation.


Part 2: Binary Search Trees
---------------------------

Suppose we have the following type of binary search trees:

> data BST k v = NEmpty 
>              | Bind k v (BST k v) (BST k v) 
>              deriving (Show)

Define a `delete` function for BSTs of this type:

> delete :: (Ord k) => k -> BST k v -> BST k v
> delete k NEmpty = NEmpty
> 
> delete k (Bind k' v NEmpty NEmpty)
>	| k == k' = NEmpty
> delete k (Bind k' v l NEmpty) 
>	| k == k' = l
> delete k (Bind k' v NEmpty r)
>	| k == k' = r 
> delete k (Bind k' v' l r) 			    
>	| k == k' = (\(Bind km vm l' r') -> Bind km vm l (delete km r)) (findmin r)
>	| k /= k' = Bind k' v' (delete k l) (delete k r)
>
>
> findmin (Bind k v NEmpty NEmpty) = Bind k v NEmpty NEmpty
> findmin (Bind k v l r) = findmin l



Part 3: Animation
-----------------

This part of the homework constructs an animation of a model
solar system.
We begin by defining various helpful functions:

> translate :: (Float, Float) -> Picture -> Picture
> translate v p =
>     case p of
>       Region c r -> Region c (Translate v r)
>       p1 `Over` p2 -> (translate v p1) `Over` (translate v p2)
>       EmptyPic -> EmptyPic

> -- Translate a picture behavior by a given vector behavior
> translateB :: (Behavior Float, Behavior Float) -> Behavior Picture -> Behavior Picture
> translateB (x,y) p = lift2 translate (zipB (x,y)) p

> -- Convert a pair of behaviors into a pair behavior
> zipB :: (Behavior a, Behavior b) -> Behavior (a,b)
> zipB (Beh b1, Beh b2) = Beh (\t -> (b1 t, b2 t))

> -- Construct a circle behavior
> circ :: Behavior Float -> Behavior Shape
> circ r = ell r r


-- Resize a planet

> class Scalable a where
> 	scale :: Float -> a -> a

> instance Scalable Region where
> 	scale fac (Shape r) = Scale (fac, fac) (Shape r)
>	scale fac (Translate (v1, v2) r) = Translate (v1*fac, v2*fac) (scale fac r)
>	scale fac (Scale (v1, v2) r) = Scale (v1*fac, v2*fac) r
>	scale fac (Complement r) = Complement (scale fac r)
>	scale fac Empty = Empty

> instance Scalable Picture where
> 	scale x (Region c r)    = Region c (scale x r)
> 	scale x (p1 `Over` p2)  = scale x p1 `Over` scale x p2
> 	scale x EmptyPic        = EmptyPic

> scaleb :: Behavior Float -> Behavior Picture -> Behavior Picture
> scaleb = lift2 scale

> sun :: Behavior Picture
> sun = reg (lift0 Yellow) (shape (circ 1))
> mercury :: Behavior Picture
> mercury = reg (lift0 Red) (shape (circ 0.2))
> earth :: Behavior Picture
> earth = reg (lift0 Blue) (shape (circ 0.3))
> moon :: Behavior Picture
> moon = reg (lift0 Green) (shape (circ 0.1))

The following define the main action of the solar system simulator.
You'll want to replace the right-hand side of `planets` with your
solar system.

> planets :: Behavior Picture
> planets = orbit (orbit moon earth 5.0 0.4 0.2 0.8) sun 1.0 2.0 0.5 0.5

> main :: IO()
> main = 
>   do animateB "Solar system" planets

Before starting the exercise proper, let's make our lives easier.
You can avoid a lot of tedious "liftn" operations in your code if you
make the Behavior type a member of the Applicative typeclass. This may
require providing additional definitions not explicitly mentioned
here. You should verify that your definition of the applicative
instance has the required properties (but don't need to turn in a
proof).

> instance Functor Behavior where
>  fmap f (Beh c)	= Beh (\t -> f (c t)) 
> 
> instance Applicative Behavior where
>  pure x                 = lift0 x
>  (<*>) (Beh ab) (Beh a) = Beh(\t -> (ab t) (a t))

Next, use the provided function translateB to write a function



> orbit :: Behavior Picture -- the satellite
>       -> Behavior Picture -- the fixed body
>       -> Float            -- the frequency of the orbit
>       -> Float            -- the x-radius of the orbit
>       -> Float            -- the y-radius of the orbit
>	-> Float	    -- the max shrinkage factor (a number < 1; satellite will shrink by this much when furthest away, and grow by (1/this)when closest to viewer)
>       -> Behavior Picture
> orbit m s freq x y f = let xRadi = lift0 x*cos(time* lift0 freq)
>			     yRadi = lift0 y*sin(time* lift0 freq)
>		       in cond ( 0.0 >* sin (time* lift0 freq))
> 			 ( (translateB (xRadi, yRadi) (scaleb ((lift0 1) + ((lift0 $ 1/f - 1)*(-1 * sin(time* lift0 freq)))) m)) `over` s) 
> 			 (s `over` (translateB (xRadi, yRadi) (scaleb ((lift0 1) + ((lift0 $ 1-f)*(-1 * sin(time* lift0 freq)))) m)) )

that takes two picture behaviors and makes the first orbit around the
second at the specified distance and with the specified radii. That
is, the two pictures will be overlayed (using `over`) and, at each time
$t$, the position of the satellite will be translated by
$xradius * cos(t * frequency)$ in the $x$ dimension and by
$yradius * sin(t * frequency)$ in the $y$ dimension.

Test your function by creating another circle, `mercury`, colored red
and with radius `0.1`, and making it orbit around the sun with a
frequency of `2.0`, and with radii of `2.0` and `0.2` in the x and y axes,
respectively.

A problem you might have noticed is the overlay behavior of
planets. For this part modify orbit to put planets over or under each
other. Hint: you might find the lifted conditional `cond` from SOE
useful for this part.

Modify your functions (and write any support functions that you find
necessary) to make the orbital distances and planet sizes shrink and
grow by some factor (you can pass this factor as parameter to the
orbit function), according to how far the planets are from the
observer. For example, the earth and moon should look a little smaller
when they are going behind the sun, and the orbital distance of the
moon from the earth should be less.

Choose the scaling factor so that the solar system simulation looks
good to you.

*Optional:* Add some other planets, perhaps with their own moons. If
you like, feel free to adjust the parameters we gave above to suit
your own aesthetic or astronomical tastes. Make sure, though, that the
features requested in previous parts --- growing, shrinking,
occlusion, etc. --- remain clearly visible.


