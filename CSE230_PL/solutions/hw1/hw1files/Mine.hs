module Mine where 

-- Vasiliki Papavasileiou
-- CSE 230, Winter 2011
-- A50059057
-- Partner: Akshay Balsubramani

import XMLTypes

------------------------------------------------------------------------------
-- Warm-up exercises

-- Put the solutions to these exercises here.  For each one, include at least 
-- one test case.  (These test cases don't need to be executed when the program
-- runs -- they should just be available in case someone wanted to look at them
-- from ghci, for example.)

-- *********** Part 3 ************

-- Part 3 : Problem 5.3

my_length :: [a] -> Integer
my_length = foldr (\_ a -> 1 + a) 0

-- Part 3 : Problem 5.4

f1 = (\fs x -> map (\f -> f x) fs) (map (*) [1,2,3,4]) 5

-- Part 3 : Problem 5.5

doubleEach :: [Int] -> [Int]
doubleEach [] 	  = []
doubleEach (x:xs) = (2*x) : doubleEach xs

doubleEach2 :: [Int] -> [Int]
doubleEach2 xs = map (*2) xs

-- doubleEach2 [1,2,3] = [2,4,6]

pairAndOne :: [Int] -> [Int]
pairAndOne [] = []
pairAndOne (x:xs) = (x+1) : pairAndOne xs

pairAndOne2 :: [Int] -> [Int]
pairAndOne2 xs = map (+1) xs

-- pairAndOne2 [1,2,3] = [2,3,4]

addEachPair :: [(Int,Int)] -> [Int]
addEachPair [] = []
addEachPair ((a,b) : xs) = (a+b) : addEachPair xs

addEachPair2 :: [(Int,Int)] -> [Int]
addEachPair2 xs = map (\(a,b) -> a + b) xs

-- addEachPair2 [(1,2),(2,3),(3,4)] = [3,5,7]
 
-- Part 3 : Problem 5.6

maxList (x1:xs) = foldr max x1 xs
minList (x1:xs) = foldr min x1 xs				   

-- maxList [1,2,3] = 3
-- minList [1,2,3] = 1

-- Part 3 : Problem 7.1
data Tree a = Leaf a
	    | Node (Tree a) (Tree a)
	     

treeFold op b (Leaf x)   = b x
treeFold op b (Node l r) = (treeFold op b l) 
                           `op` 
                           (treeFold op b r)

treeHeight = treeFold (\a b -> 1+(max a b)) (const 0)
treeSize   = treeFold (+) (const 1)
fringe =  treeFold (++) (\x -> [x])

--t1 = Node (Node (Leaf 3) (Leaf 4)) (Node ( Leaf 1 ) (Leaf 2))

-- Part 3 :Problem 7.2

data InternalTree a = ILeaf
                    | IBranch a (InternalTree a) (InternalTree a)
                      deriving Show
 
takeTree :: Int -> InternalTree a -> InternalTree a
takeTree 0 t = ILeaf
takeTree n ILeaf = ILeaf
takeTree n (IBranch a l r) = IBranch a (takeTree (n-1) l) (takeTree (n-1) r)
 
takeTreeWhile :: (a -> Bool) -> InternalTree a -> InternalTree a
takeTreeWhile f ILeaf = ILeaf
takeTreeWhile f (IBranch a x y) = if (f a)
                                  then IBranch a (takeTreeWhile f x)
                                                 (takeTreeWhile f y)
                                  else ILeaf


--t2 = IBranch 1 (IBranch 2 ILeaf ILeaf) (IBranch 2 ILeaf ILeaf)

-- Part 3 : Problem 4

doubleEach3 :: [Int] -> [Int]
doubleEach3 xs = map2 (*2) xs

map2 f = foldr ((:) . f) [] 
------------------------------------------------------------------------------
-- Generic functions for transforming XML

-- This function simply takes an Element and changes its tag name, returning the new element.
changeTagName :: SimpleXML -> String -> SimpleXML
changeTagName (Element tag body) newname = Element newname body


-- This function encloses the input tree in a "wrapper" tag with tagname
wrapTree :: SimpleXML -> String -> SimpleXML
wrapTree oldtree newtag = Element newtag [oldtree]


-- This function takes a list of elements and a "separator" element, and returns a list of just their contents, with "separator" elements inserted in between in an alternating fashion
leavesToList :: [SimpleXML] -> SimpleXML -> [SimpleXML]
leavesToList eltlist separator = foldr (++) [] (map flatten eltlist)
				 where flatten (Element tag body) = body ++ [separator]


-- This helper function takes a SimpleXML element representing a heading and several PCDATA "subpoints", along with a "separator" element and a formatting tag for the heading. It returns a list containing the correctly formatted heading and subpoints, with "separator" elements in between all of them.
headAndPts :: SimpleXML -> SimpleXML -> String -> [SimpleXML]
headAndPts separator (Element e ((Element heading tiname):subpts)) formatname = 
		([(Element formatname tiname)] ++ [separator] ++ (leavesToList subpts separator))


-- This function takes an Element representing a heading and several subpoints (each of which is an Element with a similar list as body), along with a list of formatting tags to be applied to headings at the various depths of the tree. The first element of the list is for headings at the shallowest level, the second element for headings at the next deeper level, and so on. It also takes a function f which correctly renders the subpoints at the leaves by returning a correctly rendered SimpleXML list; f takes two arguments, a formatting tag for the lowest headings and an Element containing leaves of such a heading.
-- **WARNING: The formatting tags list is assumed to be nonempty, and also accurate, i.e. its length matches the tree depth as specified.**
-- The output is a flattened HTML-like list with the specified formatting tags applied to the headings reflecting the original deep tree structure.

parseHeadings :: SimpleXML -> [String] -> (SimpleXML -> String -> [SimpleXML]) -> [SimpleXML]
parseHeadings elt (ftag:[]) leafFunc = leafFunc elt ftag
parseHeadings (Element e ((Element heading tiname):subpts)) (firstTag:tags) leafFunc = 
		([(Element firstTag tiname)] ++ (foldr (++) [] (map (\x -> (parseHeadings x tags leafFunc)) subpts))  )



------------------------------------------------------------------------------
-- Formatting plays

-- Your specific play-formatter goes here...

-- This function is headAndPts (see above) defined with separator <br/>
helperFunc :: SimpleXML -> String -> [SimpleXML]
helperFunc a b = headAndPts (Element "br" []) a b


formatPlay (Element play ((Element title    tiname):
			  (Element personae personlist):
			  actlist)) = wrapTree ot "html"
	       			    where ot = Element "body" 
						([(Element "h1" tiname)] ++  
						 [(Element "h2" [PCDATA "Dramatis Personae"])] ++  
						 (leavesToList personlist (Element "br" [])) ++ 
						 (foldr (++) [] (map (\x -> (parseHeadings x ["h2","h3","b"] helperFunc)) actlist))    )

