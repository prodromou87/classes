import SOE

-- Vasiliki Papavasileiou
-- CSE 230, Winter 2011
-- A50059057
-- Partner: Akshay Balsubramani


-- *********** Part 1 ************

-- Part 1 : Exercise 1
data Shape = Rectangle  Float Float
	   | Ellipse    Float Float
	   | RtTriangle Float Float
	   | Polygon    [(Float, Float)]
	   deriving (Show)

area :: Shape -> Float
area (Rectangle l b)  = l*b 
area (RtTriangle b h) = b*h/2
area (Ellipse r1 r2)  = pi*r1*r2

-- Part 1 : Exercise 2
rectangle :: Float -> Float -> Shape
rectangle l b = Polygon [(0,0) , (0,b) , (l,0) , (l,b)]

rtTriangle :: Float -> Float -> Shape
rtTriangle b h = Polygon [(0,0) , (b,0) , (0,h)]

-- Part 1 : Exercise 3
sides :: Shape -> Integer
sides (Ellipse v1 v2) = 42
sides (Polygon [(v1,v2),(v3,v4)])  = 0
sides (Polygon [])    = 0
sides (Polygon [v4]) = 0

-- Part 1 : Exercise 4
bigger :: Shape -> Float -> Shape
bigger (Rectangle l b) f  = Rectangle (l+(l*f)) (b+(b*f)) 
bigger (Ellipse r1 r2) f = Ellipse (r1+(r1*f)) (r2+(r2*f))
bigger (RtTriangle b h) f = RtTriangle (b+(b*f)) (h+(h*f))
bigger (Polygon xs) f = Polygon (increase xs f)
			where increase :: [(Float, Float)] -> Float -> [(Float, Float)] 
			      increase [] f = []
			      increase [(x1,y1)] f = [(x1+(x1*f),y1+(y1*f))]
			      increase ((x1,y1):xs) f = (x1+(x1*f),y1+(y1*f)):(increase xs f)

-- Part 1: Exercise 5
hanoi :: Int -> String -> String -> String -> IO ()
hanoi n a b c = do
		      printStr (n-1) a c
		      putStr "Move disc from a to b\n"
		      printStr (n-1) c b
		      where printStr :: Int -> String -> String -> IO ()
		      	    printStr 0 a b = putStr ""
			    printStr n a b = putStr "Move disc from " >> putStr a >> putStr " to "  >> putStr b >> putStr "\n" >> printStr (n-1) a b 

-- *********** Part 2 ************

-- Part 2 : Exercise 1

fillCarpet :: Window -> Int -> Int -> Int -> IO ()
fillCarpet w x y size = drawInWindow w (withColor Blue
				    (polygon [(x,y) , (x+size,y), (x+size,y-size), (x,y-size) , (x,y)]))

minSize2 :: Int
minSize2 = 1

sierpinskiCarpet :: Window -> Int -> Int -> Int -> IO ()
sierpinskiCarpet w x y size = if size <= minSize2
			   then fillCarpet w x y size
			   else let size2 = size `div` 3
			   in do 
				sierpinskiCarpet w x y size2				
				sierpinskiCarpet w (x+size2) y size2
				sierpinskiCarpet w (x+(2*size2)) y size2
					
				sierpinskiCarpet w x (y-size2) size2
				sierpinskiCarpet w x (y-(2*size2)) size2

				sierpinskiCarpet w (x+size2) (y-(2*size2)) size2
				sierpinskiCarpet w (x+(2*size2))  (y-(2*size2)) size2
				sierpinskiCarpet w (x+(2*size2))  (y-size2) size2


			
main1 = runGraphics(
	do 
	   w <- openWindow "Sierpinski Carpet" (400,400) 
	   sierpinskiCarpet w 50 300 256
	   k <- getKey w
	   closeWindow w
	)
 

-- Part 2 : Exercise 2

fillEqui :: Window -> Int -> Int -> Int -> IO ()
fillEqui w x y size = drawInWindow w (withColor Blue
				   (polygon [(x,y) , (x+size,y), (x+(size`div` 2) ,y-size) , (x,y)]))

minSize3 :: Int
minSize3 = 3

vickyCarpet :: Window -> Int -> Int -> Int -> IO ()
vickyCarpet w x y size = if size <= minSize3
			   then fillEqui w x y size
			   else let size2 = size `div` 3
			   in do 
				vickyCarpet w (x+(2*size2)) (y-size2) size2
				vickyCarpet w x (y-size2) size2
				vickyCarpet w (x+size2) y size2 				
			
			
main2 = runGraphics(
	do 
	   w <- openWindow "Vicky Carpet" (600,600) 
	   vickyCarpet w 50 300 512
	   k <- getKey w
	   closeWindow w
	)

