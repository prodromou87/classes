> import Data.Char
> import Data.Functor
> import Control.Monad

> newtype Parser a = P (String -> [(a, String)])

> doParse (P p) s = p s

> twoChar :: Parser (Char, Char)
> twoChar = P (\cs -> case cs of 
>		    c1:c2:cs' -> [((c1,c2), cs')]
>		    _	      -> [])

> pairP :: Parser a -> Parser b -> Parser (a,b)
> pairP p1 p2 = do x <- p1
>		   y <- p2
>		   return (x,y)

> bindP :: Parser a -> (a -> Parser b) -> Parser b
> bindP p1 fp2 = P $ \cs -> [(y, cs'') | (x, cs')  <- doParse p1 cs
>                                      , (y, cs'') <- doParse (fp2 x) cs']

> returnP :: a -> Parser a
> returnP x = P (\cs -> [(x,cs)])

> instance Monad Parser where
>   (>>=) = bindP
>   return = returnP

> failP = P $ const []

> oneChar :: Parser Char
> oneChar = P (\cs -> case cs of
>		    c:cs' -> [(c,cs')]
>		    _	  -> [])

> satP :: (Char -> Bool) -> Parser Char
> satP p = do c <- oneChar
>	      if p c then return c else failP

> lowercaseP = satP isAsciiLower
> alphaChar = satP isAlpha
> digitChar = satP isDigit

> digitInt = do c <- digitChar
>		return ((read[c])::Int)

> readFour :: Parser String
> readFour = P (\cs -> if length cs > 4 then [(take 4 cs, drop 4 cs)]
>			    else [])

> strP :: String -> Parser String
> strP mystr = do s <- readFour
>		  if p s then return s else failP
>		  where
>		    p s = s == mystr

> dogeP = strP "doge"

> alphaNumChar = alphaChar `chooseP` digitChar

> p1 `chooseP` p2 = P (\cs -> doParse p1 cs ++ doParse p2 cs)

> grabn :: Int -> Parser String
> grabn n 
>   | n <= 0    = return ""
>   | otherwise = do c  <- oneChar  
>                    cs <- grabn (n-1)
>                    return (c:cs)

> grab2or4 = grabn 2 `chooseP` grabn 4

> char c = satP (== c)

> intOp      = plus `chooseP` minus `chooseP` times `chooseP` divide 
>   where 
>     plus   = char '+' >> return (+)
>     minus  = char '-' >> return (-)
>     times  = char '*' >> return (*)
>     divide = char '/' >> return div

> calc = do x <- digitInt 
>	    op <- intOp
>	    y <- digitInt
>	    return $ x `op` y

> string :: String -> Parser String
> string ""	= return ""
> string (c:cs) = do char c
>		     string cs
>		     return (c:cs)

> manyP     :: Parser a -> Parser [a]
> manyP p   = many1 <|> many0 
>   where 
>     many0 = return []
>     many1 = do x  <- p
>                xs <- manyP p
>                return (x:xs)

> (<|>) :: Parser a -> Parser a -> Parser a
> p1 <|> p2 = P $ \cs -> case doParse (p1 `chooseP` p2) cs of
>                          []  -> []
>                          x:_ -> [x]
> 

> firstChooseP = (<|>)

> oneInt :: Parser Int
> oneInt = read `fmap` manyP digitChar

> instance Functor Parser where
>   fmap f p = do x <- p
>		  return (f x)

> calc1'      ::  Parser Int
> calc1'      = oneInt <|> binExp
>   where 
>     binExp = do x <- calc1'
>                 o <- intOp 
>                 y <- oneInt
>                 return $ x `o` y
