{-# LANGUAGE UnicodeSyntax #-}

> import Data.Char
> import Data.Ord

> type Parse p = State -> [(p, State)]
> type State = String 

> oneItem          :: Parse Char
> oneItem []       = []
> oneItem (l : ls) = [(l, ls)]

> zero :: Parse p
> zero  = \input -> []

> result :: p -> Parse p 
> result v input = [(v, input)]

> bind :: Parse a -> (a -> Parse b) -> Parse b
> p `bind` f = \input -> concat [f v input' | (v, input') <- p input]

> parseOneItem :: Parse Char
> parseOneItem = oneItem `bind` \x -> result x

> parseTwoItems :: Parse (Char, Char)
> parseTwoItems = oneItem `bind` \x -> oneItem `bind` \y -> result(x, y)  

> (>>>)       :: Parse a -> Parse b -> Parse (a, b)
> p >>> q = p `bind` \x -> q `bind` \y -> result(x, y) 

> satisfy      :: (Char -> Bool) -> Parse Char
> satisfy pred = oneItem `bind` \x -> if pred x then result x else zero 

> singleChar  :: Char -> Parse Char 
> singleChar a = satisfy (\b -> a == b)

> isdigit :: Parse Char
> isdigit = satisfy (\a -> a >= '0' && a <= '9') 

> islowercase :: Parse Char 
> islowercase  = satisfy (\a -> a >= 'a' && a <= 'z')

> isuppercase :: Parse Char 
> isuppercase  = satisfy (\a -> a >= 'A' && a <= 'Z')

> parseTwoDigits :: Parse (Char, Char)
> parseTwoDigits  = isdigit `bind` \x -> isdigit `bind` \y -> result (x, y)

> parseTwoChars :: Char -> Char -> Parse (Char, Char)
> parseTwoChars a b = singleChar a `bind` \x -> singleChar b `bind` \y -> result (x, y)

> parseTwoUpperCase :: Parse (Char, Char)
> parseTwoUpperCase  = isuppercase `bind` \x -> isuppercase `bind` \y -> result (x, y) 

> parseTwoLowerCase :: Parse (Char, Char)
> parseTwoLowerCase  = islowercase `bind` \x -> islowercase `bind` \y -> result (x, y) 

> (+++)       :: Parse a -> Parse a -> Parse a
> (p +++ q) input = (p input ++ q input)

> parseOneOrTwoItems :: Parse Char 
> parseOneOrTwoItems  = parseOneItem +++ parseOneItem

> (++++)     :: Parse a -> Parse a -> Parse a
> p ++++ q  = first' (p +++ q)

> first' :: Parse a -> Parse a 
> first' p = \input -> case p input of 
>                           []     -> []
>                           (x:xs) -> [x]


