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
> p +++ q = \input -> case p input of
>                         []                -> q input
>                         [(parsed, left_to_parse)] ->  [(parsed, left_to_parse)]     


> (+++++)    :: Parse a -> Parse a -> Parse a
> p +++++ q = \input -> (p input ++ q input) 

> islower_r :: Parse Char
> islower_r  = singleChar 'r' +++ islowercase

> (++++)     :: Parse a -> Parse a -> Parse a
> p ++++ q  = first' (p +++ q)

> first' :: Parse a -> Parse a 
> first' p = \input -> case p input of 
>                           []     -> []
>                           (x:xs) -> [x]

> isletter :: Parse Char
> isletter  = isuppercase +++ islowercase

> isword :: Parse String 
> isword  = neWord +++ result ""
>   where neWord = isletter `bind` \x -> isword `bind` \xs -> result (x:xs)

> data AST = DeclarationInt String Int 
>          | Print String
>          | Get String deriving (Show)

> parse_print :: Parse AST 
> parse_print = is_print `bind` \print -> singleChar ' ' `bind` \space -> singleChar '(' `bind` \open -> isword `bind` \var -> singleChar ')' `bind` \close -> result (Print var) 

> parse_get :: Parse AST
> parse_get  = is_get `bind` \get -> singleChar ' ' `bind` \space -> singleChar '(' `bind` \open -> isword `bind` \var -> singleChar ')' `bind` \close -> result (Get var) 

> is_print :: Parse String
> is_print = singleChar 'p' `bind` \p -> singleChar 'r' `bind` \r -> singleChar 'i' `bind` \i -> singleChar 'n' `bind` \n -> singleChar 't' `bind` \t -> result [p, r, i, n, t]

> is_get :: Parse String
> is_get  = singleChar 'g' `bind` \g -> singleChar 'e' `bind` \e -> singleChar 't' `bind` \t -> result [g, e, t] 

> my_parser :: Parse AST
> my_parser  = parse_print +++ parse_get

> is_integer :: Parse String 
> is_integer  = singleChar 'i' `bind` \i -> singleChar 'n' `bind` \n -> singleChar 't' `bind` \t -> singleChar 'e' `bind` \e -> singleChar 'g' `bind` \g -> singleChar 'e' `bind` \e1 -> singleChar 'r' `bind` \r -> result [i, n, t, e, g, e1, r]

> is_number :: Parse String
> is_number  = ne_number +++ result ""
>  where ne_number = isdigit `bind` \x -> is_number `bind` \xs -> result (x:xs)

> parse_declaration_int :: Parse AST 
> parse_declaration_int = is_integer `bind` \integer -> singleChar ' ' `bind` \space -> isword `bind` \id -> singleChar ' ' `bind` \space1 -> singleChar '=' `bind` \eq -> singleChar ' ' `bind` \space2 -> is_number `bind` \num -> result (DeclarationInt id (read num :: Int) )