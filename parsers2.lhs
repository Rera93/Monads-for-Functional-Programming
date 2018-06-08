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

> repeat' :: Parse a -> Parse [a] 
> repeat' p = (p `bind` \x -> repeat' p `bind` \xs -> result (x:xs)) +++ result []

> isword :: Parse String
> isword = isletter `bind` \letter -> repeat' isletter `bind` \letters -> result (letter : letters) 

> data Operator = Multi | Div | Plus | Minus | Equal | NotEqual | GreaterThan | GreaterThanOrEqual | LessThan | LessThanOrEqual deriving (Show) 

> data Condition = Condition String Operator String deriving (Show)

> data AST = DeclarationInt String Int 
>          | DeclarationString String String
>          | Print String
>          | Get String
>          | AssignmentVar String String
>          | AssignmentOp String String Operator String 
>          | WhileLoop Condition [AST] deriving (Show)

> parse_print :: Parse AST 
> parse_print = is_print `bind` \print -> singleChar ' ' `bind` \space -> singleChar '(' `bind` \open -> isword `bind` \var -> singleChar ')' `bind` \close -> result (Print var) 

> parse_get :: Parse AST
> parse_get  = is_get `bind` \get -> singleChar ' ' `bind` \space -> singleChar '(' `bind` \open -> isword `bind` \var -> singleChar ')' `bind` \close -> result (Get var) 

> is_print :: Parse String
> is_print = singleChar 'p' `bind` \p -> singleChar 'r' `bind` \r -> singleChar 'i' `bind` \i -> singleChar 'n' `bind` \n -> singleChar 't' `bind` \t -> result [p, r, i, n, t]

> is_get :: Parse String
> is_get  = singleChar 'g' `bind` \g -> singleChar 'e' `bind` \e -> singleChar 't' `bind` \t -> result [g, e, t] 

> my_parser :: Parse AST
> my_parser  = parse_print +++ parse_get +++ parse_declaration_int +++ parse_declaration_string +++ parse_assignment_op +++ parse_assignment_var +++ parse_while_loop

> is_integer :: Parse String 
> is_integer  = singleChar 'i' `bind` \i -> singleChar 'n' `bind` \n -> singleChar 't' `bind` \t -> singleChar 'e' `bind` \e -> singleChar 'g' `bind` \g -> singleChar 'e' `bind` \e1 -> singleChar 'r' `bind` \r -> result [i, n, t, e, g, e1, r]

> is_number :: Parse String
> is_number  = ne_number +++ result ""
>  where ne_number = isdigit `bind` \x -> is_number `bind` \xs -> result (x:xs)

> parse_declaration_int :: Parse AST 
> parse_declaration_int = is_integer `bind` \integer -> singleChar ' ' `bind` \space -> isword `bind` \id -> singleChar ' ' `bind` \space1 -> singleChar '=' `bind` \eq -> singleChar ' ' `bind` \space2 -> is_number `bind` \num -> result (DeclarationInt id (read num :: Int) )

> is_string :: Parse String 
> is_string  = singleChar 'S' `bind` \s -> singleChar 't' `bind` \t -> singleChar 'r' `bind` \r -> singleChar 'i' `bind` \i -> singleChar 'n' `bind` \n -> singleChar 'g' `bind` \g -> result [s, t, r, i, n, g]  

> is_any_char :: Parse Char 
> is_any_char  = satisfy (\a -> (a >= ' ' && a <= '~') && a /= '\\' && a /= '\"' && a /= '\'' )

> is_any_string :: Parse String 
> is_any_string  = ne_string +++ result ""
>  where ne_string = is_any_char `bind` \x -> is_any_string `bind` \xs -> result (x:xs)

> parse_declaration_string :: Parse AST 
> parse_declaration_string = is_string `bind` \string -> singleChar ' ' `bind` \space -> isword `bind` \id -> singleChar ' ' `bind` \space1 -> singleChar '=' `bind` \eq -> singleChar ' ' `bind` \space2 -> singleChar '\'' `bind` \startQuote -> isword `bind` \word -> singleChar '\'' `bind` \endQuoute -> result (DeclarationString id (startQuote : word ++ [endQuoute]) )

> is_space_or_not :: Parse Char
> is_space_or_not  = singleChar ' ' +++ zero 

> parse_assignment_var :: Parse AST
> parse_assignment_var  = isword `bind` \leftvar -> is_space_or_not `bind` \space -> singleChar ':' `bind` \colon -> singleChar '=' `bind` \eq -> is_space_or_not `bind` \space1 -> isword `bind` \rightvar -> result (AssignmentVar leftvar rightvar) 

> is_arith_op :: Parse Operator 
> is_arith_op  =  is_multi_op +++ is_div_op +++ is_plus_op +++ is_minus_op +++ is_greaterthan_eq_op +++ is_greaterthan_op +++ is_lessthan_eq_op +++ is_lessthan_op +++ is_eq_op +++ is_not_eq_op

> is_multi_op :: Parse Operator
> is_multi_op  = singleChar '*' `bind` \multi -> result Multi

> is_div_op :: Parse Operator
> is_div_op = singleChar '/' `bind` \div -> result Div

> is_plus_op :: Parse Operator
> is_plus_op  = singleChar '+' `bind` \plus -> result Plus

> is_minus_op :: Parse Operator
> is_minus_op  = singleChar '-' `bind` \minus -> result Minus

> is_greaterthan_op :: Parse Operator 
> is_greaterthan_op  = singleChar '>' `bind` \greater -> result GreaterThan

> is_greaterthan_eq_op :: Parse Operator 
> is_greaterthan_eq_op  = tokenize ">=" `bind` \eq -> result GreaterThanOrEqual

> is_lessthan_op :: Parse Operator 
> is_lessthan_op  = singleChar '<' `bind` \less -> result LessThan

> is_lessthan_eq_op :: Parse Operator 
> is_lessthan_eq_op  =  tokenize "<=" `bind` \eq -> result LessThanOrEqual

> is_eq_op :: Parse Operator
> is_eq_op  = tokenize "==" `bind` \eq2 -> result Equal

> is_not_eq_op :: Parse Operator
> is_not_eq_op  = tokenize "!=" `bind` \eq -> result NotEqual

> parse_assignment_op :: Parse AST
> parse_assignment_op  = isword `bind` \leftvar -> tokenize " := " `bind` \eq -> isword `bind` \leftopvar -> singleChar ' ' `bind` \space2 -> is_arith_op `bind` \arithop -> singleChar ' ' `bind` \space3 -> isword `bind` \rightopvar -> result (AssignmentOp leftvar leftopvar arithop rightopvar)  

> parse_while_loop :: Parse AST
> parse_while_loop  = tokenize "while" `bind` \while -> singleChar '(' `bind` \open -> is_loop_condition `bind` \condition -> singleChar ')' `bind` \close -> tokenize " do " `bind` \opendo -> my_parser `bind` \body -> tokenize " od" `bind` \closedo -> result (WhileLoop condition [body])

> is_loop_condition :: Parse Condition
> is_loop_condition  = isword `bind` \leftside -> singleChar ' ' `bind` \space1 -> is_arith_op `bind` \operator -> singleChar ' ' `bind` \space2 -> isword `bind` \rightside -> result (Condition leftside operator rightside)

> tokenize :: String -> Parse String
> tokenize []          = result []
> tokenize (inp: inps) = singleChar inp `bind` \first-> tokenize inps `bind` \rest -> result (first : rest)

 type Monad a = Status -> (a, Status)
 type Status  = Int 

 my_eval                              :: AST -> Monad ()  
 my_eval DeclarationString name value  =  