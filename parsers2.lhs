{-# LANGUAGE UnicodeSyntax #-}

> import Data.Char
> import Data.Ord
> import Data.List

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
> parse_print = tokenize "print(" `bind` \print -> isword `bind` \var -> singleChar ')' `bind` \close -> result (Print var) 

> parse_get :: Parse AST
> parse_get  = tokenize "get(" `bind` \get -> isword `bind` \var -> singleChar ')' `bind` \close -> result (Get var) 

> parse_all :: Parse AST
> parse_all  = parse_print +++ parse_get +++ parse_declaration_int +++ parse_declaration_string +++ parse_assignment_op +++ parse_assignment_var +++ parse_while_loop

> iterate'        :: Parse AST -> Parse [AST] 
> iterate' parser  = (parser `bind` \statement -> tokenize ";" `bind` \semicolon -> iterate' parser `bind` \statements -> result (statement : statements)) +++ result []

> my_parser :: Parse [AST]
> my_parser  = parse_all `bind` \statement -> tokenize ";" `bind` \semicolon -> iterate' parse_all `bind` \statements -> result (statement : statements)

> is_number :: Parse String
> is_number  = ne_number +++ result ""
>  where ne_number = isdigit `bind` \x -> is_number `bind` \xs -> result (x:xs)

> parse_declaration_int :: Parse AST 
> parse_declaration_int = tokenize "Integer " `bind` \integer -> isword `bind` \id -> tokenize " = " `bind` \eq -> is_number `bind` \num -> result (DeclarationInt id (read num :: Int) )

> parse_declaration_string :: Parse AST 
> parse_declaration_string = tokenize "String " `bind` \string -> isword `bind` \id -> tokenize " = " `bind` \eq -> singleChar '\'' `bind` \startQuote -> isword `bind` \word -> singleChar '\'' `bind` \endQuoute -> result (DeclarationString id (startQuote : word ++ [endQuoute]) )

> parse_assignment_var :: Parse AST
> parse_assignment_var  = isword `bind` \leftvar -> tokenize " := " `bind` \eq -> isword `bind` \rightvar -> result (AssignmentVar leftvar rightvar) 

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
> parse_while_loop  = tokenize "while(" `bind` \while -> is_loop_condition `bind` \condition -> singleChar ')' `bind` \close -> tokenize " do " `bind` \opendo -> (my_parser) `bind` \body -> tokenize " od" `bind` \closedo -> result (WhileLoop condition body)

> is_loop_condition :: Parse Condition
> is_loop_condition  = isword `bind` \leftside -> singleChar ' ' `bind` \space1 -> is_arith_op `bind` \operator -> singleChar ' ' `bind` \space2 -> isword `bind` \rightside -> result (Condition leftside operator rightside)

> tokenize :: String -> Parse String
> tokenize []          = result []
> tokenize (inp: inps) = singleChar inp `bind` \first-> tokenize inps `bind` \rest -> result (first : rest)

> type Store = [Variable]
> data Variable = IntVar String Int | StringVar String String deriving (Show, Eq)

> type StateMonad a = Store -> (a, Store)

> returnS         :: a -> StateMonad a 
> returnS a state = (a, state)

> bindS      :: StateMonad a -> (a -> StateMonad b) -> StateMonad b
> m `bindS` f = \state -> let (x, state') = m state in
>                         let m' = f x in
>                             m' state'   

> evaluate :: State -> StateMonad (Exceptions Variable)
> evaluate input = case my_parser input of
>                    []    -> returnS (raise "no parse") 
>                    [(list,_)] -> transform list 
 
> transform        :: [AST] -> StateMonad (Exceptions Variable) 
> transform []     = returnS (raise "success")
> transform (x:xs) = case x of 
>                      (DeclarationString name value)   -> (putInStore (StringVar name value)) `bindS` errorCheck xs
>                      (DeclarationInt name value)      -> (putInStore (IntVar name value)) `bindS` errorCheck xs 
>                      (Print name)                     -> (getFromStore name) `bindS` errorCheck xs
>                      (Get name)                       -> (getFromStore name) `bindS` errorCheck xs
>                      (AssignmentVar leftvar rightvar) -> (modifyStore leftvar rightvar) `bindS` errorCheck xs

> errorCheck :: [AST] -> Exceptions Variable -> StateMonad (Exceptions Variable)
> errorCheck []      = \e -> returnS (e)
> errorCheck astList = \e -> case e of (Raise _)  -> returnS (e) 
>                                      (Return _) -> transform astList 

 transform                                                    :: AST -> StateMonad (Exceptions Variable) 
 transform (DeclarationString name value)                       = putInStore (StringVar name value)
 transform (DeclarationInt name value)                          = putInStore (IntVar name value) 
 transform (Print name)                                         = getFromStore name
 transform (Get name)                                           = getFromStore name
 transform (AssignmentVar leftvar rightvar)                     = modifyStore leftvar rightvar 


 my_eval (AssignmentOp leftvar leftopvar operator rightopvar) =
 my_eval (WhileLoop condition [])                          = 

> modifyStore                    :: String -> String -> StateMonad (Exceptions Variable)
> modifyStore leftname rightname = (getFromStore leftname) `bindS` \leftExists -> case leftExists of
>                                                                                  (Raise _)  -> returnS (raise (leftname ++ " does not exist"))                                                                                 
>                                                                                  (Return l) -> (getFromStore rightname) `bindS` \rightExists -> case rightExists of 
>                                                                                                                                                  (Raise _)  -> returnS (raise (rightname ++ " does not exist"))
>                                                                                                                                                  (Return r) -> (checkTypeCompat l r) `bindS` \compat -> case compat of
>                                                                                                                                                                                                           (Raise e)  -> returnS (Raise e) 
>                                                                                                                                                                                                           (Return _) -> (removeFromStore l) `bindS` \_ -> (assign l r) `bindS` \newVal -> putInStore newVal                

> putInStore    :: Variable -> StateMonad (Exceptions Variable) 
> putInStore var = \store -> case (filter (\v -> (getVarName v) == (getVarName var)) store) of
>                              []     -> returnS (returnE var) (var : store)                               
>                              (x:xs) -> returnS (raise ("variable " ++ (getVarName var) ++ " already exists")) store

> getFromStore      :: String -> StateMonad (Exceptions Variable)
> getFromStore name = getStore `bindS` \store -> case (filter (\v -> getVarName v == name ) store) of 
>                                                    []     -> returnS (raise ("variable " ++ name ++ " does not exist"))
>                                                    (x:xs) -> returnS (returnE x)

> getVarName                 :: Variable -> String
> getVarName (IntVar name _) = name
> getVarName (StringVar name _) = name 

> getVarType                 :: Variable -> String 
> getVarType (IntVar _ _)    = "Integer"
> getVarType (StringVar _ _) = "String"

> assign :: Variable -> Variable -> StateMonad Variable 
> assign (IntVar lname lval) (IntVar rname rval)       = returnS (IntVar lname rval)
> assign (StringVar lname lval) (StringVar rname rval) = returnS (StringVar lname rval)

> removeFromStore    :: Variable -> StateMonad () 
> removeFromStore var = \store -> returnS () (delete var store) 

> checkTypeCompat                  :: Variable -> Variable -> StateMonad (Exceptions ())
> checkTypeCompat varleft varright = if (getVarType varleft) == (getVarType varright) then returnS (returnE ()) else returnS (raise "Incompatible types")

> getStore :: StateMonad [Variable]
> getStore = \store -> returnS store store 

> data Exceptions a = Raise Exception | Return a deriving (Show)
> type Exception = String 

> returnE  :: a -> Exceptions a 
> returnE a = Return a

> bindE      :: Exceptions a -> (a -> Exceptions b) -> Exceptions b
> m `bindE` f = case m of 
>                 Raise e -> Raise e 
>                 Return a -> f a

> raise  :: Exception -> Exceptions a
> raise e = Raise e 

> isword' :: Parse String
> isword'  = word +++ result ""
>               where word = isletter `bind` \x -> isword' `bind` \xs -> result (x:xs) 
