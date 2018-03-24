> import Data.Char

> data Token = Digit | Alpha deriving (Show, Eq)

> tokenize :: String -> [Token]
> tokenize [] = []
> tokenize (c : rest) =
>   if isDigit c then Digit : tokenize rest else Alpha : tokenize rest

> data Operator = Plus | Minus | Times | Div deriving (Show)
> operator :: Char -> Operator
> operator c | c == '+' = Plus 
>            | c == '-' = Minus 
>            | c == '*' = Times 
>            | c == '/' = Div