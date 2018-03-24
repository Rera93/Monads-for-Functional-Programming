> import Data.Char

> data Token = Digit | Alpha deriving (Show, Eq)

> tokenize :: String -> [Token]
> tokenize [] = []
> tokenize (c : rest) =
>   if isDigit c then Digit : tokenize rest else Alpha : tokenize rest