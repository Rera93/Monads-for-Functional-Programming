> data Exceptions e a = Ok a | Exception e deriving (Show)

> instance Monad (Exceptions e) where 
>   return            = Ok 
>   Exception l >>= _ = Exception l 
>   Ok r        >>= k = k r

> throw :: e -> Exceptions e a 
> throw = Exception 