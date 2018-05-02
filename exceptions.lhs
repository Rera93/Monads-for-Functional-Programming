> data Exceptions e a = Ok a | Exception e deriving (Show)

 instance Monad (Exceptions e a) where 
   return            = Ok 
   Exception l >>= _ = Exception l 
   Ok r        >>= k = k r

> throw :: e -> Exceptions e a 
> throw = Exception 

> catch :: Exceptions e a -> (e -> Exceptions e a) -> Exceptions e a
> catch (Exception l) h = h l 
> catch (Ok r)        _ = Ok r 