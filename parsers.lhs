 {-# LANGUAGE UnicodeSyntax #-}

> type M a = State -> [(a, State)]
> type State = String 

> item          :: M Char 
> item []       = []
> item (x : xs) = [(x, xs)]

Monadic Operations

> unit     :: a -> M a
> unit a x  = [(a, x)]

> (?)       :: M a -> (a -> M b) -> M b 
> (m ? k) x  = [(b, z) | (a, y) <- m x, (b, z) <- k a y]    

> zero   :: M a
> zero x  = []

> (⊕)        :: M a -> M a -> M a 
> (m ⊕ n) x   = m x ++ n x  

> parseTwoItems :: M (Char, Char)
> parseTwoItems  = item ? \a -> item ? \b -> unit (a,b)

> parseOneItem :: M Char 
> parseOneItem  = item ? \a -> unit a

> parseThreeItems :: M (Char, Char, Char)
> parseThreeItems  = item ? \a -> item ? \b -> item ? \c -> unit (a, b, c)

> parseOneOrTwoItems :: M String 
> parseOneOrTwoItems  = (item ? \a -> unit [a]) ⊕ (item ? \a -> item ? \b -> unit [a,b])

A parser can be filtered by combining it with a predicate 

> (▷)     :: M a -> (a -> Bool) -> M a 
> m ▷ p  = m ? \a -> if p a then unit a else zero

hasLiteral recognizes a single specified character

> hasLiteral   :: Char -> M Char 
> hasLiteral c  = item ▷ (\a -> a == c)  


