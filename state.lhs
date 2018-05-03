> import Data.Monoid

> isBigGang :: Int -> (Bool, String) 
> isBigGang x = (x > 9, "Compared gang size to 9.")

> applyLog :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
> applyLog (x, log) f = let (y, newLog) = f x in (y, log `mappend` newLog)

> type Drink = String 
> type Price = Sum Int

> addDrink :: Drink -> (Drink, Price)
> addDrink "beer" = ("bier", Sum 4)
> addDrink "wine" = ("wijn", Sum 3)
> addDrink _      = ("water", Sum 1)