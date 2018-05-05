> {-# LANGUAGE InstaceSigs #-}
> import Data.Char (isSpace, isDigit, ord)
> import Control.Monad (guard)
> import Control.Monad.Trans.State.Strict
> import Control.Applicative (Alternative(..))


> newtype Parser state maybe p = Parser (state -> maybe (p, state))
> type state = String

> parseOneItem :: Parser Char
> parseOneItem = (\items -> case items of
>                   []             -> []
>                   (item : items) -> [(item, items)])

