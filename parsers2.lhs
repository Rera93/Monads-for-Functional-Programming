> {-# LANGUAGE InstaceSigs #-}
> import Data.Char (isSpace, isDigit, ord)
> import Control.Monad (guard)
> import Control.Monad.Trans.State.Strict
> import Control.Applicative (Alternative(..))


> newtype Parser p = Parser (State -> [(p, State)])
> type State = String

> parseOneItem :: Parser Char
> parseOneItem = (\items -> case items of
>                   []             -> []
>                   (item : items) -> [(item, items)])

