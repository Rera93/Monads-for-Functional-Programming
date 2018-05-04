
> type Parser p = State -> [(p, State)]
> type State = String

> parseOneItem :: Parser Char
> parseOneItem = (\items -> case items of
>                   []             -> []
>                   (item : items) -> [(item, items)])
