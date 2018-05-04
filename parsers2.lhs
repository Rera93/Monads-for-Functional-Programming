> type State = String
> newtype Parser p = Parser (State -> [(p, State)])
