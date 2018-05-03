> half x = (x `div` 2, "I just halved " ++ (show x) ++ "!")
> halfTwice x :: (val2, log1 ++ log2)
>     where (val1, log1) = half x
>           (val2, log2) = half val1

> data Writer w a = Writer { runWriter :: (a, w) } 