> data Request = ReadFile   Name
>              | WriteFile  Name String 
>              | AppendFile Name String 
>              | DeleteFile Name 


> data Response = Success 
>               | Str String 
>               | Failure IOError

> type Name = String 

> type Behavior = [Response] -> [Request]
> type FailCont = IOError    -> Behavior
> type StrCont  = String     -> Behavior

> readFile :: Name -> FailCont -> StrCont -> Behavior
> readFile name fail succ ~(resp:resps) = ReadFile name : 
>                                          case resp of 
>                                            Str val     -> succ val resps 
>                                            Failure msg -> fail msg resps                  


            
    