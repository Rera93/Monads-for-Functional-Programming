> data Request = ReadFile   Name
>              | WriteFile  Name String 
>              | AppendFile Name String 
>              | DeleteFile Name 


> data Response = Success 
>               | Str String 
>               | Failure IOError

> type Name = String 


            
    