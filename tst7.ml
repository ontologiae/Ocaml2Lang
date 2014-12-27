type expr =
        | Int of int
        | Machin of expr list * expr
        | Truc of ( (expr list ) * expr ) list

