type expr =
        | Int of int
        | Plus  of expr * expr
        | Moins of expr * expr

let a = Moins(Plus(Int 6, Int 9),Int 5)
