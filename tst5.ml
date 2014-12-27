(*let funtst1 a b = a + b*)
let funop1 a b   = "STRfunop1"
let funop2 a b c = "STRfunop2"
(* On cherche à savoir si on peut faire la différence selon la forme de l'AST entre un appel de fonction à n arguments et une suite de code*)
let aaaaaaa = funop2 "PARAM1" "PARAM2" "PARAM3";;
let bbbbbbb = let cccc = funop1 "B_PARAM1" "B_PARAM2" in cccc^"B_PARAM3"
