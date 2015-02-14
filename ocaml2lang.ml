#require "batteries";;
#require "compiler-libs.common";;

#load "parse_ocaml.cmo";;
#load "object_lingua.cmo";;

open Parse_ocaml;;
open Object_lingua;;


let rec ser i l = 
        if i  < 2 then  l |> String.concat "\n\n" |> print_endline 
        else let rep = (((i - 1) |> string_of_int )  ^ " à " ^ (string_of_int i)) in
                 ser (i-1) (rep::l)
