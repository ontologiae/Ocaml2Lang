
#require "batteries";;
#require "compiler-libs.common";;
module L = BatList;;


open Asttypes
open Typedtree
open Parsetree

let i,ml = Cmt_format.read "tst1.cmt";;

let get_ast s = let structur = BatOption.get s in
                match structur.Cmt_format.cmt_annots with
                | Cmt_format.Implementation st -> st
                | _                            -> failwith "pas d'annotation";;

let ml2 = get_ast ml;;
(L.hd ml2.Typedtree.str_items).Typedtree.str_desc;;

(* let funtst2 a b c = a ^b ^c
 * Tstr_value (Asttypes.Nonrecursive ..
 * |_ Tpat_var ({Ident.stamp = 1008; Ident.name = "funtst2" *
 * |_? Texp_function
 *     |__ Tpat_desc a
 * *)






(*********************
 *
 *
 * AST OCAML SIMPLIFIÉ
 *
 * *******************)


(* Variable names *)
type name = string




type ty =
  | TInt              (* Integers *)
  | TBool             (* Booleans *)
  | TFloat
  | TString
  | Tchar
  | Tlist   of ty
  | Type_variant of name * (name * ty list) list (*Représente un type somme*)
  | TModule of name (*  Falloir réfléchir à une représentation simple. Pour le moment, on le met de côté*)
  | TArrow of ty * ty (* Fonctions *)



(* On ne gère que les opérateurs sur entiers, chaines et listes. TODO Float, Bool*)  
type _ expre =
  | Var                 : name    -> name expre          		(* Variable *)
  | Int                 : int     -> int expre         		(* Non-negative integer constant *)
  | Bool                : bool    -> bool expre        		(* Boolean constant *)
  | Float               : float   -> float expre
  | Char                : char    -> char expre
  | String              : string  -> string expre
  | Sequence            : 'a expre * 'b expre -> ( 'a * 'b ) expre
  | StringConcat        : 'a expre * 'b expre -> ( 'a * 'b ) expre
  | ListConcat          : 'a expre * 'a expre -> ( 'a * 'a ) expre
  | ListAddElem         : 'a expre * 'b expre -> ( 'a * 'b ) expre                             (*Beau trou de typage le 2ème argument est une liste*)
  | Times               : int expre * int expre -> ( int * int ) expre 		               (* Product [e1 * e2] *)
  | Plus                : int expre * int expre -> ( int * int ) expre   		       (* Sum [e1 + e2] *)
  | Minus               : int expre * int expre -> ( int * int ) expre  		       (* Difference [e1 - e2] *)
  | Equal               : 'a expre * 'a expre -> ( 'a * 'a ) expre	                       (* General comparison [e1 = e2] *)
  | Less                : int expre * int expre -> ( int * int ) expre    		       (* Integer comparison [e1 < e2] *)
 (* | TypeConstr          : 
  | Match               : name *) 
  | If                  : bool expre * 'a expre * 'b expre -> (bool * 'a * 'b) expre	       (* Conditional [if e1 then e2 else e3] *)
  | Fun                 : name * name * ty * ty * 'a expre -> (name * name * ty * ty * 'a) expre (* Function [fun f(x:s):t is e] 
                                                                                                  * Si on a une fonction a plusieurs paramètre, elle est réécrite comme une succession de fonctions.
                                                                                                  *)
  | Apply               : 'a expre * 'b expre -> ( 'a * 'b ) expre              



(*********************
 *
 *
 * PARCOURS AST
 *
 * *******************)




let rec lident_of_path path =
  match path with
      Path.Pident id -> Longident.Lident (Ident.name id)
    | Path.Pdot (p, s, _) -> Longident.Ldot (lident_of_path p, s)
    | Path.Papply (p1, p2) ->
        Longident.Lapply (lident_of_path p1, lident_of_path p2)


let p = print_endline;;

let pas_gere() = (*p "Item de l'AST non géré pour le moment"*) Pstr_class_type []


let rec to_object_language str =
  List.map untype_structure_item str.str_items

and untype_structure_item item =
  let desc =
    match item.str_desc with
    (* Probablement une évaluation directe*)
      Tstr_eval exp -> let on_garde = (untype_expression exp) in pas_gere()

      (** correspond à un let*)
    | Tstr_value (Nonrecursive, list)  -> let _        =  p ("Tstr_value Nonrecursive pas obligatoirement une fonction") in
                                          let on_garde =  List.map (fun (pat, exp) -> untype_pattern pat, untype_expression exp) list in pas_gere()
    | Tstr_value (Recursive, list) -> let _        =  p ("Tstr_value Recursive pas obligatoirement une fonction") in
                                      let on_garde = (List.map (fun (pat, exp) -> untype_pattern pat, untype_expression exp) list) in pas_gere()


    | Tstr_primitive (id, name, v) -> let on_garde = (name, untype_value_description v)  in pas_gere()
        (* Définition d'un type*)
    | Tstr_type list -> let on_garde = (List.map (fun (id, name, decl) ->
              name, untype_type_declaration decl) list)  in pas_gere()
        (**Définition d'une exception*)
    | Tstr_exception (id, name, decl) -> let on_garde = (name, untype_exception_declaration decl) in pas_gere()
    | Tstr_exn_rebind (id, name, p, lid) -> let on_garde =  (id, name, p, lid) in pas_gere()
    | Tstr_module (id, name, mexpr)  -> let on_garde =  (name, untype_module_expr mexpr) in pas_gere()
    | Tstr_recmodule list -> let on_garde = (List.map (fun (id, name, mtype, mexpr) ->
              name, untype_module_type mtype,
              untype_module_expr mexpr) list) in pas_gere()
    | Tstr_modtype (id, name, mtype) -> let on_garde = (name, untype_module_type mtype) in pas_gere()
    | Tstr_open (path, lid) ->  let on_garde = (path, lid) in pas_gere()
    | Tstr_class list -> let on_garde = (List.map (fun (ci, _, _) ->
              { pci_virt = ci.ci_virt;
                pci_params = ci.ci_params;
                pci_name = ci.ci_id_name;
                pci_expr = untype_class_expr ci.ci_expr;
                pci_variance = ci.ci_variance;
                pci_loc = ci.ci_loc;
              }
          ) list)
 in
pas_gere()
    | Tstr_class_type list -> let on_garde = 
              (List.map (fun (id, name, ct) ->
              {
                pci_virt = ct.ci_virt;
                pci_params = ct.ci_params;
                pci_name = ct.ci_id_name;
                pci_expr = untype_class_type ct.ci_expr;
                pci_variance = ct.ci_variance;
                pci_loc = ct.ci_loc;
              }
          ) list) in
 pas_gere()
    | Tstr_include (mexpr, _) -> let on_garde =(untype_module_expr mexpr) in pas_gere()
  in
  let on_garde = desc, item.str_loc in p "structure_item"

and untype_value_description v =
  
        let on_garde = v.val_prim, (untype_core_type v.val_desc), v.val_loc in
  p "value_description "


and untype_type_declaration decl =
        let on_garde =
     decl.typ_params,
    ( List.map (fun (ct1, ct2, loc) ->
        (untype_core_type ct1,
          untype_core_type ct2, loc)
    ) decl.typ_cstrs),
     (match decl.typ_kind with
        Ttype_abstract -> pas_gere()
      | Ttype_variant list ->
                      let on_garde =   (List.map (fun (s, name, cts, loc) ->
                (name, List.map untype_core_type cts, None, loc)
            ) list)
 in pas_gere()
      | Ttype_record list ->
                      let on_garde = (List.map (fun (s, name, mut, ct, loc) ->
                (name, mut, untype_core_type ct, loc)
            ) list)
                        in pas_gere()
    ),
    decl.typ_private,
     (match decl.typ_manifest with
        None -> None
      | Some ct  -> let on_garde = (untype_core_type ct) in pas_gere()),

    decl.typ_variance,
    decl.typ_loc
        in p "type_declaration"

and untype_exception_declaration decl =
  List.map untype_core_type decl.exn_params




and untype_pattern pat =
  let desc =
  match pat with
  { pat_extra=[Tpat_unpack, _]; pat_desc = Tpat_var (_,name) } ->  pas_gere()
    | { pat_extra=[Tpat_type (path, lid), _] } ->  pas_gere()
    | { pat_extra= (Tpat_constraint ct, _) :: rem } ->
                    let on_garde =  (untype_pattern { pat with pat_extra=rem }, untype_core_type ct) in pas_gere()
    | _ ->
    match pat.pat_desc with
      Tpat_any -> Ppat_any
    | Tpat_var (id, name) ->
        begin
          match (Ident.name id).[0] with
            'A'..'Z' ->
             (* p ("def ?? ? "^name) ;*)  pas_gere()  
          | _ ->
               (*p ("def var ? "^name) ;*)  pas_gere() 
        end
    | Tpat_alias (pat, id, name) ->
                    let on_garde = (untype_pattern pat, name) in pas_gere()
    | Tpat_constant cst ->  pas_gere()
    | Tpat_tuple list ->  let on_garde = (List.map untype_pattern list) in  p "tuple"; pas_gere()
    | Tpat_construct (path, lid, _, args, explicit_arity) ->
                    let on_garde = (lid,
          (match args with
              [] -> None
            | args -> Some
                  { ppat_desc = Ppat_tuple (List.map untype_pattern args);
                  ppat_loc = pat.pat_loc; }
          ), explicit_arity) in pas_gere()
    | Tpat_variant (label, pato, _) ->
                    let on_garde = (label, match pato with
            None -> None
          | Some pat  -> let on_garde = (untype_pattern pat) in pas_gere()) in pas_gere()
    | Tpat_record (list, closed) ->
                    let on_garde = (List.map (fun (path, lid, _, pat) ->
              lid, untype_pattern pat) list, closed) in pas_gere()
    | Tpat_array list -> let on_garde = (List.map untype_pattern list) in pas_gere()
    | Tpat_or (p1, p2, _) -> let on_garde = (untype_pattern p1, untype_pattern p2) in pas_gere()
    | Tpat_lazy p -> let on_garde = (untype_pattern p) in pas_gere()
  in
  let on_garde = desc, pat.pat_loc in p "pattern"

and option f x = match x with None -> None | Some e  -> let on_garde = (f e) in pas_gere()


and untype_extra (extra, loc) sexp =
  let desc =
    match extra with
    | Texp_constraint (cty1, cty2) -> let on_garde =   (sexp,
                         option untype_core_type cty1,
                         option untype_core_type cty2) in  pas_gere()
    | Texp_open (_path, lid, _) -> let on_garde =  ( lid, sexp) in  pas_gere()
    | Texp_poly cto -> let on_garde =  (sexp, option untype_core_type cto) in  pas_gere()
    | Texp_newtype s -> let on_garde =  (s, sexp) in  pas_gere()
  in
        let on_garde = desc, loc in p "extra"




(***********************
 *
 *      Expressions
 *
 * *********************)


and untype_expression exp =
  let desc =
    match exp.exp_desc with
      Texp_ident (path, lid, _)  -> let on_garde = (lid) in pas_gere()
    | Texp_constant cst -> pas_gere()
    | Texp_let (rec_flag, list, exp)  -> let on_garde = (rec_flag, List.map (fun (pat, exp)  ->untype_pattern pat, untype_expression exp) list, untype_expression exp) in pas_gere()
    | Texp_function (label, cases, _)  -> let on_garde = (label, None,List.map (fun (pat, exp)  ->  (untype_pattern pat, untype_expression exp)) cases) in pas_gere()
    | Texp_apply (exp, list) -> let on_garde = (untype_expression exp,
          List.fold_right (fun (label, expo, _) list ->
              match expo with
                None -> list
              | Some exp -> (label, untype_expression exp) :: list
          ) list [])  in pas_gere()
    | Texp_match (exp, list, _) ->
        let on_garde = (untype_expression exp,
          List.map (fun (pat, exp) ->
              untype_pattern pat, untype_expression exp) list)  in pas_gere()
    | Texp_try (exp, list) ->
        let on_garde = (untype_expression exp,
          List.map (fun (pat, exp) ->
              untype_pattern pat, untype_expression exp) list) in pas_gere()
    | Texp_tuple list  -> let on_garde = (List.map untype_expression list) in pas_gere()
    | Texp_construct (path, lid, _, args, explicit_arity) ->
        let on_garde = (lid,
          (match args with
              [] -> None
          | [ arg ]  -> let on_garde = (untype_expression arg) in pas_gere()
          | args -> Some
            { pexp_desc = Pexp_tuple (List.map untype_expression args);
              pexp_loc = exp.exp_loc; }
          ), explicit_arity)  in pas_gere()
    | Texp_variant (label, expo) ->
        let on_garde = (label, match expo with
            None -> None
          | Some exp  -> let on_garde = (untype_expression exp) in None)  in pas_gere()
    | Texp_record (list, expo) ->
        let on_garde = (List.map (fun (path, lid, _, exp) ->
              lid, untype_expression exp
          ) list,
          match expo with
            None -> None
          | Some exp  -> let on_garde = (untype_expression exp) in None) in pas_gere()
    | Texp_field (exp, path, lid, label)  -> let on_garde = (untype_expression exp, lid) in pas_gere()
    | Texp_setfield (exp1, path, lid, label, exp2) ->
        let on_garde = (untype_expression exp1, lid,
          untype_expression exp2)  in pas_gere()
    | Texp_array list  -> let on_garde = (List.map untype_expression list) in pas_gere()
    | Texp_ifthenelse (exp1, exp2, expo) ->
        let on_garde = (untype_expression exp1,
          untype_expression exp2,
          match expo with
            None -> None
          | Some exp  -> let on_garde = (untype_expression exp) in None)  in pas_gere()
    | Texp_sequence (exp1, exp2)  -> let on_garde = (untype_expression exp1, untype_expression exp2) in pas_gere()
    | Texp_while (exp1, exp2)  -> let on_garde = (untype_expression exp1, untype_expression exp2) in pas_gere()
    | Texp_for (id, name, exp1, exp2, dir, exp3) ->
        let on_garde = (name,
          untype_expression exp1, untype_expression exp2,
          dir, untype_expression exp3)  in pas_gere()
    | Texp_when (exp1, exp2)  -> let on_garde = (untype_expression exp1, untype_expression exp2) in pas_gere()
    | Texp_send (exp, meth, _) ->
        let on_garde = (untype_expression exp, match meth with
            Tmeth_name name -> name
          | Tmeth_val id -> Ident.name id)  in pas_gere()
    | Texp_new (path, lid, _)  -> let on_garde = (lid) in pas_gere()
    | Texp_instvar (_, path, name) -> let on_garde = ({name with txt = lident_of_path path})  in pas_gere()
    | Texp_setinstvar (_, path, lid, exp)  -> let on_garde = (lid, untype_expression exp) in pas_gere()
    | Texp_override (_, list) ->
        let on_garde = (List.map (fun (path, lid, exp) ->
              lid, untype_expression exp
          ) list)  in pas_gere()
    | Texp_letmodule (id, name, mexpr, exp)  -> let on_garde = (name, untype_module_expr mexpr,   untype_expression exp) in pas_gere()
    | Texp_assert exp  -> let on_garde = (untype_expression exp) in pas_gere()
    | Texp_assertfalse -> pas_gere()
    | Texp_lazy exp  -> let on_garde = (untype_expression exp) in pas_gere()
    | Texp_object (cl, _)  -> let on_garde = (untype_class_structure cl) in pas_gere()
    | Texp_pack (mexpr)  -> let on_garde = (untype_module_expr mexpr) in pas_gere()
  in
  List.fold_right untype_extra exp.exp_extra
    { pexp_loc = exp.exp_loc;
      pexp_desc = desc }
and untype_package_type pack =
  (pack.pack_txt,
    List.map (fun (s, ct) ->
        (s, untype_core_type ct)) pack.pack_fields)

and untype_signature sg =
  List.map untype_signature_item sg.sig_items

and untype_modtype_declaration mdecl =
  match mdecl with
    Tmodtype_abstract -> pas_gere()
  | Tmodtype_manifest mtype -> let on_garde =  (untype_module_type mtype) in pas_gere()


and untype_signature_item item =
  let desc =
    match item.sig_desc with
      Tsig_value (id, name, v) -> let on_garde =  (name, untype_value_description v) in pas_gere()
    | Tsig_type list ->
        let on_garde =  (List.map (fun (id, name, decl) ->
              name, untype_type_declaration decl
          ) list) in pas_gere()
    | Tsig_exception (id, name, decl) -> let on_garde =  (name, untype_exception_declaration decl) in pas_gere()
    | Tsig_module (id, name, mtype) -> let on_garde =  (name, untype_module_type mtype) in pas_gere()
    | Tsig_recmodule list -> let on_garde = (List.map (fun (id, name, mtype) ->  name, untype_module_type mtype) list) in pas_gere()
    | Tsig_modtype (id, name, mdecl) -> let on_garde =  (name, untype_modtype_declaration mdecl) in pas_gere()
    | Tsig_open (path, lid) -> pas_gere()
    | Tsig_include (mty, lid) -> let on_garde =  (untype_module_type mty) in pas_gere()
    | Tsig_class list -> let on_garde =  (List.map untype_class_description list) in pas_gere()
    | Tsig_class_type list -> let on_garde =  (List.map untype_class_type_declaration list) in pas_gere()
  in
  let on_garde = desc, item.sig_loc in


and untype_class_description cd =
  let on_garde = cd.ci_virt, cd.ci_params, cd.ci_id_name, 
    pci_expr = untype_class_type cd.ci_expr;
    pci_variance = cd.ci_variance;
    pci_loc = cd.ci_loc;
  }

and untype_class_type_declaration cd =
  {
    pci_virt = cd.ci_virt;
    pci_params = cd.ci_params;
    pci_name = cd.ci_id_name;
    pci_expr = untype_class_type cd.ci_expr;
    pci_variance = cd.ci_variance;
    pci_loc = cd.ci_loc;
  }


and untype_module_type mty =
  let desc = match mty.mty_desc with
      Tmty_ident (path, lid) -> pas_gere()
    | Tmty_signature sg -> let on_garde =  (untype_signature sg) in pas_gere()
    | Tmty_functor (id, name, mtype1, mtype2) -> let on_garde = (name, untype_module_type mtype1, untype_module_type mtype2) in pas_gere()
    | Tmty_with (mtype, list) ->
                    let on_garde = (untype_module_type mtype,
          List.map (fun (path, lid, withc) ->
              lid, untype_with_constraint withc
          ) list) in pas_gere()
    | Tmty_typeof mexpr -> let on_garde = (untype_module_expr mexpr) in pas_gere()
  in
  {
    pmty_desc = desc;
    pmty_loc = mty.mty_loc;
  }



and untype_with_constraint lid cstr =
  match cstr with
    Twith_type decl  -> let on_garde = (lid, untype_type_declaration decl) in pas_gere()
  | Twith_module (_path, lid2)  -> let on_garde = (lid, lid2) in pas_gere()
  | Twith_typesubst decl  -> let on_garde = (untype_type_declaration decl) in pas_gere()
  | Twith_modsubst (_path, lid2)  -> let on_garde = (_path, lid2) in pas_gere()

and untype_module_expr mexpr =
  match mexpr.mod_desc with
    Tmod_constraint (m, _, Tmodtype_implicit, _ ) ->
      untype_module_expr m
  | _ ->
      let desc = match mexpr.mod_desc with
          Tmod_ident (_p, lid)  -> let on_garde = (lid) in pas_gere()
        | Tmod_structure st  -> let on_garde = (to_object_language st) in pas_gere()
        | Tmod_functor (_id, name, mtype, mexpr)  -> let on_garde = (name, untype_module_type mtype, untype_module_expr mexpr) in pas_gere()
        | Tmod_apply (mexp1, mexp2, _)  -> let on_garde = (untype_module_expr mexp1, untype_module_expr mexp2) in pas_gere()
        | Tmod_constraint (mexpr, _, Tmodtype_explicit mtype, _)  -> let on_garde = (untype_module_expr mexpr, untype_module_type mtype) in pas_gere()
        | Tmod_constraint (_mexpr, _, Tmodtype_implicit, _) -> assert false
        | Tmod_unpack (exp, _pack)  -> let on_garde = (untype_expression exp) in pas_gere()
        (* TODO , untype_package_type pack) *)

  in
 {
    pmod_desc = desc;
    pmod_loc = mexpr.mod_loc;
  }

and untype_class_expr cexpr =
  let desc = match cexpr.cl_desc with
    | Tcl_constraint ( { cl_desc = Tcl_ident (_path, lid, tyl); _ },
                       None, _, _, _ )  -> let on_garde = (lid, List.map untype_core_type tyl) in pas_gere()
    | Tcl_structure clstr  -> let on_garde = (untype_class_structure clstr) in pas_gere()

    | Tcl_fun (label, pat, _pv, cl, _partial)  -> let on_garde = (label, None, untype_pattern pat, untype_class_expr cl) in pas_gere()

    | Tcl_apply (cl, args) -> let on_garde = (untype_class_expr cl,
          List.fold_right (fun (label, expo, _) list ->
              match expo with
                None -> list
              | Some exp -> (label, untype_expression exp) :: list
          ) args [])
         in pas_gere()


    | Tcl_let (rec_flat, bindings, _ivars, cl) ->
                    let on_garde =  (rec_flat,
          List.map (fun (pat, exp) ->
              (untype_pattern pat, untype_expression exp)) bindings,
          untype_class_expr cl) in pas_gere()

    | Tcl_constraint (cl, Some clty, _vals, _meths, _concrs)  -> let on_garde = (untype_class_expr cl,  untype_class_type clty) in pas_gere()

    | Tcl_ident _ -> assert false
    | Tcl_constraint (_, None, _, _, _) -> assert false
  in
   { pcl_desc = desc;
    pcl_loc = cexpr.cl_loc;
  }



and untype_class_type ct =
  let desc = match ct.cltyp_desc with
      Tcty_signature csg  -> let on_garde = (untype_class_signature csg) in pas_gere()
    | Tcty_constr (_path, lid, list)  -> let on_garde = (lid, List.map untype_core_type list) in pas_gere()
    | Tcty_fun (label, ct, cl)  -> let on_garde = (label, untype_core_type ct, untype_class_type cl) in pas_gere()
  in
  { pcty_desc = desc;
    pcty_loc = ct.cltyp_loc;
   }

and untype_class_signature cs =
  {
    pcsig_self = untype_core_type cs.csig_self;
    pcsig_fields = List.map untype_class_type_field cs.csig_fields;
    pcsig_loc = cs.csig_loc;
  }

and untype_class_type_field ctf =
  let desc = match ctf.ctf_desc with
      Tctf_inher ct  -> let on_garde = (untype_class_type ct) in pas_gere()
    | Tctf_val (s, mut, virt, ct)  -> let on_garde = (s, mut, virt, untype_core_type ct) in pas_gere()
    | Tctf_meth  (s, priv, ct)  -> let on_garde = (s, priv,  untype_core_type ct) in pas_gere()
    | Tctf_virt  (s, priv, ct) ->   let on_garde = (s, priv, untype_core_type ct) in pas_gere()
    | Tctf_cstr  (ct1, ct2)  -> let on_garde = (untype_core_type ct1, untype_core_type ct2) in pas_gere()
  in
  {
    pctf_desc = desc;
    pctf_loc = ctf.ctf_loc;
  }

and untype_core_type ct =
  let desc = match ct.ctyp_desc with
      Ttyp_any -> Ptyp_any
    | Ttyp_var s -> Ptyp_var s
    | Ttyp_arrow (label, ct1, ct2)  -> let on_garde = (label, untype_core_type ct1, untype_core_type ct2) in pas_gere()
    | Ttyp_tuple list  -> let on_garde = (List.map untype_core_type list) in pas_gere()
    | Ttyp_constr (_path, lid, list) -> let on_garde = (lid,List.map untype_core_type list) in pas_gere()
    | Ttyp_object list  -> let on_garde =  (List.map untype_core_field_type list) in pas_gere()
    | Ttyp_class (path, lid, list, labels)  -> let on_garde =  (lid, List.map untype_core_type list, labels) in pas_gere()
    | Ttyp_alias (ct, s)  -> let on_garde = (untype_core_type ct, s) in pas_gere()
    | Ttyp_variant (list, bool, labels)  -> let on_garde = (List.map untype_row_field list, bool, labels)  in pas_gere()
    | Ttyp_poly (list, ct)  -> let on_garde = (list, untype_core_type ct) in pas_gere()
    | Ttyp_package pack  -> let on_garde = (untype_package_type pack) in pas_gere()
  in
  { ptyp_desc = desc; ptyp_loc = ct.ctyp_loc }  

and untype_core_field_type cft =
  { pfield_desc = (match cft.field_desc with 
      | Tcfield_var -> pas_gere()
      | Tcfield (s, ct) -> let on_garde = (s, untype_core_type ct) in pas_gere());
    pfield_loc = cft.field_loc; }


and untype_class_structure cs =
  { pcstr_pat = untype_pattern cs.cstr_pat;
    pcstr_fields = List.map untype_class_field cs.cstr_fields;
  }

and untype_row_field rf =
  match rf with
    Ttag (label, bool, list) ->
      Rtag (label, bool, List.map untype_core_type list)
  | Tinherit ct  -> let on_garde = (untype_core_type ct) in pas_gere()


and untype_class_field cf =
  let desc = match cf.cf_desc with
      Tcf_inher (ovf, cl, super, _vals, _meths) -> let on_garde = (ovf, untype_class_expr cl, super) in pas_gere()
    | Tcf_constr (cty, cty') -> let on_garde = (untype_core_type cty, untype_core_type cty') in pas_gere()
    | Tcf_val (lab, name, mut, _, Tcfk_virtual cty, override) -> let on_garde = (name, mut, untype_core_type cty) in pas_gere()
    | Tcf_val (lab, name, mut, _, Tcfk_concrete exp, override) -> let on_garde = (name, mut,
          (if override then Override else Fresh),
          untype_expression exp) in pas_gere()
    | Tcf_meth (lab, name, priv, Tcfk_virtual cty, override) -> let on_garde = (name, priv, untype_core_type cty) in pas_gere()
    | Tcf_meth (lab, name, priv, Tcfk_concrete exp, override) -> let on_garde = (name, priv, (if override then Override else Fresh), untype_expression exp) in pas_gere()
(*    | Tcf_let (rec_flag, bindings, _) ->
        Pcf_let (rec_flag, List.map (fun (pat, exp) ->
              untype_pattern pat, untype_expression exp) bindings)
*)
  | Tcf_init exp -> let on_garde = (untype_expression exp) in pas_gere()
  in
  { pcf_desc = desc; pcf_loc = cf.cf_loc }

;;

#trace untype_structure_item;;
#trace untype_value_description;;
#trace untype_type_declaration;;
#trace untype_exception_declaration;;
#trace untype_pattern;;
#trace option;;
#trace untype_extra;;
#trace untype_expression;;
#trace untype_package_type;;
#trace untype_signature;;
#trace untype_modtype_declaration;;
#trace untype_signature_item;;
#trace untype_class_description;;
#trace untype_class_type_declaration;;
#trace untype_module_type;;
#trace untype_with_constraint;;
#trace untype_module_expr;;
#trace untype_class_expr;;
#trace untype_class_type;;
#trace untype_class_signature;;
#trace untype_class_type_field;;
#trace untype_core_type;;
#trace untype_core_field_type;;
#trace untype_class_structure;;
#trace untype_row_field;;
#trace untype_class_field;;

