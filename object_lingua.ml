module H = BatHashtbl;;
module O = BatOption;;
open Parse_ocaml;;

(* LE TODO de progression est en bas*)


type name = string

type dictionnaire_types = { dico : (name, Parse_ocaml.ty) BatHashtbl.t }



type  objexpre =
  | This
  | ListeVide           (* [] *)
  | Var                 of name 		(* Variable *)
  | Int                 of int     	
  | Bool                of bool
  | Float               of float   
  | Char                of char    
  | String              of string
  | IsInstanceOf        of  objexpre (*renvoi un booléen*)
  | SequenceList        of  objexpre list
  | StringConcat        of  objexpre * objexpre              (*Concaténation de chaîne*)
  | ListConcat          of  objexpre * objexpre              (*Concaténation de liste*)
  | ListAddElem         of  objexpre * objexpre              (*Beau trou de typage le 2ème argument est une liste*)
  | ListHead            of  objexpre (*Opération de base qu'il faudra traduir dans les divers langages cible*)
  | ListTail            of  objexpre
  | Times               of  objexpre * objexpre 	               (* Product [e1 * e2] *)
  | Div                 of  objexpre * objexpre
  | Plus                of  objexpre * objexpre    		       (* Sum [e1 + e2] *)
  | Minus               of  objexpre * objexpre 	       (* Difference [e1 - e2] *)
  | Equal               of  objexpre * objexpre                       (* General comparison [e1 = e2] *)
  | Less                of  objexpre * objexpre 		       (* Integer comparison [e1 < e2] *)
  | LessEqual           of  objexpre * objexpre

  | VarDef              of  name * objexpre option (*langage simple, objexpression simple*)
  | IfThenElse          of  objexpre  * objexpre *  objexpre option        (* Conditional [if e1 then e2 else e3] *)
  | ForC                of  objexpre  * objexpre * objexpre * objexpre (*for( <objexpre>; <objexpre>; <objexpre>) <objexpre>*)
  | ForTo               of  int    * int   * objexpre
  | ForEach             of  objexpre * objexpre (*ce sur quoi on itère. L'expression d'itération*)
  | FunDef              of  name * name list * objexpre list
  | Funcall             of  name * objexpre list
  | TryWith             of  objexpre  * name  * objexpre
  | VarAffect           of  name * objexpre
  | MethodCall          of  objexpre * name * objexpre list (*Receveur, méthode, arguments*)

  | ApplyExpre          of  objexpre * (objexpre list )(* Appelant, paramètres*)
  | ClassDef            of  name * name option (* héritage *) *  objexpre list
  | New                 of  name * objexpre list (*les arguments du constructeur*)
  (* Il faut def un apply à n argument*)



let astsimple = [Let (NonRec, [Var "funtst2"], TArrow (TString, TArrow (TString, TArrow (TString, TString))),
    [Fun ([Var "a"], Inconnu, [Fun ([Var "b"], Inconnu, [Fun ([Var "c"], Inconnu, [StringConcat (Var "a", StringConcat (Var "b", Var "c"))])])])]);
   Let (NonRec, [Var "funtst3"], TArrow (TString, TArrow (TString, TArrow (TString, TString))),
   [Fun ([Var "a"], Inconnu, [Fun ([Var "b"], Inconnu, [Fun ([Var "c"], Inconnu, [StringConcat (Var "a", StringConcat (Var "b", Var "c"))])])])])];;


let ast_def_type = [TypeDeclaration [("expr", TSum_type [TType_variant ("Int", TInt); TType_variant ("Plus", TTuple [TLink "expr"; TLink "expr"]); TType_variant ("Moins", TTuple [TLink "expr"; TLink "expr"])])]; 
Let (NonRec, [Var "a"], TLink "expr", [ConstructionType ("Moins",
[ConstructionType ("Plus", [ConstructionType ("Int", [Int 6]); ConstructionType
("Int", [Int 9])]); ConstructionType ("Int", [Int 5])])])];;


let ast_record = [TypeDeclaration [("enregistrement", TRecord [("a", TInt); ("b", TString); ("c", TChar)])];
   Let (NonRec, [Var "e"], TLink "enregistrement", [RecordElemAffect [("a", Inconnu, Int 8); ("b", Inconnu, String "test contenu chaine dans record"); ("c", Inconnu, Char 'R')]])]



(* Construit un dictionnaire de types. Nom -> Def*)
let dico_of_camlexpre e = 
        let get_type_decl ex = (match ex with
                                | TypeDeclaration l -> Some l
                                | _                 -> None
                ) in
        let types = L.map O.get (L.filter O.is_some (L.map get_type_decl e)) in
        let liste_Nom_Types = L.unique (L.flatten types) in
        let result = { dico = H.create 128 } in        
        let _ = L.iter (fun (n,t) -> H.add result.dico n t) liste_Nom_Types in
        result
        (*On part du principe que la déclaration des types est dans les têtes.
         * Après faudra parcourir l'AST..*)


let base_type_to_object_base_type t : objexpre =
        match t with
        | TInt -> Int 0
        | TChar -> Char '0'
        | TString -> String ""
        | _ -> failwith "à finir"



(* Cette fonction prend une définition de type et en génère un objet si nécessaire.
 * TODO résultat :      si c'est un type simple, pas besoin de faire grand chose
 *                      Si c'est un type somme, il faut construire les valeur, et survient le problème du nommage des variables.
 *
 * On gère pas les tuples, et pour chaque tuple, soit on les flatten, soit on créé un objet pour lui.
 * Pas question d'utiliser des tuples as_is, car on ne sait pas si le langage cible les gèrent
 * Autre gros problème : *)
let base_type_to_classe_props tt =
        let rec func i t = 
                let number = string_of_int i in
                match t with
                | TInt           -> [VarAffect("value"^number, Int 0)]
                | TChar          -> [VarAffect("value"^number, Char '0')]
                | TString        -> [VarAffect("value"^number, String "")]
                | TLink  name    -> [VarAffect("value"^number, New(name,[]))]
                | TTuple l       -> L.flatten (L.mapi (fun i -> fun a -> (func (i+1) a)) l) 
                | _ -> failwith "à finir"
        in func 0 tt

(* TODO : quelle est la différence avec la fonction du dessus ??*)
let base_type_liste_record_to_class tlist =
        let rec func n t =
                 match t with
                | TInt           -> [VarAffect(n, Int 0)]
                | TChar          -> [VarAffect(n, Char '0')]
                | TString        -> [VarAffect(n, String "")]
                | TLink  name    -> [VarAffect(n, New(name,[]))]
                | TTuple l       -> L.flatten (L.map (func n)  l)
                | _ -> failwith "à finir" in
        L.flatten (L.map (fun (n,t) -> func n t) tlist)



(* Construit un objet en fonction du type donné en argument
 * TODO : discriminer les cas où on a des types simple, des records, des types sommes ou un mix de tout cela*)

let object_of_type t =
        let nom_type,typ_decl = t in
        let mk_obj l nt = 
                let process_variant v = ( match v with
                | TType_variant (n,tt) -> ClassDef(n, Some nt,  base_type_to_classe_props tt (*TODO*))
                | _ -> failwith "autre cas doit pas arriver") in
            L.map process_variant l
        in
        match typ_decl with
        | TSum_type l -> ( ClassDef(nom_type, None, []))::(mk_obj l nom_type)
        | TRecord   l -> [ ClassDef(nom_type, None, base_type_liste_record_to_class l)]
        | _     -> failwith "pas géré"

(*Construit la liste des fonction du code*)
let make_function_list e =
        let process_one ee = match ee with
                | Let ( isec, params, TArrow(input_type,output_type), member) as letf -> Some letf
                | _                                                                   -> None
        in L.map O.get (L.filter O.is_some (L.map process_one e))

(* Compte le nombre s'imbrication de fonctions dans le type d'une fonction*)
let rec count_parameter_cardinal tt =
        match tt with
        | TArrow(a,b) -> 1 + count_parameter_cardinal(a) + count_parameter_cardinal(b)
        | _           -> 0

(* Convertit une expre dans l'AST objet*)
let rec to_expre (e : Parse_ocaml.expre) = match e with
        | Parse_ocaml.StringConcat (a,b)    -> StringConcat(to_expre a, to_expre b)
        | Int a                 -> Int a
        | Char c                -> Char c
        | String s              -> String s
        | StringConcat (e1,e2)  -> StringConcat (to_expre e1,to_expre e2)   
        | ListConcat (e1,e2)    -> ListConcat (to_expre e1, to_expre e2)     
        | ListAddElem (e1,e2)   -> ListAddElem (to_expre e1,to_expre e2)    
        | Times (e1,e2)         -> Times (to_expre e1,to_expre e2)          
        | Div (e1,e2)           -> Div (to_expre  e1, to_expre e2)            
        | Plus (e1,e2)          -> Plus (to_expre e1, to_expre e2)           
        | Minus (e1,e2)         -> Minus (to_expre e1, to_expre e2)          
        | Equal (e1,e2)         -> Equal (to_expre e1, to_expre e2)          
        | Less (e1,e2)          -> Less (to_expre e1,to_expre e2)
        | Let(isrec,param::[] (*une seule fonction*),TArrow(input_type,output_type), member) as letf -> uncurrify_function param letf
        | IfThenElse(cond,iff,els) -> SequenceList [VarDef "result_if";IfThenElse(cond,VarAffect("result_if",to_expre iff),VarAffect("result_if",to_expre els))] (*Point 6*)
        | Var a                 -> Var a
        | _     -> failwith "pas encore géré"


(* Décurifie une fonction *)
and uncurrify_function param_nom_fonc ef =
        (*1er cas : simple : aucun calcul intermédiaire dans les premières fonctions
         *      On va jusqu'au bout, en récursif, en empilant, dans une liste, les paramètres
         *      On fait de même avec le typage
         *      On combine les deux listes en couples (nom,type) *)
        let rec liste_typ typ = match typ with
        | TArrow( input, output) -> input::(liste_typ output)
        | portnawak              -> portnawak::[] in
        (*L.take ((L.length l) -1) l pour la liste sans le dernier*)
        let isrec, params, type_let, member =
                match ef with
                | Let ( isrec, param, ttl, member) -> isrec, param, ttl, member
                | _ -> failwith "on ne doit recevoir que des fonctions" in
        let parameter_cardinal = count_parameter_cardinal type_let in
        let rec find_corpse m = match m with
                             | [Fun (_,_,mmm)] -> find_corpse mmm
                             | m              ->  m in
        let rec find_params p = match p with
                                 | [Fun (params,_,mmm)] -> params@(find_params mmm)
                                 | _                    -> [] in
        let e_to_string e = match e with
                                    | Parse_ocaml.Var c -> c
                                    | _ -> failwith "Nom de variable pour param pas extractible" in

        let params_to_string l = 
                                L.map e_to_string l in
        FunDef(e_to_string param_nom_fonc, params_to_string (find_params member), L.map to_expre (find_corpse member)) (*TODO : Bidouille*)
         
(* Pour le moment on va juste gérer des formes fun a -> fun b -> func c -> <code...>*)


let objexpre_of_camlexpre e = This;;
        (** Stratégie
         * 0. RÉORGANISATION : le code de ocaml2lang va être renommé en parse_caml.ml, compilé dans un .cmo
         *      On aura
         *
         *
         * 1. Faire un dictionnaire des types présents, de sorte que les Tlink renvoient vers quelques chose
         *      OK
         *
         * 2. Dès qu'un type somme est détecté, on créé les objets de ce type somme.
         *      Exemple :
                 *      type expre = 
                         *      | Int of int
                         *      | Plus  of expre * expre
                         *      | Moins of expre * expre
                         *
                         *      Deviens, en pseudo code objet :
                                 *     Class expre =
                                         *     currentValue
                                         *
                                 *    Class Int inherit expre
                                 *     value of int
                                 *
                                 *     Class Plus inherit expre
                                 *     value1 of expre
                                 *     value2 of expre
                                 *
                                 *     Class Moins inherit expre
                                 *     value1 of expre
                                 *     value2 of expre
                  * ---==============OK-==============---   
                                
         * 3. Dès qu'un record est créé, on créé les objets correspondants
         *      TODO
         *
         *    ---==============OK-==============---
         *
         * 4. Prégénérer des fonctions à plusieurs variables, quand on a de l'ordre supérieur. Si on détecte un appel non total dans le code, il faudra générer un autre version de la fonction.
         *    a. Compter le nombre de paramètres : nombre d'imbrication de TArrow
         *     OK PARTIEL
         *
         *
         *
         * Pour le reste : 
         * 5. - Sur chaque Let, on va regarder le typage : si on un TArrow, alors c'est que c'est une def de fonction
         *   Sinon c'est une définition de variable
         * 6. - Tranformer le if/then/else qui est une expression en instruction, genre :
                 * var result;
                 * if <cond> then result = <expre d'origine> else result = <expre d'origine>
                 * la_variable_du_let = result
                 *
         * Dictionnaire Langage : Il faut être capable de détecter les appels standard dans la lib, genre List.map, les appels pervasives, etc...
         * De même, être capable de savoir que BatList = L = List et etc...
         * Pour ce dico, en entrée, il faut la liste des modules possibles (List, BatList, ExtList.List), la fonction, son type.
         * On va commencer par Pervasive et List/BatList, on aura en face, le langage et la fonction de transformation correspondante (d'AST caml à AST objet)
         *
         *
         * I. DÉROULEMENT
         *  
         *  On va faire comme Lisaac : Depending Pass, Compiling Pass
         *  1. --== Depending Pass ==--
         *  a. Génération du code liés au types : Types somme, records --> Tout cela dans la liste du code généré
         *  b. Faire la liste des fonctions, avec leur type
         *  c. Chercher si ces fonctions sont exécutées dans le code, de manière exclusivement totale, ou non totale
         *      Si total : La fonction d'ordre n est tranformée en fonction à plusieurs variables. Il faut pouvoir la tagguer quelques part (dans un dictionnaire ?) pour le savoir
         *      Si non total : On compte le nombre d'appels différents. On va chercher l'appel partiel, pour connaître les paramètres à lui donner lors de l'appel de fonction spécialisée construite.
         *                     Dans ce cas, on reconstruit l'AST en faisant ce remplacement ?
         *                     Exemple let toto a b c d = ... in
*                                      let tata h = toto param1 param2 param3 in
*                                      L.map tata list
*                                      Transformé en
*                                      let toto a b c d = ...
*                                      in L.map (toto param1 param2 param3) list
         *
         *  2. --== Compiling Pass ==--
         *    - Parcours de l'AST pour transformation du code. Chaque let est transformé en fonction si et seulement si, la variable définie dans le let appartient au dictionnaire des fonctions.
         *    - Transformation des appels de fonctions si appels totales ou non total
         *    - Recherche dans le dictionnaire, en fonction du langage cible, de l'écriture correspondante de la fonction (mais je crois que ça va finir, que je vais générer BatList dans chaque langage,
         *    et faire croire que c'est un port de underscore...
         *    - Pattern matching généré en fonction des objets créés précédements
         *      - Si c'est une liste, on doit déterminer 2 choses pour chaque pattern :
                 * La taille de la liste qu'on pattern match : un::deux::queue a minimum 2 éléments. MAIS (sinon ça serait pas drôle), si on a pattern matché avant un::deux::[] c'est minimum 3
                   éléments. Cela dit, comme en ocaml à l'exécution, le elseif fera son boulot puisqu'il sera exécuté avant...
                   De même un::deux::[] c'est EXACTEMENT 2 éléments
                 * La présence de constante ou de précision concernant l'élément, genre un::"deux"::queue
                - Si c'est un type somme, on va jouer avec des isInstanceOf : Si on doit matcher Plus(Moins(a,b),c) on va faire
                  IsInstanceOf(obj) == Plus and IsInstanceOf(obj.value1) == Moins et dans la fonction, obj.value1.value1 = a, obj.value1.value2 = b, obj.value2 = c

         * *)

