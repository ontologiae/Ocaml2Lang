module H = BatHashtbl;;


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

  | VarDef              of  name * objexpre (*langage simple, objexpression simple*)
  | IfThenElse          of  objexpre  * objexpre *  objexpre option        (* Conditional [if e1 then e2 else e3] *)
  | ForC                of  objexpre  * objexpre * objexpre * objexpre (*for( <objexpre>; <objexpre>; <objexpre>) <objexpre>*)
  | ForTo               of  int    * int   * objexpre
  | ForEach             of  expre * expre (*ce sur quoi on itère. L'expression d'itération*)
  | FunDef              of  name * name list * objexpre list
  | Funcall             of  name * objexpre list
  | TryWith             of  objexpre  * name  * objexpre
  | VarAffect           of  name * objexpre
  | MethodCall          of  objexpre * name * objexpre list (*Receveur, méthode, arguments*)

  | ApplyExpre          of  objexpre * (objexpre list )(* Appelant, paramètres*)
  (* Il faut def un apply à n argument*)



let astsimple = [Let (NonRec, [Var "funtst2"], TArrow (TString, TArrow (TString, TArrow (TString, TString))),
    [Fun ([Var "a"], Inconnu, [Fun ([Var "b"], Inconnu, [Fun ([Var "c"], Inconnu, [StringConcat (Var "a", StringConcat (Var "b", Var "c"))])])])]);
   Let (NonRec, [Var "funtst3"], TArrow (TString, TArrow (TString, TArrow (TString, TString))),
   [Fun ([Var "a"], Inconnu, [Fun ([Var "b"], Inconnu, [Fun ([Var "c"], Inconnu, [StringConcat (Var "a", StringConcat (Var "b", Var "c"))])])])])];;


let objexpre_of_camlexpre = This;;
        (** Stratégie
         * 0. RÉORGANISATION : le code de ocaml2lang va être renommé en parse_caml.ml, compilé dans un .cmo
         *      On aura
         *
         *
         * 1. Faire un dictionnaire des types présents, de sorte que les Tlink renvoient vers quelques chose
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
                                
         * 3. Dès qu'un record est créé, on créé les objets correspondants
         * 4. Prégénérer des fonctions à plusieurs variables, quand on a de l'ordre supérieur. Si on détecte un appel non total dans le code, il faudra génrer un autre version de la fonction.
         *
         * Pour le reste : 
         * - Sur chaque Let, on va regarder le typage : si on un TArrow, alors c'est que c'est une def de fonction
         *   Sinon c'est une définition de variable
         * - Tranformer le if/then/else qui est une expression en instruction, genre :
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

