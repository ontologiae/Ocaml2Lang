module L = BatList;;
let erreur = print_endline;;
(** Type d'arbre ne contenant qu'un seul élément par noeud*)
 type arbre = Node of string * arbre list | Feuille of string;;

(** Type d'arbre  contenant plusieurs éléments par noeud*)
 type arbre2 = Noeud of string list * arbre2 list  | Feuille_ of string (** Permet d'éviter les pattern match fastidieux*)
 
(** Renvoi les têtes de chaque listes de la liste*)
 let rec extractFirsts = function
   | [[]]    -> []
   | []      -> []
         (* | [[]]::q -> erreur "Faire attention à bien mettre des distinct dans la requête..";
            failwith "Erreur extraction arbre de la requête"*)
   | t::q    -> let tet = try (L.hd t) with e -> erreur "Faire attention à bien mettre des distinct dans la requête..";
     failwith "Erreur extraction arbre de la requête" in
                tet::extractFirsts q
 ;;





 (** Renvoi la liste de la queue de chaque sous liste si la clé correspond à tete*)
 let rec get_assoc tete =  function
   | [[]] -> []
   | []   -> []
   | t::q -> if (L.hd t) = tete then (L.tl t)::(get_assoc tete q) else get_assoc tete q
 ;;



 (** Détecte les pattern Node a,[] et les transforme en feuille*)
 let rec enfeuille = function
   | Feuille a     -> Feuille a
   | Node (a , []) -> Feuille a
   | Node (a , l ) -> Node(a, L.map enfeuille l);;


(** Transforme 'a option list en 'a list *)
 let rec deSome = function
   | (Some(t))::q -> t::deSome(q)
   | [None]   -> []
   | []  -> []
   | None::q -> [];;


(** Construit un arbre n-aire à partir d'une liste *)
 let rec list2tree lst =
   let cles lst = L.unique ( extractFirsts lst) in
   let res      = L.map (fun c -> let souliste = get_assoc c lst in 
                                     Node(c,list2tree souliste)
   )
     (cles lst) in
   L.map enfeuille res;;
 


(** Algo d'extraction d'arbre à partir d'une liste de profondeur *)
let rec extractFirsts_avec_profondeur nval = function
   | [[]]    -> []
   | []      -> []
         (* | [[]]::q -> erreur "Faire attention à bien mettre des distinct dans la requête..";
            failwith "Erreur extraction arbre de la requête"*)
   | t::q    -> let tet = try let d,r = L.split_at nval t in d with e -> erreur "Faire attention à bien mettre des distinct dans la requête..";
     failwith "Erreur extraction arbre de la requête" in
                tet::(extractFirsts_avec_profondeur nval q)
 ;;


(**Renvoi la liste de la queue de chaque sous liste si la clé correspond à tete, avec une notion de profondeur, donc d'élément dans la liste**)
let rec get_assoc_avec_profondeur nval tete =  function
   | [[]] -> []
   | []   -> []
   | t::q -> let tetlist,queue = L.split_at nval t in if tetlist = tete then queue::(get_assoc_avec_profondeur nval tete q) else get_assoc_avec_profondeur nval tete q
 ;;

(** Construit un arbre n-aire à partir d'une liste avec liste de profondeur*)
 let rec list2tree_avec_profondeur nval_list lst =
   let cles nval lst = L.unique ( extractFirsts_avec_profondeur nval lst) in
    match nval_list with
    | nval::q  ->      L.map (fun c -> let souliste = get_assoc_avec_profondeur nval c lst in 
                                     Noeud(c,list2tree_avec_profondeur q souliste)
   )
     (cles nval lst)
    | [] -> []  



(*A partir d'une liste ORDONNÉE  dans l'ordre des colonnes, construit un arbre n-aire en groupant les entêtes communs. Permet de construire
 * des sructures de données plus facilement*)
let listTree lstOption = list2tree (deSome lstOption);;


