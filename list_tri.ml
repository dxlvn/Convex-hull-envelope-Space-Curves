"TRI PAR CREATION DU MAXIMUM "

(*1*)


let rec selectionne_max_bis comp l a = match l with
[] -> a
|x::r -> if (comp) x a
         then selectionne_max_bis comp r a
         else selectionne_max_bis comp r x ;;
let selectionne_max comp l = match l with 
[] -> failwith "liste vide"
|x::r -> selectionne_max_bis comp l x ;;


(*2*)

let rec supprime a l = match l with
[] -> []
|x::r -> if x=a 
         then r 
         else x::(supprime a r);;


(*3*)

let ajoute_fin x l = l@[x] ;;

(*4*)

let rec tri_creation_max comp l = match l with 
[] -> []
|x::r -> (tri_creation_max comp (supprime (selectionne_max comp (x::r)) (x::r)))@[selectionne_max comp (x::r)];;

(*5*)

(*
tri_creation_max (<) (random_list 100 10);;
tri_creation_max (>) (random_list 100 80);;
tri_creation_max (<) (random_list 200 50 );;
tri_creation_max (>) (random_list 100 200);;
*)

(*6*)

(*Fait*)

"TRI PAR PARTITION-FUSION"

(*1*)

let rec partition l (k1,k2) = match l with
[] -> (k1,k2)
|[x] -> (k1@[x],k2)
|x::y::r -> partition r (k1@[x],k2@[y]) ;;
let partitionne l = partition l ([],[]) ;;

(*2*)

let rec fusion_aux comp l1 l2 l=
match (l1,l2) with
([],[])->l
|([],l2)->tri_creation_max comp (l@l2)
|(l1,[])->tri_creation_max comp (l@l1)
| (x1::r1,x2::r2)->tri_creation_max comp (fusion_aux comp r1 r2 (x1::x2::l));;
let fusionne comp l1 l2 = fusion_aux comp l1 l2 [];;

let fusionner comp (l1,l2) = fusionne comp l1 l2 ;;

(*3*)

let tri_partition_fusion comp l= match l with
[]->[]
| x::r->fusionner comp (partitionne l);;

(*4*)

(*
tri_partition_fusion (<) (random_list 100 10);;
tri_partition_fusion (>) (random_list 100 80);;
tri_partition_fusion (<) (random_list 200 50 );;
tri_partition_fusion (>) (random_list 100 200);;
*)



(*5*)

(*Fait*)

"TRI PAR ARBRE BINAIRE DE RECHERCHE"


(*1*)

type 'a arbreBinaire =
ArbreVide
|Noeud of ('a * 'a arbreBinaire * 'a arbreBinaire);;

(*2*)


let rec insere_noeud comp x a = match a with
ArbreVide -> Noeud(x,ArbreVide,ArbreVide)
|Noeud(r,g,d) -> if comp r x
                 then Noeud(r,g,(insere_noeud comp x d))
                 else Noeud(r,(insere_noeud comp x g),d);;

let a=Noeud(4,
           Noeud(2,ArbreVide,ArbreVide) ,
	   Noeud(7,
	         Noeud(5,ArbreVide,ArbreVide),
		 Noeud(9,ArbreVide,ArbreVide)));;
let a2=Noeud(4,
             Noeud(7,
	            Noeud(9,ArbreVide,ArbreVide),
		    Noeud(5,ArbreVide,ArbreVide)),
	     Noeud(2,ArbreVide,ArbreVide));;


(*3*)


let rec insere_liste_noeuds comp l a = match l with
[] -> a
|x::r -> insere_liste_noeuds comp r ( insere_noeud comp x a);;

(*4*)

let rec pig a = match a with 
ArbreVide -> [] 
|Noeud(r,g,d) -> (pig g)@[r]@(pig d);;
let parcours_arbre a = pig a;;

(*5*)

let tri_par_abr comp l = parcours_arbre (insere_liste_noeuds comp l ArbreVide) ;;

(*6*)

(*
tri_par_abr (<) (random_list 100 10);;
tri_par_abr (>) (random_list 100 80);;
tri_par_abr (<) (random_list 200 50 );;
tri_par_abr (>) (random_list 100 200);;
*)

(*7*)

(*Fait*)


(*"CHOIX D'UNE FONCTION DE TRI"*)

(*let temps_debut = Sys.time () in
let f1 =  tri_creation_max in
let temps_fin = Sys.time () in
(temps_fin -. temps_debut) ;;


let temps_debut = Sys.time () in
let f2 = tri_partition_fusion in
let temps_fin = Sys.time () in
(temps_fin -. temps_debut) ;;


let temps_debut = Sys.time () in
let f3 = tri_par_abr in
let temps_fin = Sys.time () in
(temps_fin -. temps_debut) ;;*)


(* Plusieurs essais ayant été effectués ,nous n'en garderont que deux en exemple. *)


(* 
 Warning 26: unused variable f1.
- : float = 1.09999999999832454e-05
File "list_tri.ml", line 142, characters 4-6:
Warning 26: unused variable f2.
- : float = 9.99999999917733362e-07
File "list_tri.ml", line 148, characters 4-6:
Warning 26: unused variable f3.
*)

(*
Warning 26: unused variable f1.
- : float = 1.000000000001e-06
File "list_tri.ml", line 144, characters 4-6:
Warning 26: unused variable f2.
- : float = 1.000000000001e-06
File "list_tri.ml", line 150, characters 4-6:
Warning 26: unused variable f3.
- : float = 1.000000000001e-06

 *)

"la fonction tri_partition_fusion est notre meilleure fonction de tri"

let tri =  tri_partition_fusion ;; 


"DEFINITION DU MODULE List_tri"

(*Deux autres fonctions*)


let rec min_list_bis comp l a = match l with
|[] -> a
|x::r -> if comp x a
         then min_list_bis comp r x
         else min_list_bis comp r a ;;
let min_list comp l = match l with
|[] -> failwith "liste vide" 
|x::r -> min_list_bis comp l x ;;


let rec appartient a l = match l with
[] -> false
|x::r -> if x = a 
         then true 
         else appartient a r ;;
let rec sup_doublons l k = match l with 
[] -> k
|x::r -> if appartient x k
         then sup_doublons r k 
         else sup_doublons r (k@[x]);;
let suppr_doublons l = sup_doublons l [] ;;
