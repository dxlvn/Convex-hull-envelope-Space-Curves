(* list_tri.mli *)

      (************************************)
      (*                                  *)
      (* Specification du module List_tri *) 
      (*                                  *)
      (************************************)

(*************************************************)

val tri : ('a -> 'a -> bool) -> 'a list -> 'a list 

(* (tri comp l) partage l en deux listes de tailles équivalentes, trie chacune de ces listes récursivement, puis fusionne les deux résultats. De façon générale, elle trie la liste l en fonction de l'ordre passé en paramètre. *)

(*************************************************)

val min_list : ('a -> 'a -> bool) -> 'a list -> 'a

(* (min_list comp l) retourne le plus petit élément de l selon l'ordre passé en paramètre. *)

(*************************************************)

val suppr_doublons : 'a list -> 'a list

(* (suppr_doublons l) prend une liste l contenant d'éventuels doublons et la retourne sans ses doublons. *)


(*************************************************)