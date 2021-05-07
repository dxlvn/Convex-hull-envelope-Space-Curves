#load "graphics.cma";;
#load "point.cmo";;
open Point;;
#load "list_tri.cmo";;
open List_tri;;
open Graphics;;


"Calcul de pa et pb "

(*1*)

let min_point {abs=x1;ord=y1} {abs=x2;ord=y2}=if x1<x2 then true else false;;

(*2*)


let max_point {abs=x1;ord=y1} {abs=x2;ord=y2}=if x1>x2 then true else false;;
max_point {abs=2;ord=9} {abs=5;ord=10};;

(*3*)

let points_depart l = if l=[]
                      then failwith"liste vide"
                      else ((min_list (<) l) , (min_list (>) l)) ;;


(*ENSEMBLE DE POINTS A DROITE D'UNE DROITE ORIENTEE*)

(*Question*)

let rec points_droite_bis l p1 p2 k = match l with 
[] -> k
|x::r -> if ( (p2.abs - p1.abs)*(x.ord - p1.ord)) - ((p2.ord - p1.ord)*(x.abs - p1.abs)) < 0
         then points_droite_bis r p1 p2 (x::k) 
         else points_droite_bis r p1 p2 k ;;

let points_droite l p1 p2 = points_droite_bis l p1 p2 [] ;;



(*POINT LE PLUS ELOIGNE D'UNE DROITE*)

(*1*) 

let equation_droite p1 p2 = 
let b=(p1.abs - p2.abs) and a=(p2.ord - p1.ord) in let  c= -(a*p1.abs + b*p1.ord)  in (a,b,c) ;;


(*2*)

let distance_droite a b c p = float_of_int(abs(a*p.abs + b*p.ord + c))/. sqrt(float_of_int((a*a)+(b*b))) ;;


(*3*)

let distance_max p1 p2 pa pb = let (x1,x2,x3)=equation_droite p1 p2 in if distance_droite x1 x2 x3 pa > distance_droite x1 x2 x3 pb
                                                                       then true
                                                                       else false ;;

(*4*)


let rec point_eloigne_bis_bis p1 p2 l k = match l with 
[] -> k
|x::y::r -> if distance_max  p1 p2 x y
            then point_eloigne_bis_bis p1 p2 r (x::k)
            else point_eloigne_bis_bis p1 p2 r (y::k) ;;

let point_eloigne_bis p1 p2 l = point_eloigne_bis_bis p1 p2 l [] ;;

let point_eloigne p1 p2 l = min_list (>) (point_eloigne_bis p1 p2 l);;

(*
let rec distance p1 p2 l=
match l with
[]->[]
|x::r-> let (x1,x2,x3)=equation_droite p1 p2 in distance_droite x1 x2 x3 x::distance p1 p2 r;;
let grand_distance p1 p2 l=min_list (>) (distance p1 p2 l);;

let rec point_eloigne p1 p2 l=
match l with
[]->failwith"liste vide"
|x::r->let (x1,x2,x3)=equation_droite p1 p2 in if distance_droite x1 x2 x3 x=grand_distance p1 p2 l then x else point_eloigne p1 p2 r;;
*)

(*AJOUTER UN POINT DANS UNE LISTE A SA PLACE *)

(*1*)

let points_egaux p1 p2 = if p1.abs=p2.abs && p1.ord=p2.ord
                         then true
                         else false ;;

(*2*)


let rec ajoute_list_apres p1 p l=
match l with
[] -> failwith "P1 n'est pas dans la liste"
|x::r -> if x=p1 
         then x::p::r 
         else x::ajoute_list_apres p1 p r ;;


"FONCTION PRINCIPALE"
let rec findhull l p q enveloppe=
match l with
[]-> enveloppe
| x::r-> let c=point_eloigne p q l in let enveloppe=ajoute_list_apres p c enveloppe in let enveloppe=findhull (points_droite l p c ) p c enveloppe in findhull (points_droite l c q) c q enveloppe;;

let quickhull l=
let l=suppr_doublons l in let (pa,pb) =points_depart l in let enveloppe=pa::(pb::[]) in let enveloppe=findhull (points_droite l pa pb) pa pb enveloppe in findhull (points_droite l pb pa) pb pa enveloppe;;
quickhull [{abs=2;ord=5};{abs=7;ord=6}];;

(*Expérimentations*)

let enveloppe_convexe g n=
let l=g n in (vider ();
set_color red;
tracer_nuage l;
set_color blue;
tracer_polygone(quickhull l));;

initialiser();;
enveloppe_convexe gen_soleil 1000;;                           


