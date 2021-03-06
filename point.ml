1 (* point.ml *)

           (********************************)
           (*                              *)
           (* Implantation du module Point *)
           (*                              *)
           (********************************)


open Graphics ;;
(***************************************************************)

type point = { abs : int ; ord : int } ;;

type nuage = point list ;;

type polygone = point list ;;

(***************************************************************)
(***************************************************************)

let couleur_trace = red ;;

let couleur_fond = white ;;

(***************************************************************)
(**********************************************************)

let zero () = { abs = (size_x ()) / 2 ; ord = (size_y ()) / 2 } ;;

let vider () =
( set_color couleur_fond ;
  fill_rect 0 0 (size_x ()) (size_y ()) ;
  set_color couleur_trace ;
  moveto (zero ()).abs (zero ()).ord ) ;;

let initialiser () = ( open_graph "" ; vider () ) ;;

let terminer = close_graph ;;

(***************************************************************)

let tracer_point p =
  draw_circle (p.abs + (zero ()).abs) (p.ord + (zero ()).ord) 2 ;;

let tracer_nuage = List.iter tracer_point ;;

let tracer_polygone l =
  match l with
    p::r ->
      ( moveto (p.abs + (zero ()).abs) (p.ord + (zero ()).ord) ;
        List.iter
          (function q ->
            lineto (q.abs + (zero ()).abs) (q.ord + (zero ()).ord))
          r ;
        lineto (p.abs + (zero ()).abs) (p.ord + (zero ()).ord) )
  | [] -> () ;;

(***************************************************************)
(***************************************************************)

let deux_pi = 8.0 *. (atan 1.0) ;;

let rayon () =
  let x = float_of_int ((size_x ()) / 2)
  and y = float_of_int ((size_y ()) / 2) in
  min x y ;;

(***************************************************************)

let rec gen_rectangle n =
  if n = 0
  then []
  else
    let x = (Random.int (size_x ())) - ((size_x ()) / 2) 
    and y = (Random.int (size_y ())) - ((size_y ()) / 2)
    in { abs = x ; ord = y }
    :: ( gen_rectangle (n-1) ) ;;

(***************************************************************)

let rec gen_cercle n =
  if n = 0
  then []
  else
    let o = (Random.float deux_pi)
    and r = (Random.float (rayon ()))
    in { abs = int_of_float (r*.(cos o)) ;
         ord = int_of_float (r*.(sin o)) }
    :: ( gen_cercle (n-1) ) ;;

(***************************************************************)

let rec gen_papillon n =
  if n = 0
  then []
  else
    let o = (Random.float deux_pi) in
    let c = cos o
    and s = sin o in
    let r = (rayon ()) *.
            (Random.float (c*.c*.c*.c*.c*.c
                        +. s*.s*.s*.s*.s*.s))
    in { abs = int_of_float (r*.c) ;
         ord = int_of_float (r*.s) }
    :: ( gen_papillon (n-1) ) ;;

(***************************************************************)

let rec gen_cervolant n =
  if n = 0
  then []
  else
    let o = (Random.float deux_pi) in
    let c = cos o
    and s = sin o in
    let x = c *. c *. c *. (rayon ())
    and y = s *. s *. s *. (rayon ()) in
    let r = (((exp (Random.float 1.0)) -. 1.0) /.
             ((exp 1.0) -. 1.0))
    in { abs = int_of_float (r*.x) ;
         ord = int_of_float (r*.y) }
    :: ( gen_cervolant (n-1) ) ;;

(***************************************************************)

let rec gen_soleil n =
  if n = 0
  then []
  else
    let o = (Random.float (8.0*.deux_pi)) in
    let c = cos (0.03125 *. o)
    and s = sin (0.03125 *. o) in
    let c2 = (cos o)*.(cos o) in
    let x = c *. c2 *. (rayon ())
    and y = s *. c2 *. (rayon ())
    and r = (Random.float 1.0)
    in { abs = int_of_float (r*.x) ;
         ord = int_of_float (r*.y) }
    :: ( gen_soleil (n-1) ) ;;

(***************************************************************)

let rec gen_poisson n =
  if n = 0
  then []
  else
    let o = (Random.float deux_pi) in
    let c = cos (2.0 *. o)
    and s = sin (7.0 *. o) in
    let c2 = (cos o)*.(cos o) in
    let x = c *. c2 *. (rayon ())
    and y = s *. c2 *. (rayon ())
    and r = (Random.float 1.0)
    in { abs = int_of_float (r*.x) ;
         ord = int_of_float (r*.y) }
    :: ( gen_poisson (n-1) ) ;;

