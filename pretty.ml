
module Notation_scientifique : sig
  type ns
    
  val make :
    
    mantisse:(float) ->
    exposant:(int) ->
    unit -> ns
    
  val get_mantisse : ns -> float 
  val get_exposant : ns -> int
  val notation: ns -> float * int
                      
  
end 
= struct 
  type ns = {
    mantisse :float; (* E [1 ; 10[*)
    exposant : int; (* E Z*)
  }
  
  let make    
      ~mantisse
      ~exposant
      ()
    = 
    {
      mantisse ;
      exposant ;
    }
    
  let get_exposant ns = ns.exposant
  let get_mantisse ns = ns.mantisse
  let notation ns = (get_mantisse ns, get_exposant ns)
                    
  
end

let exemple_ns mantisse exposant= Notation_scientifique.make mantisse exposant ()
let notation mantisse exposant =  Notation_scientifique.notation (exemple_ns mantisse exposant)
(*
# notation 0.264 (-5);;
- : float * int = (0.264, -5)
*)

let rec list_aux n i f =
  if i > n then [] 
  else f i :: list_aux n (i + 1) f
         
let list_init n f = list_aux n 0 f 
    
let abs a = -. a

let is_div x n = if (mod_float x n) = 0. then true else false
let pas a b n = (b -. a ) /. n
                  
let delta a b = b -. a      

(*cas ou b-a est divisible par n *)
let rec range_aux a b n i = 
  if i > n then [] 
  else (a +. (pas a b n) *. i) :: range_aux a b n  (i +. 1.)  

let range a b n = range_aux a b n 0.
(*
tests
#  range 0. 20. 5.;;
- : float list = [0.; 4.; 8.; 12.; 16.; 20.]
#   range 0. 10. 5.;;
- : float list = [0.; 2.; 4.; 6.; 8.; 10.]
# range 0. 15. 3.;;
- : float list = [0.; 5.; 10.; 15.]
# range 0. 15. 5.;;
- : float list = [0.; 3.; 6.; 9.; 12.; 15.]
*)        


(*
fonction qui appelle range si b - a est divisible par n
 et qui appelle un autre traitement sinon 

la valeur max minimum doit etre 2 
*)    
let range_far_zero  a b n = 
  if (is_div (delta a b) n ) then
    range a b n  
  else 
     let max = b +. 1. in 
    (* let min = a -. 1. in*)
    range a max n 

(*
 range_far_zero 0. 9. 5.;;
- : float list = [0.; 2.; 4.; 6.; 8.; 10.]
# range_far_zero 0. 20. 2.;;
- : float list = [0.; 10.; 20.]
# range_far_zero 0. 20. 5.;;
- : float list = [0.; 4.; 8.; 12.; 16.; 20.]
# range_far_zero 1. 20. 10.;;
- : float list = [1.; 3.; 5.; 7.; 9.; 11.; 13.; 15.; 17.; 19.; 21.]
# range_far_zero 1. 15. 3.;;
- : float list = [1.; 6.; 11.; 16.]
 range_far_zero 0. 5. 3.;;
- : float list = [0.; 2.; 4.; 6.]
range_far_zero 0. 3. 2.;;
- : float list = [0.; 2.; 4.]
# range_far_zero 6. 8. 2.;;
- : float list = [6.; 7.; 8.]
# range_far_zero 6.5 10. 6.;;
- : float list = [6.5; 7.25; 8.; 8.75; 9.5; 10.25; 11.]
*)


(*
pour d = max - min proche de 0
*)


let rec max_list l = match l with 
    [] -> invalid_arg "liste vide dans max_list"
  |[a] -> a
  |h::t -> max  h  (max_list t)
(*
let max_list l = List.fold_left (fun x y -> if x < y then y else x) l 
*)


let u = [1;2;5;10]



(*
#  offset k 0.15 0.65;;
- : float = 0.5
#  offset k 0.15 0.65;;
- : float = 1.
#  offset k 0.15 0.65;;
- : float = 2.5
#  offset k 0.15 0.65;;
- : float = 5.
*)


let rec range_off a b n pas i = 
if i > n then [] 
else a+.(pas *. i) :: range_off a b n pas (i +.1.)

let range_offset u a b n = 
  let k a b= floor(log10 (delta a b)) in 
  let offset_a =
    floor ( a /. (u *. 10.** (k a b))) *. u *. 10.**(k a b) in 
  let offset_b =
    ceil ( b /. (u *.  10.** (k a b))) *. u *. 10.**(k a b) in
   let new_a = a -. offset_a in
  let new_b = b -. offset_b in  
  let pas = (new_b -. new_a) /. n in (offset_a,offset_b, (k a b))
 (* range_off new_a new_b n pas 0.*)


(*
 let k a b= log10 (delta a b) 
let offset_b candidat a b =  (floor ( b /. (candidat *.( 10.**(k a b))))) *. (candidat *. (10.**(k a b)))
let list_offset u a b i = List.map (offset_b a b) u  
 
let rec best_offset l = match l with 
    [] -> invalid_arg "liste vide dans max_list"
  |[a] -> a
  |h::t -> max h (best_offset t)*)

