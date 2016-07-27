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
    mantisse :float;
    exposant : int;

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

(*
let nn ns = Notation_scientifique.make 2.3 3 ()
let a ns =  Notation_scientifique.get_mantisse (nn ns)

            *)
let rec list_aux n i f =
  if i > n then [] 
  else f i :: list_aux n (i + 1) f
         
let list_init n f = list_aux n 0 f 
    
let abs a = -. a

let is_div x n = if (mod_float x n) = 0. then true else false
let delta a b n = (b -. a ) /. n
                  
let dt a b = b -. a      

(*cas ou b-a est divisible par n *)
let rec range a b n i = 
 (* let pas =  ((delta a b n) *. i) in*)
  if i > n then [] 
  else (a +. (delta a b n) *. i) :: range a b n  (i +. 1.)  
(*
tests
#  range 0. 20. 5. 0.;;
- : float list = [0.; 4.; 8.; 12.; 16.; 20.]
#   range 0. 10. 5. 0.;;
- : float list = [0.; 2.; 4.; 6.; 8.; 10.]
# range 0. 15. 3. 0.;;
- : float list = [0.; 5.; 10.; 15.]
# range 0. 15. 5. 0.;;
- : float list = [0.; 3.; 6.; 9.; 12.; 15.]
*)        


(*
fonction qui appelle range si b - a est divisible par n
 et qui appelle un autre traitement sinon 

la valeur max minimum doit etre 2 
*)    
let range_far_zero  a b n = 
  if (is_div (dt a b) n ) then
    range a b n 0. 
  else 
     let max = b +. 1. in 
    (* let min = a -. 1. in*)
    range a max n 0.

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



(*on cree un type notation scientifique avec la mantisse l'exposant et le couple mantisse exposant*)

type ns = {
  mantisse : float (* E [1 ; 10[*);
  exposant : int  (* E Z*);
  ns : float * int (* couple mantisse exposant *)
}
 let notation ns mant exp = {mantisse = mant; exposant = exp; ns = mant , exp}



(*
let max_list l = List.fold_left (fun x y -> if x < y then y else x) l 
*)

(*cas ou b - a < 2*)
(*
let c a b n i= max_list(range a b n i) *. 2. /. (mod_float n 2.)

let rec range_close_zero_aux  a b n i = 
   if i >= n then [] else
      (a +. (c  a b n i) *. i) :: range_close_zero_aux a b n  (i +. 1.)

*)
(*
let  range_close_zero a b n =
   if (b -. a < 2. ) then
     range_close_zero_aux  a b n 0.
else  range a b n 0. 
*)
