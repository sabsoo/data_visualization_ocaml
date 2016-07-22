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

let rec range_f i n =  if i > n then [] 
  else  i :: range_f (i +. 1.) n  
     

(*
fonction qui appelle range si b - a est divisible par n
 et qui appelle un autre traitement sinon 
*)    
let range_far_zero  a b n = 
  if (is_div (dt a b) n ) then
    range a b n 0. 
  else range_f 0. n 



(**pour les valeurs proches de 0*)

(*
let range_close_zero a b n =
  let delta =
    let d a b =  b -. a in 
    (d a b )/. float n in
  list_init n (fun i -> a  +. float i *. delta )
    
(*
let verif x y =assert ((d x y )> 0. && (d x y) < 0.5)
*)
let rec pretty_aux (x,y) n i e =
  let d x y = y -. x in 
  let c x y n = (d x y ) /. n in 
  let a b c n= (c -. b /. n) +. 1. in
  if (i>=n ) then []
  else if ((d x y )> 0. && (d x y) < 0.5) then  [c x y n]
  else 
    (a x y n) :: pretty_aux (x,y) n (i +. 1.) (e +. ( c x y n))
      
  (*
let pretty (x,y) n = 
    if (x >= y) then pretty_aux (x,y) n n
else pretty_aux(c,y) n 0.
*)

(*en supposant qu on ne passe pas de valeurs positives à negatives*)
let abs a = if a >= 0.0 then a else -.a
let fmax2 x y = max (abs x) (abs y)
    
let i_small = true
let cell = 0.

let small x y = 
let delta = y -. x in 
  if (delta ==y || delta == x)   then  cell = 1. 
else cell = fmax2 x y 
       
*)
