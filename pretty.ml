

  let rec list_aux n i f =
    if i > n then [] 
    else f i :: list_aux n (i + 1) f
           
  let list_init n f = list_aux n 0 f 
      
  let abs a = -. a


(*
version ac des entiers 
let delta_ent a b = b - a

let rec det_n a b i = 
  if ( (delta_ent a b)+1 == i)  then []
  (*else if (delta a b =( 2 * a)) then  2(*(b - a / 2) *)
  else if ( delta a b >= (3 * a) ) then 3(* ( b - a / 3)*)*)
  else (a+i) ::  det_n a b (i + 1)
 *)

let delta a b = b -. a

let rec range_far_zero a b i = 
  if ( (delta a b) +. 1.)  <= i  then []
  (*else if (delta a b =( 2 * a)) then  2(*(b - a / 2) *)
  else if ( delta a b >= (3 * a) ) then 3(* ( b - a / 3)*)*)
  else (a +. i) :: range_far_zero a b (i +. 1.)


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
