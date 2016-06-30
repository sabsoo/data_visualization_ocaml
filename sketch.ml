
(*
#use "topfind";;
#require "vg.pdf";;
  ocamlfind ocamlopt -package gg,vg,vg.svg \
                     -linkpkg -o sketch.native sketch.ml && ./sketch.native
*)

open Gg
open Vg

module Viewport : sig
  type t

  val make :
    ?view:Size2.t ->
    ?xmar:(float * float) ->
    ?ymar:(float * float) ->
    xlim:(float * float) ->
    ylim:(float * float) ->
    unit -> t
  

  val xlim : t -> float * float
  val ylim : t -> float * float
  val xmar : t -> float * float
  val ymar : t -> float * float
  val view : t -> Box2.t

  val scale_x : t -> float -> float
  val scale_y : t -> float -> float
  val scale : t -> float * float -> V2.t
 
end
=
struct
  type t = {
    view : Box2.t;
    xmar : float * float;
    ymar :float * float;
    xlim: float * float ;
    ylim: float * float ;
   
  }

  let make
      ?(view=Size2.v 10. 10.)
      ?(xmar=(1., 1.))
      ?(ymar=(1. , 1.))
      ~xlim 
      ~ylim
      ()
    = 
    {
      view = Box2.v P2.o view ;
      xmar ;(*xmar = xmar*)
      ymar ;
      xlim;
      ylim;
    }

  let xlim vp = vp.xlim
  let ylim vp = vp.ylim
  let xmar vp = vp.xmar
  let ymar vp = vp.ymar
  let view vp = vp.view
  let delta (x, y) = y -. x

  let scale_x vp x =
    (x -. fst vp.xlim) 
    *. (Box2.w vp.view -. fst vp.xmar -. snd vp.xmar) 
    /. delta vp.xlim +. fst vp.xmar

  let scale_y vp y =
    (y -. fst vp.ylim) 
    *. (Box2.h vp.view -. fst vp.ymar -. snd vp.ymar) 
    /. delta vp.ylim +. fst vp.ymar

  let scale vp (x, y) =
    V2.v (scale_x vp x) (scale_y vp y)

 
end


module Primitives : sig
  
  val axis : Viewport.t -> image

  val cloud : Viewport.t -> (float * float) list -> image

  val curve : Viewport.t -> (float * float) list -> image

end
=
struct
(*  val axis : Viewport.t -> image *)
let gray = I.const Color.red
let x_marqueur = P.empty >> P.rect (Box2.v P2.o (Size2.v 0.05 0.05))
let marqueur_abs = I.cut x_marqueur gray


let rec list_init_aux n i f r= 
  if n = i then []
  else f r :: list_init_aux n (i + 1) f r
 
let list_init n f r = list_init_aux n 0 f r
 
 (*
incremente de 0.1 le premier element du couple (x, y)
i est un compteur de type couple 
pas est le pas pour placer les marqueurs : 0.1 en exemple
 *)
let rec traitement_x x i pas = if  fst i >= fst x  then [] else (fst i, 0.) :: traitement_x x (fst i +.pas, 0.) pas

let rec traitement_y y i pas = if  snd i >= snd y  then [] else ( 0., snd i) :: traitement_y y (0., snd i +. pas) pas
(*
 (float * float) list =
[(0., 0.); (0., 0.2); (0., 0.4); (0., 0.600); (0., 0.8); (0., 1.); (0., 1.2); (0., 1.4); (0., 1.59); (0., 1.7999); (0., 1.999)]
*)

let cadre_vide =
  let square = P.empty >> P.rect (Box2.v  (P2.v 1. 1.) (P2.v 10. 10.)) in 
let area = `O { P.o with P.width = 0.01; dashes = Some (0., [0.]); } in
 I.const (Color.gray 0.1) >> I.cut ~area square

let axis_x vp =
  let f accu (x, y) =
    let p = Viewport.scale vp (x,y) in 
    let i = I.move p marqueur_abs in 
    I.blend i accu in 
  List.fold_left f cadre_vide  (traitement_x (2. , 0.) (0. , 0.) 0.2)

let axis vp = 
  let f accu (x, y) = 
    let p = Viewport.scale vp (x,y) in 
    let i = I.move p marqueur_abs in 
    I.blend i accu in 
  List.fold_left f (axis_x vp) (traitement_y (0. , 2.) (0. , 0.) 0.2)

 
 (*----------------------------------------------------*)
  let gray = I.const (Color.gray 0.5)
  let circle = P.empty >> P.circle P2.o 0.05
  let gray_circle = I.cut circle gray

  let circle_outline =
    let area = `O { P.o with P.width = 0.03 } in
    let black = I.const Color.red in
    I.cut ~area  circle black
      
  let point = I.blend  gray_circle circle_outline 


  let cloud vp l =
    let f accu (x, y) =
      let p = Viewport.scale vp (x,y) in 
      let i = I.move p point in 
      I.blend i accu 
    in
    List.fold_left f (axis vp) l


let map_p data vp= List.map (Viewport.scale vp) data

  let curve vp data =
    let points = List.map ( Viewport.scale vp) data in
    match points with
    | [] -> I.const Color.void
    | h :: t ->
      let f accu pt = 
        accu >> P.line pt
      in
      let p = List.fold_left f (P.sub h P.empty) points
      in
      let area = `O { P.o with P.width = 0.02 } in
      let green = I.const Color.green in
      let im = I.cut ~area p green in 
      I.blend (axis vp) im
      (* List.fold_left (funct vp acc e ->f vp acc e) P.empty l *)
        
end

module Plot : sig
  type t
 
  val to_svg : t -> string -> unit
  val scatter_plot : (float * float) list -> t
  val curve_plot : xmin:float -> xmax:float -> (float -> float) -> float -> t
end
=
struct 

  type t = {
     image : image ;
  (* axis : image ;*)
    viewport : Viewport.t
  }

  let list_min = List.fold_left min infinity 

  let list_max = List.fold_left max neg_infinity 

  let list_min_max l = ( list_min l , list_max l )        

  let scatter_plot l = 
    let xlim = list_min_max (List.map fst l) in
    let ylim = list_min_max (List.map snd l) in 
    let vp = Viewport.make ~xlim ~ylim () in 
    { 
     image = Primitives.cloud vp l ;
     viewport = vp ;
    }

  let to_svg plot out =
    let view = Viewport.view plot.viewport in
    let size = Size2.v 150. 150. in 
    try
      let oc = open_out out in
      let r = Vgr.create (Vgr_svg.target ()) (`Channel oc) in
      try
        ignore (Vgr.render r (`Image (size, view, plot.image)));
        ignore (Vgr.render r `End);
      close_out oc
    with e -> close_out oc; raise e
  with Sys_error e -> prerr_endline e

 
let rec table f inf sup pas = if inf > sup then [] else (inf , f inf) :: table f (inf +. pas) sup pas  

(* nstep pour le nombre de pas entre xmin et xmax , plus c'est proche de 0 , moins la courbe sera lisse. Fixé à 100*)
let curve_plot ~xmin ~xmax f nstep = 
  let l = table f xmin xmax ((xmax -. xmin) /. nstep ) in 
  let xlim = list_min_max (List.map fst l) in
  let ylim = list_min_max (List.map snd l) in 
  let vp = Viewport.make ~xlim ~ylim () in 
 { 
    image = Primitives.curve vp l ;
    viewport = vp ;
  }


end


let rec list_init_aux n i f r= 
  if n = i then []
  else (f r,f r) :: list_init_aux n (i + 1) f r

(* n la longueur de la liste , f la fonction et r pour appliquer random dessus . *)
let list_init n f r = list_init_aux n 0 f r
(* list_init 3 Random.int 20;;
- : (int * int) list = [(16, 1); (3, 12); (18, 9)]*)

let cadre_vide width = 
  let square = P.empty >> P.rect (Box2.v P2.o (P2.v 0.95 0.95)) in
  let area = `O { P.o with P.width = width; dashes = Some (0., [0.]); } in
  I.const (Color.gray 0.1) >> I.cut ~area square

let funct x = ( x +. 1. ) *. ( x +. 2. )
let f_carre x = x *. x 

let () =
  Plot.to_svg (Plot.scatter_plot (list_init 100 Random.float 1.)) "scatter_plot.svg"
(*cos application partielle*)
let () =
  Plot.to_svg (Plot.curve_plot (0.) (1.) cos 100.) "curve_plot_cos.svg"
let () =
  Plot.to_svg (Plot.curve_plot (0.) (1.) sin 100.) "curve_plot_sin.svg"
let () =
  Plot.to_svg (Plot.curve_plot (0.) (1.) funct 100.) "curve_plot_funct.svg"
let () =
  Plot.to_svg (Plot.curve_plot (0.) (1.) f_carre 100.) "curve_plot_carre.svg"
let () =
  Plot.to_svg (Plot.curve_plot (0.) (1.) sqrt 100.) "curve_plot_sqrt.svg"
