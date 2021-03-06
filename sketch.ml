
(*

#use "topfind";;
#require "vg.svg";;


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
      ?(xmar=(1. , 1.))
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
    
  val label : Viewport.t -> (float -> float) -> image
    
  val cloud : Viewport.t -> (float * float) list -> image
    
  val curve : Viewport.t -> (float * float) list -> image
    
  
  
end

=
struct
  
  
  let gray = I.const Color.red
  let marqueur_abs = P.empty >> P.rect (Box2.v P2.o (Size2.v 0.05 0.05))
  let marqueur = I.cut marqueur_abs gray
      
  
  let cadre_vide =
    let square = P.empty >> P.rect (Box2.v  (P2.v 1. 1.) (P2.v 10. 10.)) in 
    let area = `O { P.o with P.width = 0.035; dashes = Some (0., [0.]); } in
    I.const (Color.gray 0.1) >> I.cut ~area square
      
  
  
  (*--------------------------------**)
      
  
  
  let rec list_aux n i f =
    if i >= n then [] 
    else f i :: list_aux n (i + 1) f
           
  let list_init n f = list_aux n 0 f 
      
  
  let range a b n =
    let delta = (b -. a) /. float n in
    list_init n (fun i -> a  +. float i *. delta )
      
  let points_x xmin xmax ymin n =
    range xmin xmax n
    |> List.map (fun x -> (x, ymin))
      
  
  let points_y xmin ymin ymax n =
    range ymin ymax n
    |> List.map (fun y -> (xmin,y))
      
  
  let rec table_aux f inf sup pas =
    if inf >= sup then [] else (inf , f inf) :: table_aux f (inf +. pas) sup pas  
                                 
  
  let table f inf sup pas =
    let n = (sup -. inf) /. pas in
    table_aux f inf sup n
      
  let val_x f inf sup c n = 
    List.map (fun (x,_) -> (x,c)) (table f inf sup n)
      
  let val_y  f inf sup c n = 
    List.map (fun (_,y) -> (c,y)) (table f inf sup n)
      
  
  
  let axis_x vp = 
    let f accu (x, y) =
      let p = Viewport.scale vp (x,y) in 
      let i = I.move p marqueur in 
      I.blend i accu in 
    List.fold_left f  cadre_vide (points_x (fst (Viewport.xlim vp))
                                           (snd (Viewport.xlim vp))
                                           (fst (Viewport.ylim vp)) 10)
      
  
  let axis vp = 
    let f accu (x, y) = 
      let p = Viewport.scale vp (x,y) in 
      let i = I.move p marqueur in 
      I.blend i accu in 
    List.fold_left f (axis_x vp) (points_y (fst (Viewport.xlim vp))  
                                           (fst (Viewport.ylim vp)) 
                                           (snd (Viewport.ylim vp)) 10)
      
  
  
  let open_sans_xbold =
    { Font.name = "Open Sans"; size = 1.0; weight = `W800; slant = `Normal}
    
  let glyphs = [ 53; 72; 89; 82; 79; 87; 4 ]
               
  
(*
let float_to_point data = 
  let point p = 
    let x = fst (List.hd data) in
    let y = snd (List.hd data) in 
    P2.v x y in  
list_init ((List.length data)- 1) point
*)
               
  
  (*"%glab"*)
               
  let fa_lab_x accu  position (data_x, _) = 
    I.blend (I.move position (
        let font = { open_sans_xbold with Font.size = 0.15 } in
        let text = Printf.sprintf "%g" data_x  in 
        I.const Color.black >> I.cut_glyphs ~text font glyphs
      )) accu
      
  (*prend une image et une position pour un un mettre le label*)
  let fa_lab_y accu position (_,data_y) = 
    I.blend (I.move  position (
        let font = { open_sans_xbold with Font.size = 0.15 } in
        let text = Printf.sprintf "%g" data_y  in 
        I.const Color.black >> I.cut_glyphs ~text font glyphs
      )) accu 
      
  

  let label_x vp f=
    let (xmin, xmax) =( Viewport.xlim vp ) in
    let (ymin, _) =  (Viewport.ylim vp) in
    let data_points = val_x f xmin xmax ymin 10. in 
    let plot_points =
      data_points
      |> List.map (Viewport.scale vp)
    in
    List.fold_left2 
      fa_lab_x
      (axis vp)
      plot_points
      data_points
      
  
  let label_y vp f = 
    let (ymin,ymax) = Viewport.ylim vp in
    let (xmin,_) = Viewport.xlim vp in
    let data_points = val_y f ymin ymax xmin 10. in 
    let plot_points =
      data_points
      |> List.map (Viewport.scale vp)  in
    List.fold_left2 
      fa_lab_y
      (axis vp)
      plot_points
      data_points
      
  
  let label vp f = I.blend (label_x vp f)  (label_y vp f)
      
  
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
      I.cut ~area p green 
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
    viewport : Viewport.t
  }
  
  let list_min = List.fold_left min infinity 
      
  let list_max = List.fold_left max neg_infinity 
      
  let list_min_max l = ( list_min l , list_max l )        
                       
  let scatter_plot l = 
    let xlim = list_min_max (List.map fst l) in
    let ylim = list_min_max (List.map snd l) in 
    let vp = Viewport.make ~xlim ~ylim () in 
    let image = 
      I.blend
        (Primitives.cloud vp l)      
        (Primitives.axis vp) 
    in
    { 
      image ;
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
                          
  
  let rec table_aux f inf sup n =
    if inf >= sup then [] else (inf , f inf) :: table_aux f (inf +. n) sup n  
                                 
  let table f inf sup n= 
    let nstep = (sup -. inf) /. n in
    table_aux f inf sup nstep        
      
  (* nstep : plus c'est proche de 0 , moins la courbe sera lisse. Fixé à 100*)
  let curve_plot ~xmin ~xmax f nstep = 
    let l =table f xmin xmax nstep in 
    let xlim = list_min_max (List.map fst l) in
    let ylim = list_min_max (List.map snd l) in 
    let vp = Viewport.make ~xlim ~ylim () in 
    let image = 
      I.blend
        (Primitives.label vp f)
        (Primitives.curve vp l)
    in
    { 
      image ;
      viewport = vp ;
    }
    
  
  
  
end


let rec list_rand_aux n i f r = 
  if n = i then []
  else (f r,f r) :: list_rand_aux n (i + 1) f r 
         
(* n la longueur de la liste , f la fonction et r pour appliquer random dessus . *)
let list_rand n f r = list_rand_aux n 0 f r 
(* list_init 3 (fun _ -> Random.int 20);;
   - : (int * int) list = [(16, 1); (3, 12); (18, 9)]*)
    

let funct x = x(* ( x +. 1. ) *. ( x +. 2. )*)
let f_carre x = x *. x 
                
let pi = 4.0 *. atan 1.0
(*f(x) = cos(pi.x).exp(x) sur [-1;1]*)
let f_bis x = (cos ( pi *. x)) *. (exp x)
                                  
let () =
  Plot.to_svg (Plot.scatter_plot (list_rand 100 Random.float 10.)) "scatter_plot.svg"
    
(*cos application partielle*)
let () =
  Plot.to_svg (Plot.curve_plot (-1.) (1.) cos 100.) "curve_plot_cos.svg"
let () =
  Plot.to_svg (Plot.curve_plot (-10.) (10.) sin 100.) "curve_plot_sin.svg"
let () =
  Plot.to_svg (Plot.curve_plot (2.) (10.) funct 100.) "curve_plot_funct.svg"
let () =
  Plot.to_svg (Plot.curve_plot (0.) (10.) f_carre 100.) "curve_plot_carre.svg"
let () =
  Plot.to_svg (Plot.curve_plot (1.) (20.) sqrt 100.) "curve_plot_sqrt.svg"
let () =
  Plot.to_svg (Plot.curve_plot (0.) (10.) f_bis 100.) "curve_plot_f_bis.svg"
    
let () =
  Plot.to_svg (Plot.curve_plot (0.) (10.) (fun x -> -.x) 100.) "fonction decreoissante.svg"



(*--------------------------*)
