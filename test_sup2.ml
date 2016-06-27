(*
 ocamlfind ocamlopt -package gg,vg,vg.svg \
                     -linkpkg -o min_svg.native test_sup.ml

#use "topfind";;
#require "vg.pdf";;

(*
let circle =
      let circle = P.(empty >> circle (P2.v 0.5 0.5) 0.4) in
      let area = `O { P.o with P.width = 0.1 } in
      I.cut ~area circle (I.const Color.black) >>
      I.scale (Size2.v 0.5 1.)

let () = svg_of_usquare "circlclc.svg" circle
*)

let c = P2.v 0.5 0.5
 let area = `O { P.o with P.width = 0.01 }

    let p =
      P.empty >>
      P.ellipse c (V2.v 0.1 0.2) >>
      P.circle c 0.25 >>
      P.rect (Box2.v (P2.v 0.2 0.15) (Size2.v 0.6 0.7)) >>
      P.rrect (Box2.v (P2.v 0.1 0.05) (Size2.v 0.8 0.9)) (Size2.v 0.2 0.1)
    
  
 let cutage = I.const (Color.gray 0.3) >> I.cut ~area p
  let () = svg_of_usquare "cutage.svg" cutage
*)

open Gg
open Vg

(*cree un rectangle.svg dans le dossier courant *)
let gray = I.const Color.blue
let grayyy= I.const (Color.gray 0.6)
let grayy = I.const (Color.gray 0.1) 
let constant_image = I.const Color.green(* constant infinite imageet argument apres color.gray caracterise l intensité de la couleur , plus c 'est proche de 0 plus c est sombre,  *)

let svg_of_usquare out view i =
  let size = Size2.v 150. 150.(*premier arg longueur et 2eme largeur*) in 
  try
    let oc = open_out out in
    let r = Vgr.create (Vgr_svg.target ()) (`Channel oc) in
    try
      ignore (Vgr.render r (`Image (size, view, i)));
      ignore (Vgr.render r `End);
      close_out oc
    with e -> close_out oc; raise e
  with Sys_error e -> prerr_endline e




(*sur le rectangle precedent on decoupe un disque*)
let circle = P.empty >> P.circle (P2.v 0.5 0.5)(*cercle v avec centre 0.5 et 0.5 et rayon 0.4*) 0.4
let gray_circle = I.cut circle gray
(*let () = svg_of_usquare "gray_circle.svg" gray_circle (*a appeler pour appliquer le renderer a chaque fois*)*)
    
let square = P.empty >> P.rect (Box2.v P2.o (Size2.v 1. 1.)) 
let gray_square = I.cut square gray
(*let () = svg_of_usquare "rectdansrec.svg" gray_square*)
    
let angle = Float.rad_of_deg 60.
let rayon =  Size2.v 0.6 0.2
let ellipse = P.empty >> P.ellipse ~angle P2.o rayon
let ellipse_from_square = I.cut ellipse gray
    
(*sur le disque precedent, on decoupe un cercle qui est le pourtour du disque*)
let circle_outline =
  let area = `O { P.o with P.width = 0.09 } in
  let black = I.const Color.red in
  I.cut ~area circle black
    
(*super glue entre le disque et le pourtour !association a gauche , sens derriere -> devant*)
let dot = I.blend circle_outline gray_circle
    
(*diagramme de dispersion de points , prend une liste de points et renvoie un diagramme de dispersion de ces points *)
let scatter_plot pts pt_width =
  let dot =
    let circle = P.empty >> P.circle P2.o (0.5 *. pt_width) in
    I.const Color.black >> I.cut circle  in
  let mark pt = dot >> I.move pt in
  let blend_mark acc pt = acc >> I.blend (mark pt) in
  List.fold_left blend_mark I.void pts
    
let normal_pts count =
  let s = Random.State.make [|18278|] in
  let rand = Float.srandom s in
  let normal_pt () =                           (* Box-Muller transform. *)
    let u1 = rand ~len:1.0 () in
    let u2 = rand ~len:1.0 () in
    let z0 = sqrt (-. 2. *. log u1) *. cos (Float.two_pi *. u2) in
    let z1 = sqrt (-. 2. *. log u1) *. sin (Float.two_pi *. u2) in
    P2.v z0 z1
  in
  let acc = ref [] in
  for i = 1 to count do
    acc := V2.((P2.v 0.5 0.5) + 0.125 * normal_pt ()) :: !acc
  done;
  !acc



(*modifier 1. et 1. reduire de 0.1 pour le titre , faire des modulo *)
let cadre_vide width = 
  let square = P.empty >> P.rect (Box2.v P2.o (P2.v 0.95 0.95)) in(*on donne moins de 1 pour que le reste soit occupé par le titre , P2. o pour le mettre tout a gauche , sinon mettre composant*)
  let area = `O { P.o with P.width = width; dashes = Some (2., [0.]); } in
  I.const (Color.gray 0.6) >> I.cut ~area square

let () = svg_of_usquare "cadre_vide.svg" Box2.unit (cadre_vide 0.01)

let x_marqueur = P.empty >> P.rect (Box2.v P2.o (Size2.v 0.005 0.015)) 
let marqueur_abs = I.cut x_marqueur gray
    
let t_marqueur pt = marqueur_abs >> I.move pt
 
(*une image qui cree la marque a cette position et qui l ajoute dans accu *)
let fa_abs accu position =  I.blend (t_marqueur position) accu
    
(*fonction qui prend un float et cree un point en ajoutant 0.1 au composant x*)
let traitement_x x =
  let z0 = x+.0.1  in 
  let z1 = 0.  in
  P2.v z0 z1
(*a enlever sert a rien , surtout acc >=1.
let rec t x acc = if acc >= 1. then traitement_x x else t x (acc +. 0.1)
      
(*fonction qui applique le traitement jusqu' a 1*)
let rec l_traitement_x x = t x 0.*)
    
(*fonction qui cree une liste de taille n et lui applique la fonction f ; c est l'argument qui permet de prendre un nouveau point à chaque fois*)
let rec l_funct n i f c = 
  if n = i then []
  else f c :: l_funct n (i + 1) f (c+. 0.094)

let list_init n f c = l_funct n 0 f c
    
let cadre_x n x = List.fold_left fa_abs (cadre_vide 0.01) (list_init n traitement_x x) (*Gg.p2 list*)
let () = svg_of_usquare "cadre_x.svg" Box2.unit (cadre_x 9 0.)

let y_marqueur = P.empty >> P.rect (Box2.v P2.o (Size2.v 0.015 0.005)) 
 
let marqueur_ord = I.cut y_marqueur gray
 (* let () = svg_of_usquare "marqueur_ord.svg" marqueur_ord   *)
let t_marqueur_ord pt = marqueur_ord >> I.move pt

(*une image qui cree la marque a cette position et qui l ajoute dans accu *)
let fa_ord accu position =   I.blend (t_marqueur_ord position) accu
    
(*fonction qui prend un float et cree un point en ajoutant 0.1 au composant y*)
let traitement_y y =
  let z0 = 0.  in 
  let z1 = y +. 0.1  in
  P2.v z0 z1
let rec t y acc = if acc >= 0.9 then traitement_y y else t y (acc +. 0.1)
let rec l_traitement_y y = t y 0.

(*argument n pour le nombre de marqueur et y pour le debut du marquage*)    
let cadre n y = List.fold_left fa_ord (cadre_x 9 0.0) (list_init n l_traitement_y y) (*Gg.p2 list*)
let () = svg_of_usquare "cadrons.svg" Box2.unit (cadre 9 0.0)
    
(* nuage de points*)

let cp x y = P.empty >> P.circle (P2.v x y)(*cercle v avec centre 0.5 et 0.5 et rayon 0.4*) 0.005 
let gcp = I.cut (cp 0.0 0.0) grayyy
(*let () = svg_of_usquare "sss.svg" gcp*)
let point =
  let area = `O { P.o with P.width = 0.001 } in
  let black = I.const Color.red in
  I.cut ~area (cp 0.0 0.0) black

let m = I.blend point gcp
(*let () = svg_of_usquare "marque.svg" m*)
let tm pt = m >> I.move pt

(*let () = svg_of_usquare "tm.svg" (tm  (V2.v 0.2 0.6) )*) 

(*une image qui cree le point a cette position et qui l ajoute dans accum *)
let fa_p accu_m position_m =  I.blend (tm position_m) accu_m
let () = svg_of_usquare "r.svg" Box2.unit (fa_p (cadre 9 0.0) (V2.v 0.1 0.4))

    
let rec nieme l i =
match l with
 [] -> failwith "nieme"
   | a::r -> if i<0 then failwith "nieme"
     else if i=0 then a else nieme r (i-1)

let is_empty liste =
 match liste with
[] -> true
|_ -> false 

let rec length l = 
match l with 
[] -> 0
|_::r -> 1+ length (r) 

let posx = [0.1;0.2;0.3;0.4;0.5;0.6;0.1;0.2;0.8;0.85]
let posy = [0.4;0.5;0.1;0.6;0.23;0.5;0.1;0.2;0.8;0.85]


let rec randomiseur r i acc= if i = acc then [] else (Random.float r ) :: randomiseur r i (acc+1)
let random_l r = randomiseur r 500 0


let traitement_point la lb i =
  let z0 = (nieme la i)  in 
  let z1 = (nieme lb i)  in
  P2.v z0 z1

let rec traitement_p la lb x i= 
  if i>x then []
  else traitement_point la lb i :: traitement_p la lb x (i + 1)

let traitement la lb = traitement_p la lb ((length la) -1 ) 0

let nuage_de_points = List.fold_left fa_p (cadre 9 0.0) (traitement (random_l 0.94) (random_l 0.94) )


(*
utiliser map au lieu de nieme car recherche dans une liste fastidieuse ,
 nettoyer et factoriser le code, 
gerer les intervalles si jamais en dehors de 0 1
faire une legende
*)

(*titre du graphique*)
let open_sans_xbold =
  { Font.name = "Open Sans"; size = 1.0; weight = `W200; slant = `Normal}

let glyphs = [ 53; 72; 89; 82; 79; 87; 4 ]

let titre size =
    let font = { open_sans_xbold with Font.size = size } in
    let text = "Diagramme de dispersion" in
    I.const Color.black >> I.cut_glyphs ~text font glyphs >>
   (* I.move (V2.v 0.4 0.7)*)
    I.move (V2.v 0.3 0.97) 

let a = I.blend nuage_de_points (titre 0.02)

let () = svg_of_usquare "titre_diag.svg" Box2.unit a

(******)

(*
let a = Box2.min vu
let ax = Box2.minx vu

let b = Box2.max vu
let bx = Box2.maxx vu
let mid = Box2.mid vu
let midx = Box2.midx vu
let s = Box2.size vu*)

(*
(*
val round : Gg.box2 -> Gg.box2
round b is the smallest box containing b with integer valued corners. 
*)
let r = Box2.round vu

(*
val tr : Gg.m3 -> Gg.box2 -> Gg.box2
    tr m b is the smallest box containing the corners of b transformed by m in homogenous 2D space.
*)
let t = Box2.tr M3.id unit_vue
*)

let vu = Box2.v_mid P2.o (Size2.v 2. 2.)
let unit_vue = Box2.unit

(* val move : Gg.v2 -> Gg.box2 -> Gg.box2 *)
let m = Box2.move (V2.v 0.1 0.1) unit_vue 
let box = Box2.v (P2.v 0.1 0.1) (Size2.v 1. 1.)

(*cree un chemin selon box, ensuite decoupe sur image le chemin cree pour avoir une image, ensuite mise à l'echelle puis move pour future legende*)
let travail box image = I.move(V2.v 0.1 0.1) ( I.scale (V2.v ((1. /. Box2.w box) -. 0.1) ((1. /. Box2.h box) -. 0.1)) ( I.cut ( P.empty >> P.rect(box) ) image))

(*
let () = svg_of_usquare " travail.svg" (travail unit_vue constant_image)*)

let stb box image image2 =I.blend (travail box image) image2

(*unit_vue pour la vue , a  est l image du diagramme avec le titre et constant_image est l'image en arriere plan*)
let () = svg_of_usquare "stb.svg" Box2.unit (stb unit_vue a constant_image)

let grayb = I.const (Color.gray 0.5)
let x_marqueurb = P.empty >> P.rect (Box2.v P2.o (Size2.v 0.005 0.015)) 
let marqueur_absb = I.cut x_marqueurb grayb
   let () =svg_of_usquare "marqueurb.svg" Box2.unit  marqueur_absb 



(*test path*)
    let g = I.const (Color.gray 0.3)
    let white = I.const Color.white 
    let wedge =
      P.empty >>
      P.sub (P2.v 0.2 0.) >> P.line (P2.v 0.5 0.5) >> P.line (P2.v 0.8 0.)
   
    let path x join =
      let area = (`O { P.o with P.width = 0.2; join }) in
      let outline = I.cut ~area wedge g in
      let data = I.cut ~area:(`O { P.o with P.width = 0.01 }) wedge white in
      outline >> I.blend data >> I.move (P2.v x 0.2) 

  let path_test =  (path 0. `Round) >> I.blend (path 1. `Round) >> I.blend (path 2. `Bevel)
 
let () = svg_of_usquare "path_test.svg" Box2.unit path_test


(*test_subpath*)
(*idee , prendre cette fonciton et creer a vec le prmier sub la ligne de courbe , exemple les arguments de sub peuvent etre x et y , ensuite fonction recursive qui repete cette action jusqu a la fin de la liste*)
    let p =
      let rel = true in
      P.empty >>
      P.sub (P2.v 0.1 0.5) >>
        P.line (P2.v 0.3 0.5) >>
        P.qcurve ~rel (P2.v 0.2 0.5) (P2.v 0.2 0.0) >>
        P.ccurve ~rel (P2.v 0.0 (-. 0.5)) (P2.v 0.1 (-. 0.5)) (P2.v 0.3 0.0) >>
        P.earc ~rel (Size2.v 0.1 0.2) (P2.v 0.15 0.0) >>
      P.sub (P2.v 0.18 0.26) >>
        P.qcurve ~rel (P2.v (0.01) (-0.1)) (P2.v 0.1 (-. 0.05)) >>
        P.close >>
      P.sub (P2.v 0.65 0.8) >>
      P.line ~rel (P2.v 0.1 (-. 0.05))

 let area = `O { P.o with P.width = 0.01 } 
let p_area =  I.const Color.black >> I.cut ~area p

let () = svg_of_usquare "p_area.svg" Box2.unit p_area


(*test bordure *)
let square_t = P.empty >> P.rect (Box2.v P2.o (P2.v 1. 1.))
let area = `O { P.o with P.width = 0.2; dashes = Some (0., [0.05]); }
let test_bord = I.const (Color.gray 0.3) >> I.cut ~area square
let () = svg_of_usquare "test_bord.svg" Box2.unit test_bord


let chemin =  
  let rel = true in P.empty >> P.sub (P2.v 0.1 0.1) >> 
  P.line (P2.v 0. 0.) >>  P.line ~rel (P2.v 0.1 0.102) >> 
  P.qcurve ~rel (P2.v 0.2 0.5) (P2.v 0.2 0.0) >>  
  P.ccurve ~rel (P2.v 0.0 (-. 0.5)) (P2.v 0.1 (-. 0.5)) (P2.v 0.3 0.0) >>P.close
let p_area = I.cut chemin (I.const Color.black)
    
let () = svg_of_usquare "chemin.svg" Box2.unit p_area

let cheminn  = let rel = true in  
 P.empty >> P.sub (P2.v 0.1 0.1) >> P.line (P2.v 0.101 0.1) >> P.line ~rel (P2.v 0.101 0.10) 
let  pp_area = I.cut cheminn (I.const Color.black)
let () = svg_of_usquare "cheminn.svg" Box2.unit pp_area




let cheminn_pas pas  = let rel = true in  
 P.empty >> P.sub (P2.v (0.1+.pas)  (0.1+. pas)) >> P.line P2.o >> P.line ~rel (P2.v 0.101 0.10)

let rec cheminot pas c =  if c = 0 then cheminn_pas pas else cheminot pas (c - 1)
let pas_area_c pas c= I.cut (cheminot pas c ) (I.const Color.black) 
let paspap pas c = I.blend (pas_area_c pas c) gray

let () = svg_of_usquare "chemin_pas.svg" Box2.unit (paspap 0.1 10)