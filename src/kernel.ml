
open Graphics
open Png


let adresse = "D:\\Programmation\\OCaml\\StarWars"


(* code du jeu *)


let deltay = 0

let (boom_img1, boom_img2, boom_img3) =
  begin
    open_graph "1x1";
    let res = (
      set_color_transp black (fst (get_img_bmp
                                     (adresse ^ "\\include\\boom1"))),
      set_color_transp black (fst (get_img_bmp
                                     (adresse ^ "\\include\\boom2"))),
      set_color_transp black (fst (get_img_bmp
                                     (adresse ^ "\\include\\boom3"))) ) in
    close_graph ();
    res
  end


(* paramètres : *)

let element e tab =
  let t = Array.length tab in
  let i =ref 0 in
  while !i < t && tab.(!i) <> e do incr i done;
  if !i = t then None else Some !i

let iof x = int_of_float (x+.0.5)

let foi x = float_of_int x

let ri = Random.int

let dist_d (a,b) (c,d) = max (abs (a - c)) (abs (b-d))



let mat_chemin_of_mat_adj mat_adj =
  let taille = Array.length mat_adj in
  let res = Array.make (taille / 2 + 1) [| [| |] |] in
  let mat0 = Array.make_matrix taille taille (-1) in
  for i = 0 to taille - 1 do
    for j = 0 to taille - 1 do
      if mat_adj.(i).(j) then mat0.(i).(j) <- j
    done
  done;
  res.(0) <- mat0;
  for n = 1 to taille / 2 do
    let mat = Array.make_matrix taille taille (-1) in
    for i = 0 to taille - 1 do
      for j = 0 to taille - 1 do
        let k = ref 0 in
        while !k < taille && (res.(n-1).(i).(!k) = -1 || not mat_adj.(!k).(j))
        do incr k done;
        mat.(i).(j) <- if !k = taille then -1 else res.(n-1).(i).(!k)
      done
    done;
    res.(n) <- mat
  done;
  res


type culture = Neanderthal | Fermier | Industriel | Riche | Adaptee

type joueur =
  {
    vit:float;
    def:float;
    atk:float;
    vol:float;
    terrifiant:bool;
    defenseur:bool;
    furtif:bool;
    dipl_vol:bool;
    dipl_atk:bool;
    dipl_def:bool;
    civi:culture;
    conquerant:bool
  }

type etoile =
  {
    x:int;
    y:int;
    r:int;
    prod:int;
    num:int;
    mutable control:int;
    mutable pop:int
  }

type vaisseau =
  {
    camp:int;
    mutable sens:int;
    mutable avancee:float;
    mutable charge : int
  }

type arete =
  {
    depart:int;
    arrivee:int;
    long:float;
    dist:float;
    mutable trafic:vaisseau list
  }
  
type lien = Rien | Direct of arete | Indirect of arete

type niveau = (int * int * int * int * int * int) array *
    bool array array * joueur * joueur * int * int

let victoire = ref 0

let vrai_revenu prod = function
  | Neanderthal -> 0
  | Fermier -> 1
  | Industriel -> 2
  | Adaptee -> prod
  | Riche -> if prod <= 1 then 1 + prod else prod

let equilibrer force p =
  if p.vit <= 0. then failwith "vitesse negative";
  if force <= 0. then p else begin
    let srd b f = if b then f else 1. in
    let norme = (p.atk +. p.def +. p.vol) /. 3. in
    let coef = force *.
               (p.vit ** (-.1./.3.2)) /. norme *.
               srd p.defenseur 0.87 *.
               srd p.terrifiant 0.94 *.
               srd p.dipl_def 0.95 *.
               srd p.dipl_vol 0.92 *.
               srd p.dipl_atk 0.95 *.
               srd p.furtif 0.96 *.
               srd p.conquerant 0.95 *.
               match p.civi with
               | Neanderthal -> 4.
               | Adaptee -> 1.
               | Riche -> 0.80
               | Industriel -> 0.85
        |Fermier -> 2. in
    {atk=p.atk *. coef; def=p.def *. coef; vol=p.vol *. coef;
    vit=p.vit; conquerant=p.conquerant; furtif=p.furtif; dipl_vol=p.dipl_vol;
    dipl_def =p.dipl_def; dipl_atk=p.dipl_atk; defenseur=p.defenseur;
    terrifiant=p.terrifiant; civi=p.civi } end;;


let jeu ?(temps_paye=16.) ?(nbet=0.005) ?(vitess=1.) ?(control_lettres=1)
    ?(touches_j1=[|'a';'q';'w';'z';'s';'x';'e';'d';'c';'r';'f';'v';'t';'g';'b'|])
    ?(touches_j2=[|'1';'2';'3';'4';'5';'6';'7';'8';'9';'+';'*';'-';'/';'0';'.'|])
    ?(control_chiffres=2) ?(control_souris=3) ?(ordi=[|false;false;false|])
    ?(force_ordi=0.) ~niveau:(niveau:niveau) ~j1:j1 ~j2:j2 ~j3:j3 () =

  let (c1,c2,c3) = (control_lettres-1, control_chiffres-1, control_souris-1) in
  let prb_mouse = ref (0,0) in
  victoire := 0;
  let vitesse = ref (0.15 /. vitess) in
  Random.self_init ();
  (* constantes utiles *)
  let (tab_univers, mat_adj, neutre, j4bis,taillex,tailley) = niveau in
  let j4 = equilibrer force_ordi j4bis in
  let taille = Array.length tab_univers in
  let mat_chemin = mat_chemin_of_mat_adj mat_adj in
  let univers = 
    let res = Array.make taille {x=0;y=0;r=0;prod=0;num=0;control=0;pop=0} in
    for i = 0 to taille - 1 do
      let (x,y,r,prod,pop,c) = tab_univers.(i) in
      res.(i) <- {x=x;y=y;r=r;prod=prod;num=i;control=c;pop=pop}
    done; res in
  let j = [|neutre;j1;j2;j3;j4|] in
  let touches1 = Array.make taille 'a' in
  let touches2 = Array.make taille 'a' in
  for i = 0 to taille - 1 do
    touches1.(i)<- touches_j1.(i);
    touches2.(i)<- touches_j2.(i)
  done;
  let grey = rgb 200 200 200 in
  let tab_aretes =
    let res = Array.make_matrix taille taille Rien in
    for i = 0 to taille - 1 do
      for j = i+1 to taille - 1 do
        if mat_adj.(i).(j) then begin
          let dx = abs (univers.(j).x - univers.(i).x) in
          let dy = abs (univers.(j).y - univers.(i).y) in
          let l = sqrt ( float_of_int (dx*dx+dy*dy)) in
          let a = {depart=i; arrivee=j; long=l;
                   dist = l -. float_of_int (univers.(j).r + univers.(i).r);
                   trafic=[]} in
          res.(i).(j) <- Direct a;
          res.(j).(i) <- Indirect a end
      done;
    done;
    res
  in

  let evenements = ref [] in

  let actu_evenements () =
    let rec aux = function
      | [] -> []
      | (x,y,t)::q ->
        if t <= 0 then aux q else (x,y,t-1)::(aux q) in
    evenements := aux !evenements in
  
  let aff_evenements () =
    let rec aux = function
      | [] -> ()
      | (x,y,t)::q ->
        draw_image (
          if      t >= 50 then boom_img1
          else if t >= 30 then boom_img2
          else boom_img3) (x - 25) (y - 25);
        aux q in
    aux !evenements in
  
  (* fonctions utiles *)
  let sous_reserve_dipl_atk degats nbj=
    if not j.(nbj).dipl_atk then degats
    else match ri 8 with
      | 0 -> -. degats
      | 1 -> 0.
      | _ -> degats in
  
  let sous_reserve_dipl_vol degats nbj=
    if not j.(nbj).dipl_vol then degats
    else match ri 8 with
      | 0 -> -. degats
      | 1 -> 0.
      | _ -> degats in

  let sous_reserve_dipl_def degats nbj=
    if not j.(nbj).dipl_def then degats
    else match ri 8 with
      | 0 -> -. degats
      | 1 -> 0.
      | _ -> degats in

  let affiche_etoile e =
    set_color [|grey;blue;red;green;magenta|].(e.control);
    fill_circle e.x e.y e.r;
    set_color [|black;yellow;green;red;yellow|].(e.control);
    moveto (e.x - 7) (e.y + 9);
    draw_char touches1.(e.num);
    moveto (e.x - 7) (e.y - 23);
    draw_char touches2.(e.num);
    let txt = (string_of_int e.pop ^ " / " ^ (string_of_int e.prod)) in
    let (xt,_) = text_size txt in
    moveto (e.x - xt / 2) (e.y - 8);
    draw_string txt in
  
  let get_coord ar av =
    let (d, a) = (ar.depart, ar.arrivee) in
    let c = (av +. float_of_int (univers.(d).r) ) /. ar.long in
    univers.(d).x + int_of_float (c *. float_of_int (univers.(a).x - univers.(d).x))
  , univers.(d).y + int_of_float (c *. float_of_int (univers.(a).y - univers.(d).y))
  in
  
  let aff_lien = function
    | Rien -> ()
    | Direct ar
    | Indirect ar ->
      let (d, a) = (ar.depart, ar.arrivee) in
      set_color white;
      moveto univers.(d).x univers.(d).y;
      lineto univers.(a).x univers.(a).y;
      let rec aux = function
        | [] -> ()
        | v::q -> begin
            set_color [|grey;blue;red;green;magenta|].(v.camp);
            let (px,py) = get_coord ar v.avancee in
            fill_circle px py 10;
            set_color white;
            moveto (px +10) (py + 10);
            draw_string (string_of_int v.charge);
            aux q
          end in
      aux ar.trafic in
  
  let lance_vaisseau camp charge depart arrivee =
    match tab_aretes.(depart).(arrivee) with
    | Rien -> failwith "erreur, on veut lancer un vaisseau dans le vide"
    | Direct ar ->
      ar.trafic <- {camp=camp;sens=1;avancee=0.;charge=charge}::ar.trafic
    | Indirect ar ->
      ar.trafic <- {camp=camp;sens=(-1);avancee=ar.dist;charge=charge}::ar.trafic
  in
  
  let ai n =
    for i = 0 to taille - 1 do
      if univers.(i).control = n then begin
        let frontiere = ref false in
        let best_target = ref (-1) in
        let best_score = ref max_float in
        for jp = 0 to taille - 1 do
          if mat_adj.(i).(jp) && univers.(jp).control <> n then begin
            frontiere := true;
            let u = univers.(jp) in
            let vr = vrai_revenu u.prod j.(u.control).civi in
            match tab_aretes.(i).(jp) with
            | Rien -> failwith "mais ça devrait pas arriver"
            | Direct ar| Indirect ar -> begin
                let score =
                  (foi u.pop +. (foi vr +. 0.2) *. ar.dist /. (4. *. temps_paye)) *.
                  (max j.(u.control).def j.(u.control).vol) -. foi univers.(i).pop *.
                                                               (min j.(n).atk j.(n).vol) in
                let drev = vr - vrai_revenu univers.(i).prod j.(n).civi in
                let score2 = score -. foi
                               (if drev >=0
                                then (if score < 0.
                                      then 50*vrai_revenu u.prod j.(n).civi
                                      else 0)
                                else (20 * vrai_revenu u.prod j.(n).civi)) in
                if score2 < !best_score then (
                  best_score := score2;
                  best_target := if score < 0. then jp else (-1))
              end
          end
        done;
     if !best_target >= 0 then (
       lance_vaisseau n univers.(i).pop i !best_target;
       univers.(i).pop <- 0)
     else if not !frontiere
     then
       begin
         let rep = ref (-1) in
         let recherche = ref 1 in
         while !rep = -1 && !recherche < taille do
           for k = 0 to taille - 1 do
             if univers.(k).control <> n &&
                mat_chemin.(!recherche).(i).(k) <> (-1) then
               rep := mat_chemin.(!recherche).(i).(k)
           done;
           incr recherche
         done;
         if !recherche < taille then (
           lance_vaisseau n univers.(i).pop i !rep;
           univers.(i).pop <- 0)
       end
      end
    done in

  let rec cherche_bagarre v inf sup ar = function
    |[] -> ()
    |v2::q -> if v.camp <> v2.camp && v2.avancee >= inf && v2.avancee <= sup then begin
        let f = foi v.charge *. j.(v.camp).vol in
        let f2 = foi v2.charge *. j.(v2.camp).vol in
        if    f > f2 then begin
            if not j.(v2.camp).furtif || ri 3 <> 0 then
                (v.charge <- iof ((f -. sous_reserve_dipl_vol f2 v.camp) /. j.(v.camp).vol);
                    v2.charge <- 0; (let px, py = get_coord ar v2.avancee in
                        evenements := !evenements@[(px,py,60)]);
                    if j.(v2.camp).terrifiant && ri 3 = 0 then v.sens <- -v.sens) end
        else begin
            if not j.(v.camp).furtif || ri 3 <> 0 then
                (v2.charge <- iof ((f2 -. sous_reserve_dipl_vol f v2.camp) /. j.(v2.camp).vol); 
                    v.charge <- 0; (let px, py = get_coord ar v.avancee in
                        evenements := !evenements@[(px,py,60)]);
                    if j.(v.camp).terrifiant && ri 3 = 0 then v2.sens <- -v2.sens) end
        end;
        cherche_bagarre v inf sup ar q
  in

let actu_arete = function
    |Rien -> ([],[])
    |Direct ar | Indirect ar ->
begin
    let rec aux = function
    |[] -> ()
    |v::q ->
        if v.sens = 1 then begin
            let fin = v.avancee +. 4. *. j.(v.camp).vit in
            cherche_bagarre v v.avancee fin ar ar.trafic;
            v.avancee <- fin end
        else begin
            let fin = v.avancee -. 4. *. j.(v.camp).vit in
            cherche_bagarre v fin v.avancee ar ar.trafic;
            v.avancee <- fin end;
        aux q in
    aux ar.trafic;
    let arrives_a = ref [] in
    let arrives_d = ref [] in
    let rec aux2 = function
    |[] -> []
    |t::q -> if t.charge = 0 then aux2 q
        else if t.avancee < 0. then begin
            if univers.(ar.depart).control <> t.camp then
                (let px, py = get_coord ar 1. in
                    evenements := !evenements@[(px,py,60)]);
            if univers.(ar.depart).control <> t.camp &&
                j.(univers.(ar.depart).control).defenseur &&
                ri 3 = 0 then (t.sens <- - t.sens; t::(aux2 q))
            else    (arrives_d := t::!arrives_d; aux2 q) end
        else if t.avancee > ar.dist then begin
            if univers.(ar.arrivee).control <> t.camp then
                (let px, py = get_coord ar (ar.dist -. 1.) in
                    evenements := !evenements@[(px,py,60)]);
            if univers.(ar.arrivee).control <> t.camp && 
                j.(univers.(ar.arrivee).control).defenseur &&
                ri 3 = 0 then (t.sens <- - t.sens; t::(aux2 q))
            else (arrives_a := t::!arrives_a; aux2 q) end
        else t::(aux2 q) in
    ar.trafic <- aux2 ar.trafic;
    (!arrives_d, !arrives_a)
end
  in

  let actu_univers () =
    actu_evenements ();
    let arrivees = Array.make taille [] in
    for i = 0 to taille - 1 do
        for j = i + 1 to taille - 1 do
            let (a,b) = actu_arete tab_aretes.(i).(j) in
            arrivees.(i) <- a@arrivees.(i);
            arrivees.(j) <- b@arrivees.(j);
        done
    done;
    let rec aux planete = function
    |[] -> ()
    |t::q ->
        if t.camp = planete.control then
            planete.pop <- planete.pop + t.charge
        else
            let f = foi planete.pop *. j.(planete.control).def in
            let f2 = foi t.charge *. j.(t.camp).atk in
            if    f >= f2 then
                planete.pop <- iof ((f -. sous_reserve_dipl_def f2 planete.control)
                    /. j.(planete.control).def)
            else (planete.pop <- iof ((f2 -. sous_reserve_dipl_atk f t.camp) /. j.(t.camp).atk)
                    + if j.(t.camp).conquerant then 6 * planete.prod else 0;
                planete.control <- t.camp );
        aux planete q in
    for i = 0 to taille - 1 do
      aux univers.(i) arrivees.(i)
    done
in

let jour_de_paye () =
    for i = 0 to taille - 1 do
        let p = univers.(i) in
        let nj = p.control in
        p.pop <- p.pop + vrai_revenu p.prod j.(nj).civi;
    done;
    for k = 0 to 2 do if ordi.(k) then ai (k + 1) done;
    ai 4;
    let i = ref 1 in
    while !i < taille && univers.(!i).control = univers.(0).control do
        incr i done;
    if !i = taille then begin
        let gagne = ref true in
        for ip = 1 to taille - 1 do
        for jp = 0 to ip - 1 do
            match tab_aretes.(ip).(jp) with
            |Rien -> ()
            |Direct ar | Indirect ar ->
                if ar.trafic <> [] then gagne := false
        done done;
        if !gagne then victoire := univers.(0).control
end
in

open_graph ((string_of_int taillex) ^ "x" ^ (string_of_int (tailley+deltay)));
begin
  let t0 = Sys.time () in
  while Sys.time () -. t0 < 0.5 do
    set_text_size 24 done
end;
auto_synchronize false;

let select = [|-1;-1;-1|] in

let continue = ref true in

let fond = begin
    set_color black;
    fill_rect 0 0 taillex tailley;
    set_color white;
    for i = 0 to iof (foi (taillex * tailley) *. nbet)
    do plot (ri taillex) (ri tailley) done;
    synchronize ();
 get_image 0 0 taillex tailley
end in

let chrono_paye = ref 0. in

let chrono_actu = ref 0. in

let pause () =
  let tp = Sys.time () in
  let (dx,dy) = text_size "MODE PAUSE : Appuyez sur P pour reprendre" in
  moveto ((taillex - dx) / 2) (tailley-dy-2);
  draw_string "MODE PAUSE : Appuyez sur P pour reprendre";
  synchronize ();
  let p = ref true in
  while !p do
    let statut = wait_next_event [Key_pressed] in
    continue := statut.key <> '\027';
    p := statut.key <> 'p' && statut.key <> '\027' done;
  chrono_paye := !chrono_paye +. Sys.time () -. tp;
  chrono_actu := !chrono_actu +. Sys.time () -. tp
in

let aff_univers s =
  clear_graph ();
  draw_image fond 0 0;
  aff_evenements ();
  set_color white;
  for i = 0 to taille - 1 do
    for j = i + 1 to taille - 1 do
      aff_lien tab_aretes.(i).(j)
    done
  done;
  Array.iter affiche_etoile univers;
  set_color white;
  for i = 0 to 2 do
    if s.(i) <> (-1)
    then (let p = univers.(s.(i)) in
          draw_circle p.x p.y p.r;
          draw_circle p.x p.y (p.r+1);
          draw_circle p.x p.y (p.r-1))
  done;
  synchronize ()
in

let traite_touche = function
  | 'l' -> vitesse := !vitesse *. 1.2
  | 'm' -> vitesse := !vitesse /. 1.2
  | 'p' -> pause ()
  | 'k' -> auto_synchronize false
  | '\027' -> continue := false
  | c -> match element c touches1 with
    | None -> if not ordi.(c2) then begin
        match element c touches2 with
        | None -> ()
        | Some i ->
          if select.(1) = (-1)
          then (if univers.(i).control = c2 + 1 then select.(1) <- i)
          else begin
            if univers.(select.(1)).control = (c2+1) &&
               mat_adj.(select.(1)).(i) then begin
              lance_vaisseau (c2+1) univers.(select.(1)).pop select.(1) i;
              univers.(select.(1)).pop <- 0 end;
            select.(1) <- -1
          end
      end
    | Some i -> if not ordi.(c1) then begin
        if select.(0) = (-1) then
          (if univers.(i).control = (c1+1) then select.(0) <- i)
        else begin
          if univers.(select.(0)).control = (c1+1) &&
             mat_adj.(select.(0)).(i) then begin
            lance_vaisseau (c1+1) univers.(select.(0)).pop select.(0) i;
            univers.(select.(0)).pop <- 0 end;
          select.(0) <- -1
        end
      end
in

let traite_clic pos =
  if dist_d pos !prb_mouse > 2 then begin
    let i = ref 0 in
    while !i < taille && not (dist_d (univers.(!i).x, univers.(!i).y) pos
                              < univers.(!i).r) do incr i done;
    if !i < taille then begin
      if select.(2) = (-1) then
        (if univers.(!i).control = (c3+1) then select.(2) <- !i)
      else if !i <> select.(2) then begin
        if univers.(select.(2)).control = c3+1 &&
           mat_adj.(select.(2)).(!i) then begin
          lance_vaisseau (c3+1) univers.(select.(2)).pop select.(2) !i;
          univers.(select.(2)).pop <- 0;
          prb_mouse := pos;
          select.(2) <- -1 end;
        select.(2) <- -1 end
    end
    else select.(2) <- -1
  end
in

(* main : *)

while key_pressed () do ignore (read_key ()) done;
chrono_paye := Sys.time (); chrono_actu := Sys.time ();
while !continue do
  if key_pressed () then traite_touche (read_key ());
  if button_down () && not ordi.(c3) then traite_clic (mouse_pos ());
  aff_univers select;
  if Sys.time () -. !chrono_paye > temps_paye *. !vitesse
  then (jour_de_paye ();
        chrono_paye := !chrono_paye +. temps_paye *. !vitesse);
  if Sys.time () -. !chrono_actu > !vitesse
  then (actu_univers ();
        chrono_actu := !chrono_actu +. !vitesse)
done;
close_graph ()


