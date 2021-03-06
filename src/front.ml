
open Basic
open Png
open Kernel
open Cypher
open Graphics

let neutre_base : joueur =
  {
    vit = 1.;
    def = 1.1;
    atk = 1.;
    vol = 1.;
    terrifiant = false;
    defenseur  = false;
    furtif     = false;
    dipl_vol   = false;
    dipl_atk   = false;
    dipl_def   = false;
    conquerant = false;
    civi=Neanderthal
  }

let ordi_base : joueur =
  {
    vit = 1.1;
    def = 1.1;
    atk = 1.;
    vol = 1.;
    terrifiant = false;
    defenseur  = true;
    furtif     = false;
    dipl_vol   = false;
    dipl_atk   = false;
    dipl_def   = false;
    conquerant = false;
    civi=Adaptee
  }

let lit_perso tab =
  let gf () = lit_float tab in
  let gb () = lit_bool tab in
  let nom = lit_string tab in
  let (conq,dd,da,dv,fu,def,ter,vo,a,d,vi) =
    (gb(),gb(),gb(),gb(),gb(),gb(),gb(),
     gf(),gf(),gf(),gf()) in
  let civ = decode_civi (lit_int tab) in
  let perso =
    {
      vit = vi;
      def = d;
      atk = a;
      vol = vo;
      terrifiant = ter;
      defenseur  = def;
      furtif     = fu;
      dipl_vol   = dv;
      dipl_atk   = da;
      dipl_def   = dd;
      conquerant = conq;
      civi = civ
    } in
  (nom, perso)


let charge_perso nom_fic =
  let decoded = decoder_charger nom_fic in
  let cut = coupe_ligne decoded in
  let nb_persos = Array.length cut / 13 in
  Basic.debug "Test1: %i\n" (Array.length cut);
  Basic.debug "Test2: %i\n" nb_persos;
  Basic.debug "Test 3\n%s" (String.concat "\n" (Array.to_list cut));
  Array.init nb_persos (fun i -> lit_perso (cut, ref (-1)) )

let met_perso (nom,j) l =
  let ps s = l := s::!l in
  let pf f = (Basic.debug "sof: %s\n" (string_of_float f); ps (string_of_float f)) in
  let pb b = ps (string_of_bool  b) in
  ps nom;
  pf j.vit;
  pf j.def;
  pf j.atk;
  pf j.vol;
  pb j.terrifiant;
  pb j.defenseur;
  pb j.furtif;
  pb j.dipl_vol;
  pb j.dipl_atk;
  pb j.dipl_def;
  pb j.conquerant;
  ps (string_of_int (code_civi j.civi))

let perso_save nom_fic perso =
  let l = ref [] in
  met_perso perso l;
  Basic.debug "%s" (String.concat "\r\n" (List.rev !l));
  coder_sauver nom_fic (String.concat "\r\n" (List.rev !l))

let maod taille tab =
  let res = Array.make_matrix taille taille false in
  for i = 0 to Array.length tab - 1 do
    let (x,y) = tab.(i) in
    res.(x-1).(y-1) <- true;
    res.(y-1).(x-1) <- true
  done;
  res


let lit_niveau (tab,i) =
  let gi () = lit_int (tab,i) in
  let nom = lit_string (tab,i) in
  let nb_et = gi () in
  let tab_et = Array.make nb_et (0,0,0,0,0,0) in
  for j = 0 to nb_et - 1 do
    let (f,e,d,c,b,a) = (gi(),gi(),gi(),gi(),gi(),gi()) in
    tab_et.(j) <- (a,b,c,d,e,f);
  done;
  let nb_db = gi() in
  let tab_db = Array.make nb_db (0,0) in
  for j = 0 to nb_db - 1 do
    let (b,a) = (gi(),gi()) in
    tab_db.(j) <- (a,b);
  done;
  let neutre = lit_perso (tab,i) in
  let j4 = lit_perso (tab,i) in
  let tx = gi() in
  let ty = gi() in
  (nom, (tab_et, maod nb_et tab_db, snd neutre,snd j4, tx,ty))
  
let charge_niveau nom_fic =
  let decoded = decoder_charger nom_fic in
  let tab = (coupe_ligne decoded, ref (-1)) in
  let res = ref [] in
  let continue = ref true in
  while !continue do
    try (res := (lit_niveau tab)::!res)
    with _ -> continue := false
  done;
  Array.of_list !res



let niveaux_4j = charge_niveau (get_include "lvl4j.txt")
let niveaux_3j = charge_niveau (get_include "lvl3j.txt")
let niveaux_2j = charge_niveau (get_include "lvl2j.txt")
let tab_niveaux = [| niveaux_2j; niveaux_3j; niveaux_4j |]

let persos_forts   = charge_perso (get_include "forts.txt"  )
let persos_faibles = charge_perso (get_include "faibles.txt")
let persos_moyens  = charge_perso (get_include "moyens.txt" )
let tab_persos = [| persos_faibles; persos_moyens; persos_forts |]


let pprint_float f = string_of_int (int_of_float (100. *. f))

let print_perso (a,b) =
  let aux b s = if b then " - " ^ s ^ "\n" else "" in
  let desc =
    a ^ "\n\n" ^
    "Vitesse  :  "        ^ (pprint_float b.vit) ^ "\n" ^
    "Attaque  :  "        ^ (pprint_float b.atk) ^ "\n" ^
    "D�fense  :  "        ^ (pprint_float b.def) ^ "\n" ^
    "Combat spatial  :  " ^ (pprint_float b.vol) ^ "\n" ^
    "Civilisation  :  "   ^ (print_civi b.civi) ^ "\n" ^
    "<<< Bonus >>>\n" ^
    (aux b.terrifiant "Terrifiant") ^
    (aux b.defenseur  "D�fenseur") ^
    (aux b.furtif     "Furtif") ^
    (aux b.dipl_vol   "Diplomate spatial") ^
    (aux b.dipl_atk   "Diplomate attaquant") ^
    (aux b.dipl_def   "Diplomate defenseur") ^
    (aux b.conquerant "Conqu�rant") in
  print_string desc

let presentation_niveau a b =
  let (x,_) = tab_niveaux.(a).(b) in
  print_string x;
  let path = get_level a b in
  Basic.debug "path : %s" path;
  let imgbmp = get_img_bmp path in
  let (img, (ty,tx)) = imgbmp in
  resize_window tx ty;
  while key_pressed () do ignore (read_key ()) done;
  while not (key_pressed ()) do
    draw_image img 0 0;
    synchronize ();
  done

let dos s =
  let i = ref 0 in
  while s.[!i] <> 'x' do incr i done;
  int_of_string (String.sub s 0 !i) ,
  int_of_string (String.sub s (!i+1) (String.length s - !i-1))

let perso_of_string mot =
  debug "Mot: %s\n" mot;
  if mot.[0] == '-'
  then
    if mot.[1] <> '>' then failwith "commencer par '->'"
    else begin
      let nom = String.sub mot 2 (String.length mot - 2) in
      let perso = charge_perso (get_path ["persos"; nom ^ ".txt"]) in
      if Array.length perso == 0
      then failwith ("Wrong character string: " ^ mot);
      perso.(0)
    end
  else
    let (a,b) = dos mot in
    try tab_persos.(a).(b)
    with Invalid_argument _ -> failwith ("Wrong character string: " ^ mot)

let print_perso_by_name s = print_perso (perso_of_string s)

let creer_perso
    ?(conquerant=false)
    ?(furtif=false)
    ?(diplomatie_vol=false)
    ?(diplomatie_defense=false)
    ?(diplomatie_attaque =false)
    ?(defenseur=false)
    ?(terrifiant=false)
    ?(civilisation=Adaptee) ~attaque ~defense ~combat_spatial ~vitesse nom =
  let perso =
    {
      atk=attaque;
      def=defense;
      vol=combat_spatial;
      vit=vitesse;
      conquerant=conquerant;
      furtif=furtif;
      dipl_vol=diplomatie_vol;
      dipl_def=diplomatie_defense;
      dipl_atk=diplomatie_attaque;
      defenseur=defenseur;
      terrifiant=terrifiant;
      civi=civilisation
    } in
  let a = equilibrer 1. perso in
  Basic.debug "New character created (%s) :\n\n" nom;
  print_perso (nom, a);
  perso_save (get_persos (nom ^ ".txt")) (nom, a)


let jouer
    ?(controlIA=[|false;false;false|])
    ?(commandes_lettres=1)
    ?(commandes_chiffres=2)
    ?(commandes_souris=3)
    ?(force_j1=0.)
    ?(force_j2=0.)
    ?(force_j3=0.)
    ?(force_ordi_j4=0.) ~j1 ~j2 ~j3 niveau =
  let niv1, niv2 = dos niveau in
  let niveau = tab_niveaux.(niv1).(niv2) in
  let persoj1 = (snd (perso_of_string j1)) in
  let j1 = equilibrer force_j1 persoj1 in
  let j2 = equilibrer force_j2 (snd (perso_of_string j2)) in
  let j3 = equilibrer force_j3 (snd (perso_of_string j3)) in
  jeu
    ~temps_paye:16.
    ~niveau:(snd niveau)
    ~j1:(j1)
    ~j2:(j2)
    ~j3:(j3)
    ~force_ordi:force_ordi_j4
    ~control_lettres:commandes_lettres
    ~control_chiffres:commandes_chiffres
    ~control_souris:commandes_souris
    ~ordi:controlIA ()
