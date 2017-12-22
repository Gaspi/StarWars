
open Graphics

let rec int_of_bytes = function
  | []   -> 0
  | t::q -> t + 256 * int_of_bytes q;;
(*convertit une liste de bytes en sa valeur entière correspondante*)

let rec get_bytes fichier = function
  | 0 -> 0
  | n -> let a = input_byte fichier in
    a + 256 * (get_bytes fichier (n-1));;
(* lit n bytes d'un fichier et renvoie la valeur entière correspondante *)

let get_mat_bmp nom_fichier =
  let fic = open_in
      (if String.sub nom_fichier (String.length nom_fichier - 4) 4 = ".bmp" then
      nom_fichier else nom_fichier ^ ".bmp") in
  (* ouverture du fichier, rajoute eventuellement le .bmp manquant *)
  let getb = get_bytes fic in
  (* getb -> lit dans le fichier ouvert n bytes *)
  if getb 2 <> 19778 then failwith "Mauvais format";
  (* les deux premiers bytes doivent être 66 et 77 (format bmp) *)
  ignore (getb 8);
  let start = getb 4 in
  ignore (getb 4);
  let largeur = getb 4 in
  let hauteur = getb 4 in
  if getb 2 <> 1 then failwith "nb de plan"; (* doit etre égal à 1 *)
  if getb 2 <> 24 then failwith "mauvais format couleur (24bits)";
  (* nb de bits pour coder une couleur *)
  if getb 4 <> 0 then failwith "compression"; (*mode de compression *)
  seek_in fic start; (* on se déplace au début de l'image *)
  let resultat = Array.make_matrix hauteur largeur (rgb 0 0 0) in
  let decalage = (8 - ((3 * largeur) mod 4)) mod 4 in
  (* calcul du décalage, nb de bts ajouté à chaque ligne pour que sa
     longueur soit un multiple de 4*)
  for i = hauteur - 1 downto 0 do
    for j = 0 to largeur - 1 do
   try
     resultat.(i).(j) <- rgb (input_byte fic)
      (input_byte fic) (input_byte fic)
   with _ -> close_in fic done;
 ignore (getb decalage)
    done;
    (*lecture de l'image -> souvent ici qu'il y a des bugs *)
    close_in fic;
    (* fermeture du fichier *)
    (resultat, (hauteur, largeur));;

(*
A partir de l'adresse complète d'un fichier, renvoie la matrice des couleurs de l'image
bmp et ses dimensions.
*)

let get_img_bmp fic =
  let (a,b) = get_mat_bmp fic in
  (make_image a, b)
(* lit le fichier et renvoie l'image correspondante *)

let set_color_transp couleur imag =
  let mat = dump_image imag in
  for i = 0 to Array.length mat - 1 do
    for j = 0 to Array.length mat.(0) - 1 do
   if mat.(i).(j) = couleur then
     mat.(i).(j) <- transp done done;
  make_image mat
(* transforme tout les occurences de "couleur" dans la matrice de couleurs "imag"
   par la couleur transp (transparence) *)
    
let get_list_fic fichier =
  let fic = open_in fichier in
  let resultat = ref [] in
  let keep = ref true in
  while !keep do
    try resultat := (input_byte fic)::!resultat
    with End_of_file -> keep := false
  done;
  close_in fic;
  !resultat
(* Très utile pour disséquer le fichier paint byte par byte en se niquant les yeux
 * pour essayer de comprendre pourquoi les fonctions précédentes refusent d'en faire
 * une image. *)

