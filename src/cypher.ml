
(* programme de sauvegarde cod�e *)

let coi n = char_of_int ((27 + ((n + 255) mod 255)) mod 256)

let ioc c = (int_of_char c + 229) mod 256
(* bijection entre les entiers entre 0 et 254 et les char sauvegardables
    (diff�rent de '\026') *)


let int_list_of_string s =
    let res = ref [] in
    for i = 0 to String.length s -1 do
        res := (int_of_char s.[i])::!res
    done; !res

let string_of_array f arr =
  String.init (Array.length arr) (fun i -> f arr.(i))

let string_of_list f lst =
  string_of_array f (Array.of_list lst)

let string_of_int_list = string_of_list char_of_int

let string_of_char_list = string_of_list (fun x -> x)

let rec is_not_divisor n d =
  d * d > n || (n mod d <> 0 && is_not_divisor n (d+1))

let is_prime n = n > 1 && is_not_divisor n 2

let trouve_inverse ?(modulo=256) n =
  let i = ref 0 in
  while !i < modulo && !i * n mod modulo <> 1 do
    incr i done;
  if !i = modulo then failwith "element non inversible";
  !i


let coder ?(code=97) ?(decalage=147) ?(graine=(0,0,0)) message =
    let t = String.length message in
    let cd = if is_prime code then code else 97 in
    let dec = decalage mod 255 in
    let (gr0, gr1, gr2) = if graine = (0,0,0) then
            (Random.int 255, Random.int 255, Random.int 255)
        else graine in
    let res = String.make (2*t+3) ' ' in
    res.[0] <- coi gr0; res.[1] <- coi gr1; res.[2] <- coi gr2;
    let a,b,c = ref gr0, ref gr1, ref gr2 in
    for i = 0 to String.length message - 1 do
        res.[2*i+3] <- coi ((int_of_char message.[i] + dec) * cd);
        let aux = (!a + !b + !c) mod 255 in
        res.[2*i+4] <- coi aux;
        a := !b; b := !c; c := aux;
    done;
    res

let decoder ?(code=97) ?(decalage=147) message =
    let cd = trouve_inverse ~modulo:255 (code mod 255) in
    let dec = decalage mod 255 in
    let t = String.length message in
    if t < 3 then failwith "message trop court";
    let res = String.make ((t - 3) / 2) ' ' in
    let (a,b,c) = (ref (ioc message.[0]), ref (ioc message.[1]),
        ref (ioc message.[2])) in
    let i = ref 3 in
    while !i < t do
        res.[(!i-3)/2] <- char_of_int
            (((((ioc message.[!i]) * cd) mod 255)
                - dec + 255) mod 255);
        incr i;
        let aux = (!a + !b + !c) mod 255 in
        a := !b; b := !c; c := aux;
        if ioc message.[!i] <> !c then
            failwith "code erron�";
        incr i
    done;
    res

let coupe_ligne s =
    let res = ref [] in
    let courant = ref [] in
    let i = ref 0 in
    while !i < String.length s do
        if s.[!i] = '\n' then (
            res := (string_of_char_list (List.rev!courant))::!res;
            courant := [] )
        else
            courant := s.[!i]::!courant;
        incr i
    done;
    res := (string_of_char_list (List.rev !courant))::!res;
    Array.of_list (List.rev !res)


let enregistrer nom_fic s =
    let fic = open_out nom_fic in
    output_string fic s;
    close_out fic

let charger nom_fic =
    let fic = open_in nom_fic in
    let res = ref [] in
    let continue = ref true in
    while !continue do
        try res := (input_line fic)::!res
        with End_of_file -> continue := false
    done;
    close_in fic;
    String.concat "\n" (List.rev !res)

let coder_sauver ?(code=97) ?(decalage=147) ?(graine=(0,0,0)) nom_fic message =
    enregistrer nom_fic (coder ~code:code ~decalage:decalage ~graine:graine message)

let decoder_charger ?(code=97) ?(decalage=147) nom_fic =
    decoder ~code:code ~decalage:decalage (charger nom_fic)

let lit_string (tab, i) = incr i; tab.(!i)
let lit_float x = float_of_string (lit_string x)
let lit_int   x = int_of_string (lit_string x)
let lit_bool  x = bool_of_string (lit_string x)