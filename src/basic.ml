

let adresse = Sys.getcwd ()

let separator =
  if Sys.unix then "/"
  else if Sys.win32 then "\\"
  else "\\"

let build_path names = String.concat separator names

let get_path names = build_path (adresse::names)

let get_include name = get_path ["include"; name]

let get_persos name = get_path ["persos"; name]

let get_level a b =
  get_path ["include"; "niveaux"; (string_of_int a) ^ "x" ^ (string_of_int b) ^ ".bmp"]


let rec aff_list = function
  | [] -> ()
  | hd :: tl ->
    begin
      print_int hd;
      print_string "  ";
      aff_list tl
    end
(* Prints the given list.  *)


let debug fmt = Format.printf fmt
