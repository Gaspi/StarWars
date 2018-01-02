

let adresse = Sys.getcwd ()

let separator =
  if Sys.unix then "/"
  else if Sys.win32 then "\\"
  else "\\"

let get_path names =
  List.fold_left (fun s p -> s ^ separator ^ p) adresse names

let get_full_path name = get_path ["include"; name]


let rec aff_list = function
  | [] -> ()
  | hd :: tl ->
    begin
      print_int hd;
      print_string "  ";
      aff_list tl
    end
(* Prints the given list.  *)


let debug s = Format.printf "Debug: %s@." s
