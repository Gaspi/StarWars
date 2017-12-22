



let rec aff_list = function
	|[] -> ()
	|t::q -> (print_int t;print_string "  "; aff_list q);;
(* Prints the given list.  *)



