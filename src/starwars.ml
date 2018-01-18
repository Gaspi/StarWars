
open Kernel
open Front


(* La table des personnages est de dimension 3 * 8. *)

let _ =
  begin

    init ();
    
    (*
    print_perso_by_name "2x4";
    *)
    
    (*
    presentation_niveau 2 1;
    *)

    (**)
    creer_perso
      ~furtif:false
      ~diplomatie_vol:true
      ~diplomatie_defense:false
      ~diplomatie_attaque:true
      ~defenseur:false
      ~terrifiant:false
      ~civilisation:Adaptee
      ~conquerant:false
      ~attaque:1.01
      ~defense:1.01
      ~vitesse:1.01
      ~combat_spatial:1.3
      "gaspi";
    
    
    creer_perso
      ~furtif:false
      ~diplomatie_vol:true
      ~diplomatie_defense:false
      ~diplomatie_attaque:true
      ~defenseur:false
      ~terrifiant:false
      ~civilisation:Adaptee
      ~conquerant:false
      ~attaque:1.0
      ~defense:1.0
      ~vitesse:1.
      ~combat_spatial:1.3
      "pp";
    (**)

    
    (*
    let affiche_colonne_perso j =
      if j < 0 then failwith "Index < 0";
      if j >= Array.length tab_persos then failwith "Index > length";
      for i = 0 to Array.length tab_persos.(j) - 1
      do
        print_int i;
        print_string " : ";
        print_perso tab_persos.(j).(i);
        print_newline ();
        print_newline ()
      done in
    
    affiche_colonne_perso 1;
    affiche_colonne_perso 2;
    affiche_colonne_perso 3;
    *)
    
    
    jouer
      ~j1:"->gaspi"
      ~j2:"0x0"
      ~j3:"->pp"
      ~controlIA:[|false;true;false|]
      ~commandes_lettres:1
      ~commandes_chiffres:2
      ~commandes_souris:3
      
      ~force_j1:1.
      ~force_j2:1.
      ~force_j3:0.9
      ~force_ordi_j4:0.8
      "2x1";    (*le dernier parametre est le niveau choisi *)
    
    
    terminate ()

  end
         
