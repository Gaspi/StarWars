
open Front

(* La table des personnages est de dimension 3 * 8. *)

let _ =
  begin
    presentation_perso "->napoléon";
    presentation_perso "2x4";
    
    presentation_niveau 2 1;
    
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
      "2x1";
    (*le dernier parametre est le niveau choisi *)
    
    creer_perso
      ~furtif:false
      ~diplomatie_vol:true
      ~diplomatie_defense:false
      ~diplomatie_attaque:true
      ~defenseur:false
      ~terrifiant:false
      ~civilisation:Kernel.Adaptee
      ~conquerant:false
      ~attaque:1.0
      ~defense:1.0
      ~vitesse:1.
      ~combat_spatial:1.3
      "gaspi";
    
    
    (* rajouter ici toute fonction pouvant aider
       les blaireaux *)
    let affiche_colonne_perso j =
      for i = 0 to 7 do
        print_int i; print_string " : ";
        presentation_perso tab_persos.(j).(i);
        print_newline (); print_newline ()
      done in
    
    affiche_colonne_perso 1
  end
         
