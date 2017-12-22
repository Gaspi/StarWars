
open Kernel
open Cypher

val tab_persos : (string * 'a) array array
val presentation_niveau : int -> int -> unit
val presentation_perso : string -> unit

val creer_perso :?conquerant:bool ->
  ?furtif:bool ->
  ?diplomatie_vol:bool ->
  ?diplomatie_defense:bool ->
  ?diplomatie_attaque:bool ->
  ?defenseur:bool ->
  ?terrifiant:bool ->
  ?civilisation:culture ->
  attaque:'b -> defense:'c -> combat_spatial:'d -> vitesse:'e -> string -> 'f


val jouer :
  ?controlIA:bool array ->
  ?commandes_lettres:int ->
  ?commandes_chiffres:int ->
  ?commandes_souris:int ->
  ?force_j1:float ->
  ?force_j2:float ->
  ?force_j3:float ->
  ?force_ordi_j4:float ->
  j1:string -> j2:string -> j3:string -> string -> unit
