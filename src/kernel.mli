
val adresse : string


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

val equilibrer : float -> joueur -> joueur

val jeu :
  ?temps_paye:float ->
  ?nbet:float ->
  ?vitess:float ->
  ?control_lettres:int ->
  ?touches_j1:char array ->
  ?touches_j2:char array ->
  ?control_chiffres:int ->
  ?control_souris:int ->
  ?ordi:bool array ->
  ?force_ordi:float ->
  niveau:niveau -> j1:joueur -> j2:joueur -> j3:joueur -> unit -> unit
