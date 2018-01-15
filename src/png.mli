
open Graphics

(*
Module permettant de traiter (la plupart) des fichiers BMP au format 24 bits non compressés.
Pour convertir une image sous ce format : paint -> enregistrer sous puis dans format
chosir bmp 24bits.
Attention, certaines images ne pourront pas être lues par ces fonctions même après conversion
il convient alors de les bidouiller jusuqu'à 

Pour comprendre comment est foutu le format bmp :

http://www.commentcamarche.net/contents/video/format-bmp.php3
+ google.

*)


val get_img_bmp : string -> image * (int * int)
(* Lit le fichier et renvoie l'image correspondante *)

val set_color_transp : color -> image -> image
(* Transforme tout les occurences de "couleur" dans la matrice de couleurs
   par la couleur transp (transparence) *)

