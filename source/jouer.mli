open Types

exception En_dehors
(** exception levée par [case_carte] si la position est invalide *)

val case_carte : carte -> position -> terrain
(** [case_carte c (x,y)] renvoie le terrain de [c] aux coordonnées (x,y)
 * @raise En_dehors si [x] ou [y] sont en dehors des limites de la carte
 * *)
val change_case_carte : carte -> position -> terrain -> unit

val taille_carte : carte -> position
(** renvoie les coordonnées maximales de la carte (X,Y) 
 * toute position est comprise entre (1,1) et (X,Y) *)


val init : string -> int -> carte * (id_chameau * int * int) * (id_chameau * position) list
(** petite fonction d'initialisation
 * [init hote port] se connecte sur le port [port] de l'hôte [hote] et renvoie
 * un tuple contenant :
   - la carte
   - la configuration du joueur: id du chameau * capacité mémoire * quantité d'eau
   - la liste de tous les chameaux avec leur position initiale
 *)


val joue_un_tour : mise -> commande -> (id_chameau * action list) list * terme list
(** Fonction pour jouer, à appeler à chaque tour de jeu :
 * la fonction joue un coup pour votre chameau, attend que tout le monde ait
 * joué et renvoie :
    - la liste des actions effectuées par chaque chameau
    - la liste des λ-termes au pied de votre chameau
 *
 *)


val stop : unit -> unit
(** Si vous en avez marre de jouer, c'est la fonction à appeler :)
 *)


(** Quelques fonctions d'affichage juste pour y voir quelque chose au début
 * pendant la phase de familiarisation des étudiants
 *)
val afficher_les_ordres: (id_chameau * action list) list -> unit

val afficher_les_termes: terme list -> unit

(* une fonction de dessin basique, la correspondance id_chameau -> caractère est
 * crée à chaque appel de la fonction *)
val dess : carte -> (id_chameau * position) list -> unit
