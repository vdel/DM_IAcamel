val output_log : Types.map_monde -> string -> unit
(** écrit dans le fichier log **)

val output_log_action : Types.map_monde -> Types.commande -> unit
(** écrit dans le fichier log **)

val joue : Types.map_monde -> int -> Types.commande -> Types.mechant list -> bool -> int*Types.commande
(**joue en se souvenant de l'action executée pour voir si on perd ou on gagne le duel**)

val get_direction: int*int -> Types.direction
(**transforme un vecteur en une direction**)

val get_vector : Types.direction -> int*int
(**transforme une direction en un vecteur**)

val string_of_direction : Types.direction -> string
(**transforme une direction en chaîne de caractère**)

val max_mise : Types.map_monde -> int
(**calcul la mise max que l'on peut miser pour pousser un méchant **)

val ajuste_mise_win : Types.map_monde -> Types.mechant -> int -> bool -> unit
(**ajuste la mise optimale en cas de victoire**)

val ajuste_mise_loose : Types.map_monde -> Types.mechant -> int -> bool -> unit
(**ajuste la mise optimale en cas de défaite**)

val depl_possib : Types.map_monde -> Types.direction -> bool
(**renvoit vrai ssi si on peut se déplacer vers dir, ie s'il n'y a pas de palmier ou d'omega**)



(**************************************************************************)
(**Fonction pour l'attaque et la défense, en fait, surtout pour l'attaque**)
(**************************************************************************)
type probabilite =    (*la probabilite qu'un méchant soit sur une case au prochain tour*)
  | MAINTENANT of int (*le méchant est sur la case actuellement*)
  | TOUR_DAPRES of int(*le méchant n'est pas sur la case actuellement*)
  | ZERO

type securite = (*sécurité d'une case du désert: dans l'ordre de priorité*)
  | DANGEREUX   (*c'est case est dangereuse: si on y va, on risque d'être poussé par un autre dans un omega*)
  | POUSSE of Types.direction*probabilite   (*case strategique: on peut pousser un méchant dans l'omega*)
  (*POUSSE([direction vers laquelle on pousse],[proba que le méchant soit sur la case])*)
(*POUSSE([direction dans laquelle il faut pousser])*)
  | RIEN

type ramasse =    (*le gain potentiel que l'on peut attendre en lterm allant sur la case *)
  | BASE          (*c'est une base*)
  | SABLE of int  (*somme des tailles des ltermes à rammasser*)

type case = (*une case du damier*)
  |INFRANCHISSABLE (*soit elle est infranchissable*)
  |FRANCHISSABLE of securite*probabilite*ramasse  (*soit elle est plus interessane et franchissable*)
 (*FRANCHISSABLE([securite de la case],[proba qu'un méchant aille sur la case],[ce qu'on peut rammaser])*)

(*respecte les priorités: d'abord DANGEREUX ensuite POUSSE ensuite RIEN*)
val set_securite : case array array -> int -> int -> securite -> unit
(*respecte les priorités: d'abord MAINTENANT ensuite TOUR_DAPRES ensuite ZERO*)
val set_probabilite : case array array -> int -> int -> probabilite -> unit
(*renvoit proba qu'un méchant aille sur une case*)
val get_probabilite : case array array -> int -> int -> probabilite option
(*renvoit 1 si s1>s2 0 si s1=s2 et -1 si s1<s2*)
val cmp_securite : case array array -> securite -> securite -> bool -> int
(*renvoit 1 si p1>p2 0 si p1=p2 et -1 si p1<p2*)
val cmp_probabilite : probabilite option -> probabilite option -> int
(*renvoit 1 si c1>c2 0 si c1=c2 et -1 si c1<c2*)
(*on préfère dans l'ordre croissant:
INFRANCHISSABLE
FRANCHISSABLE(DANGEREUX,_,_)
FRANCHISSABLE(RIEN,_,SABLE _)
FRANCHISSABLE(RIEN,_,BASE)
FRANCHISSABLE(POUSSE,MAINTENANT _,_)
FRANCHISSABLE(POUSSE,TOUR_DAPRES _,_)
*)
val cmp_case : case array array -> int -> int -> int -> int -> int
(* renvoit true ssi un méchant est en (i,j) *)
val mechant_on : case array array -> int -> int -> bool

val calcule_alentours: Types.map_monde -> Types.mechant list -> case array array *int * int
