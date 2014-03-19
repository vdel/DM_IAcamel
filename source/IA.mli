val joue : Types.map_monde -> int*Types.commande
(** renvoit une commande à jouer **)

val set_strategie : int*int -> Types.params_IA
(** initialise les paramètres de l'IA:         **)
(** prend en argumet un couple (w,h) où w et h **)
(** sont respectivement la largeur et la       **)
(** de la carte                                **)
