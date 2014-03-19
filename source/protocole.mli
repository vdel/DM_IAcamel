open Types

class client : object
  method connect: string -> int -> unit (* connection au serveur *)
  method send: string -> unit           (* envoyer un chaine *)
  method disconnect: unit -> unit       (* on se casse ! *)

  method lit_carte: unit -> carte
  method lit_config_initiale: unit -> id_chameau*int*int

  method lit_actions : unit -> (id_chameau * action list) list
  method lit_termes: unit -> terme list
  method envoie_commande: mise -> commande -> unit
end

(** Modification du terminal pour pouvoir lire les caractères un par un 
 au clavier en entrée sans appuyer sur "Enter" à chaque fois. *)
val terminal_char_by_char: unit -> in_channel
