type position = int * int

(** un chemin est une suite d'étape**)
(** Soit c'est une position clé: la position ou l'on veut aller **)
(** Soit c'est juste une transition **)
type chemin = position list

type direction = Nord | Est | Ouest | Sud 

type id_terme = int (* identifiant unique *)
type id_chameau  = int (* idem, mais pour les chameaux *)

type terme = id_terme * position * int
(* la position ici est en fait la destination du terme
 * l'entier est la taille qu'il occupe en mémoire
 * *)
type mise = int (* pour agir, il faut miser *)

type commande =
  | Deplace of direction
  | Prends of id_terme list
  | Pose of id_terme list

(* Actions possibles qu'un chameau a pu faire dans un tour *)
type action =
  | Apparait of position (* Seulement pour l'initialisation, n'arrivera plus
                            jamais dans le jeu *)
  | Abouge of direction
  | Apris of id_terme list
  | Apose of id_terme list


type terrain = Sable | Palmier | Omega | Base
type carte = terrain array array
(* matrice du terrain 
 * le terrain aux coordonnées (x,y) est carte.(y-1).(x-1)
 * Pour éviter ce genre de problème, il est conseillé d'utiliser la fonction
 * [case_carte] du module [Jouer]
 * *)

(* Structure de données perso *)
type map_monde =
{
  desert_hostile : carte;   (* la carte du monde*)
  mutable bases : position list;    (* la liste des positions des bases *)
  moi : chamoi;                     (* notre chameau *)
  pas_moi : (id_chameau,mechant) Hashtbl.t;   (* les autres chameaux agressifs (ou pas) *)
  victimes : (id_terme,lterme) Hashtbl.t;  (* les pauvres petits lambda-termes qu'on a pas encore sauvé (par terre ou sur un chameau) *)
  mutable nbr_tours : int;
  mutable nbr_combats: int;
  params : commande_params;
  termes_bannis : (id_terme,unit) Hashtbl.t;
}
and chamoi = 
{
  id_moi : id_chameau;
  capacite : int;         (*la capacité totale du cerveau*)
  mutable cerveau : int;  (*la capacité libre du cerveau mutant de notre chameau*)
  mutable bosse : int; (*la bosse mutante chargée d'eau de notre chameau pouvant contenir jusqu'à 1000000000 litres*)
  mutable sac_a_bosse : id_terme list; (*le sac à bosses mutant: pour mettre plein d'innocents lambda-termes à sauver de transe paranoiaque*)
  mutable a_mes_pieds : id_terme list; (* liste de lambda-termes paranoiaques aux pieds de notre gentil chameau *)
  mutable suis_la: position;
  strategie : params_IA;  
}
and mechant = 
{
  id_mechant : id_chameau;
  mutable est_la: position;
  mutable otages : id_terme list;  (* les lambda-termes portés par le méchant*)
  comportement : mechant_params;
}
and lterme=
{
  mutable taille: int option;
  mutable vers: position option;    (*là où il doit être sauvé*)
  mutable la: position;  (*là ou il est*)
  mutable sur: status;
}
and params_IA =
{
  (*MOD*)
  (*nombre max de tours qu'on peut jouer: pour savoir combien on peut dépenser en eau pour l'attaque et la défense*)
  max_tours : int;
  ajuste_win : float;
  ajuste_loose : float;
  mutable ajuste_mise : (int*action*(mechant list)*bool) option;   (*(mise d'eau jouée, action demandée, rival,duel à mort?) : pour ajuster les mises lors des duels *)
  (*Les différents aspect de la strategie pour sauver les lterms*)
  mutable trouve_chemin : map_monde -> position -> position -> chemin*int;
  mutable ramasse_lterm : map_monde -> id_terme list;
  mutable calcule_livraison : map_monde -> chemin*int;
  mutable ameliore_chemin : map_monde -> chemin -> int -> chemin;
  mutable calcule_chargement: map_monde -> chemin*int;
  mutable distance: map_monde -> position -> position -> int;

  (*Des variables utiles*)
  (*Pour SAUVER*)  
  mutable chemin_a_suivre : chemin;       (*Le chemin prévu*)
  pourcent_ameliore_chemin : int; (*largeur du couloir utilisé dans améliore_chemin  *)  
  pourcent_taille_min : int;      (*utilisé dans ramasse_ratio: taille min des lterms*)
  mutable somme_ratio_bases : float; (*la somme des tailles de tous les ltermes trouvés sur les bases*)
  mutable nbr_bases         : int; (*le nbr de bases rencontrées jusqu'ici*)

  (*Pour Attaquer*)
  pourcent_gain_min : int;   (*on attaque si on y gagne vraiment *)   
  max_dist_omega : int;      (*distance max d'un omega dans lequel on essaye de pousser quelqu'un*)
  mutable cible : id_chameau option; (*la cible que l'on traque*)
  (*Pour limiter le nombre d'attaque: évite que notre chammeau reste bloqué en mode attaque*)
  max_attaques_successives : int;
  mutable nbr_attaques_successives : int;
  mutable attaque_bloquee : bool;

  (*ANTI-BLOCAGE: non opérationnel*)
  (*Les test ont montré qu'on pouvait arriver à des situations de blocage
    pour éviter ca on fait un anti-blocage: évite N S N S ou O E O E*)
  mutable last_direction : direction option;  (*dernière direction empruntée*)
  mutable nbr_aller_retour : int;      (*nombre de directions contradictoires empruntées*)
  max_aller_retour : int;      (*quand le max est atteind on essait de contourner*)
 
  (*pour la mise de l'eau*)
  pourcent_mise_opt_initiale : int;  (*mise initial en pourcentage de l'eau réservée au combat*)
  (*Concerne le calcul du chemin*)
  mutable last_moi_pos : position; (*dernière position pour laquelle on a calculé pred_dij_depuis_moi *)
  (*Pour le calcul des chemins d'origine notre chameau: on se souvient de ce qu'on a déjà calculé:
    utile surtout pour Dijkstra, moins pour A-star*)
  mutable pred_dij_depuis_moi : (position,position) Hashtbl.t;     (*le couple (pos,pos) correspond à (position x, predecesseur de x) *)
  mutable waiting_depuis_moi : (position,int) Hashtbl.t;           (*l'ensemble des cases en attente d'être calculées*)
  mutable restant_depuis_moi : (position*position*int) Tas_fibo.t; (*idem mais en version tas de fibonacci*)
  (*Pour le calcul des chemins n'ayant pas notre chameau pour origine*)
  mutable pred_dij_autre : (position,position) Hashtbl.t;      (*idem que ci-dessus, mais pour un trajet n'ayant le chameau...*)
  mutable waiting_autre : (position,int) Hashtbl.t;            (*...pour origine*)
}
and mechant_params=
{
  (*MOD*)
  mutable last_pos : position;   (* avant-dernière position *)
  mutable mise_optimale : int;  (*ce qu'on doit miser contre ce chameau en duel normal: évolue si on perd ou si on gagne*)
  mutable mise_ultime : int;    (*ce qu'on doit miser contre ce chameau en duel à mort: évolue si on perd ou si on gagne*)
}
and commande_params=
{
  mutable outchannel : out_channel option;
}
and status = SOL | CHAMEAU of id_chameau
;; 
