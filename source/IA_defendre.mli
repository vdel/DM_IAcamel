(* Stratégie de défense contre les autres chameaux: si je ne suis pas menacé, je continue à sauver les lambda-termes, et si je suis menacé, je "fuis" les cases ou je pourrai être en danger de mort *)

type terrain_danger
type danger 
(* listes des chameaux méchants dans un rayon ici de 1 case, mais modifiable si on veut plus de méfiance *)
val mechants_du_coin : Types.map_monde -> Types.mechant list

(* donne une commande possible dans la limite de la suite donnée, si rien n'est possible, il reste sur place *)
val commande_sure : Types.map_monde -> Types.direction list -> Types.commande

(* donne la mise qu'il faut en fonction des éventuels chameaux aux positions données dans la liste *)
val mise_adaptee_mort : Types.map_monde -> Types.mechant list -> Types.mise
val mise_adaptee : Types.map_monde -> Types.mechant list -> Types.mise

(* donne une commande si on est en danger de mort, sinon donne 0, Prends [] *)
val danger_de_mort : Types.map_monde -> Types.commande -> int*Types.commande

(* donne une commande nouvelle, si je risque de mourir en faisant celle prévue *)
val danger_prochain_tour : Types.map_monde -> Types.commande -> int*Types.commande




