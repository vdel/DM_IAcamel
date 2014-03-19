(*Renvoit la liste des ltermes à ramasser*)
(*Classé par ordre de stupidité décroissante*)
val ramasse_stupide : Types.map_monde -> Types.id_terme list
val ramasse_moins_stupide : Types.map_monde -> Types.id_terme list
val ramasse_ratio : Types.map_monde -> Types.id_terme list
val ramasse_ratio_borne_inf : Types.map_monde -> Types.id_terme list
val ramasse_ratio_borne_inf_mieux : Types.map_monde -> Types.id_terme list

